{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.BuildPlan
  ( BuildPlan (BuildPlan),
    BuildPlanFile,
    AllBootstrappable,
    toBuildPlanFiles,
    T,
    toBuildPlanFile,
    toReasonTree,
    confirmBuildPlan,
    bootstrap,
  )
where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason, bootstrapWithMaybe))
import Bootstrap.Data.HList (HList (HCons, HNil))
import Bootstrap.Error
  ( CanDieOnError (dieOnError),
    InProgressDuration (Quick),
    runWithProgressMsg,
  )
import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.Terminal (promptYesNo, withAttribute, withAttributes)
import Control.Exception (IOException)
import Control.Monad.Catch (catchAll, try)
import Data.Char (toLower)
import Data.Tree (Tree (Node, rootLabel, subForest))
import Language.Haskell.TH (Quote)
import Relude.Extra.Group (groupBy)
import Relude.Extra.Map (toPairs)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (joinPath, splitDirectories, takeDirectory)
import System.Terminal
  ( MonadColorPrinter (foreground, green, red, yellow),
    MonadFormattingPrinter (bold),
    MonadPrinter (putLn, putText),
    putTextLn,
  )

newtype BuildPlan = BuildPlan {buildPlanFiles :: [(FilePath, BuildPlanFile)]}

data BuildPlanFile = BuildPlanFile
  { reasonPhrase :: Text,
    contents :: Text,
    overwriteStatus :: OverwriteStatus
  }

data OverwriteStatus
  = WillOverwrite
  | NothingToOverwrite
  | NoContentChange
  deriving stock (Eq, Ord)

getOverwriteStatus :: MonadIO m => FilePath -> Text -> m OverwriteStatus
getOverwriteStatus path newContents = liftIO do
  doesFileExist path >>= \case
    True ->
      readFileBS path `catchAll` const (pure "") >>= \oldContents ->
        pure $
          if oldContents == encodeUtf8 newContents
            then NoContentChange
            else WillOverwrite
    False -> pure NothingToOverwrite

-- | Converts a `Bootstrappable` file into a `BuildPlanFile`.
--
-- Only returns `Nothing` if the file passed in is `Nothing`.
toBuildPlanFile :: (Bootstrappable a, CanDieOnError m, MonadIO m, Quote m) => a -> m (Maybe (FilePath, BuildPlanFile))
toBuildPlanFile maybeA = case bootstrapWithMaybe maybeA of
  Nothing -> pure Nothing
  Just a -> do
    let path = bootstrapName a
    newContents <-
      dieOnError ((("Could not bootstrap " <> toText path <> ": ") <>) . toText)
        . ExceptT
        $ bootstrapContent a
    overwriteStatus <- getOverwriteStatus path newContents
    pure $ Just (path, BuildPlanFile (bootstrapReason a) newContents overwriteStatus)

-- | Constrains every type in the list with `Bootstrappable`
type family AllBootstrappable (xs :: [Type]) :: Constraint where
  AllBootstrappable '[] = ()
  AllBootstrappable (x ': xs) = (Bootstrappable x, AllBootstrappable xs)

-- | Takes a list of Bootstrappable files and converts them into `BuildPlanFile`s.
toBuildPlanFiles ::
  (AllBootstrappable xs, CanDieOnError m, MonadIO m, Quote m) =>
  HList xs ->
  m [(FilePath, BuildPlanFile)]
toBuildPlanFiles HNil = pure []
toBuildPlanFiles (x `HCons` xs) = do
  fmap catMaybes
    . (:)
    <$> toBuildPlanFile x
    <*> (fmap Just <$> toBuildPlanFiles xs)

type T = Tree String

toReasonTree :: BuildPlan -> T
toReasonTree = mergeDuplicateNodes . go (Node "/" []) . (nivFiles <>) . buildPlanFiles
  where
    nivFiles :: [(FilePath, BuildPlanFile)]
    nivFiles =
      [ ( "nix/sources.json",
          BuildPlanFile "This contains metadata about your nix dependencies." "" WillOverwrite
        ),
        ( "nix/sources.nix",
          BuildPlanFile "This is the interface between nix and the dependencies listed in sources.json." "" WillOverwrite
        )
      ]
    go :: T -> [(FilePath, BuildPlanFile)] -> T
    go t [] = t
    go t (x : xs) = go (widenTree t $ mkNode x) xs
    mkNode :: (FilePath, BuildPlanFile) -> Maybe T
    mkNode (path, f) = case nonEmpty $ splitDirectories path of
      Just (x :| xs) ->
        if null xs
          then Just $ Node (toString x <> " - " <> toString (reasonPhrase f)) []
          else Just $ Node (toString x) $ catMaybes [mkNode (joinPath xs, f)]
      Nothing -> Nothing
    widenTree :: T -> Maybe T -> T
    widenTree t Nothing = t
    widenTree t1@(Node _ forest) (Just t2) = t1 {subForest = t2 : forest}
    mergeDuplicateNodes :: T -> T
    mergeDuplicateNodes t@(Node _ []) = t
    mergeDuplicateNodes t@(Node _ forest) = t {subForest = mergeDuplicateNodes' forest}
    mergeDuplicateNodes' :: [T] -> [T]
    mergeDuplicateNodes' ts =
      let groups = groupBy rootLabel ts :: Map String (NonEmpty T)
          mergeGroup :: (String, NonEmpty T) -> [T] -> [T]
          mergeGroup (path, duplicates) =
            ((mergeDuplicateNodes . Node path . sortOn (map toLower . rootLabel) . sconcat $ subForest <$> duplicates) :)
       in foldr mergeGroup [] . sortOn (map toLower . fst) $ toPairs groups

confirmBuildPlan :: forall m. MonadBootstrap m => BuildPlan -> m (Maybe BuildPlan)
confirmBuildPlan BuildPlan {..} = do
  let willOverwrite = filter ((== WillOverwrite) . overwriteStatus . snd) buildPlanFiles
      willWriteFromNew = filter ((== NothingToOverwrite) . overwriteStatus . snd) buildPlanFiles
      keptInBuildPlan = filter ((/= NoContentChange) . overwriteStatus . snd) buildPlanFiles
  putLn
  if null keptInBuildPlan
    then do
      withAttribute (foreground green) $ putTextLn "Nothing to overwrite; everything is up-to-date."
      pure . Just $ BuildPlan []
    else do
      showSummaries (willOverwrite, willWriteFromNew)
      oldConfigExists <- liftIO $ doesFileExist ".nix-bootstrap.toml"
      if oldConfigExists
        then do
          withAttribute bold $ putText "I'm also going to "
          withAttributes [bold, foreground red] $ putText "DELETE"
          withAttribute bold $ putTextLn " the following files:"
          putTextLn "  - .nix-bootstrap.toml (it will be replaced by .nix-bootstrap.dhall)"
        else pass
      promptYesNo "Is that okay?" >>= \case
        True -> pure . Just $ BuildPlan keptInBuildPlan
        False -> pure Nothing
  where
    showSummaries :: ([(FilePath, BuildPlanFile)], [(FilePath, BuildPlanFile)]) -> m ()
    showSummaries (willOverwrite, willWriteFromNew) = do
      let forGroup :: [(FilePath, BuildPlanFile)] -> (NonEmpty (FilePath, BuildPlanFile) -> m ()) -> m ()
          forGroup g = fromMaybe pass . flip viaNonEmpty g
      forGroup willOverwrite \filesToOverwrite -> do
        withAttribute bold $ putText "I'm going to "
        withAttributes [bold, foreground yellow] $ putText "OVERWRITE"
        withAttribute bold $ putTextLn " the following files:"
        withAttribute (foreground yellow) $ showFiles filesToOverwrite
      forGroup willWriteFromNew \filesToWrite -> do
        withAttribute bold $ putText "I'm "
        unless (null willOverwrite) $ withAttribute bold (putText "also ")
        withAttribute bold $ putTextLn "going to write the following files:"
        showFiles filesToWrite
    showFiles :: NonEmpty (FilePath, BuildPlanFile) -> m ()
    showFiles = mapM_ (putTextLn . ("  - " <>)) . sort . toList . (toText . fst <$>)

bootstrap :: MonadBootstrap m => BuildPlan -> m ()
bootstrap BuildPlan {..} =
  forM_ buildPlanFiles \f@(buildPlanFilePath, _) -> do
    void
      . dieOnError id
      . ExceptT
      $ runWithProgressMsg Quick ("Bootstrapping " <> toText buildPlanFilePath) do
        r1 <- createParentDir buildPlanFilePath
        r2 <- bootstrapFile (second contents f)
        pure (r1, r2)

createParentDir :: MonadIO m => FilePath -> ExceptT Text m ()
createParentDir path =
  let dir = takeDirectory path
   in ExceptT $
        first
          ( (("Could not create parent directory for " <> toText path <> ": ") <>)
              . toText
              . displayException @IOException
          )
          <$> liftIO (try $ createDirectoryIfMissing True dir)

bootstrapFile :: MonadIO m => (FilePath, Text) -> ExceptT Text m ()
bootstrapFile (path, contents) =
  ExceptT $
    first
      ( (("Could not bootstrap " <> toText path <> ": ") <>)
          . toText
          . displayException @IOException
      )
      <$> liftIO (try $ writeFileText path contents)
