{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable
  ( Bootstrappable (..),
    bootstrapContentHaskell,
    bootstrapContentNix,
    bootstrapContentPrettyJson,
    bootstrapContentYaml,

    -- * Haskell Modules
    HaskellModule (..),
    HaskellImport (..),
    haskellModule,
    haskellModulePragmas,
    haskellModuleName,
    haskellModuleExports,
    haskellModuleImports,
    haskellModuleDecs,
  )
where

import Bootstrap.Nix.Expr
  ( CommentsPolicy (ShowComments),
    Identifier (unIdentifier),
    IsNixExpr (toNixExpr),
    isMostlyCorrectlyScoped,
    writeExprFormatted,
  )
import Control.Lens (makeLenses)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty
  ( Config (confCompare),
    defConfig,
    encodePretty',
    keyOrder,
  )
import qualified Data.Text as T
import qualified Data.Yaml.Aeson as Yaml
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS

-- | Things which can be written out as files during the bootstrapping
-- process.
class Bootstrappable a where
  -- | Wraps the value in a Just constructor. Should only use Nothing
  -- when there can be no meaningful bootstrapping performed from the
  -- value.
  bootstrapWithMaybe :: a -> Maybe a
  bootstrapWithMaybe = Just

  -- | The filename of the file when bootstrapped
  bootstrapName :: a -> FilePath

  -- | The human-readable reason we bootstrap this file
  bootstrapReason :: a -> Text

  -- | The contents of the file
  bootstrapContent :: (MonadIO m, TH.Quote m) => a -> m (Either String Text)

-- | The Maybe instance for Bootstrappable allows handling of files which
-- may have been bootstrapped, but now shouldn't be due to some condition.
--
-- If given a `Just` value, it behaves the same way as the underlying instance.
--
-- If given a Nothing value, it **must not** be bootstrapped; any attempts to do
-- so will call `error`. This can be safely avoided by checking for a `Nothing`
-- value using `bootstrapWithMaybe`.
instance (Bootstrappable a) => Bootstrappable (Maybe a) where
  bootstrapWithMaybe (Just a) = Just (Just a)
  bootstrapWithMaybe Nothing = Nothing
  bootstrapName = maybe (error "Called bootstrapName on Nothing value") bootstrapName
  bootstrapReason = maybe (error "Called bootstrapReason on Nothing value") bootstrapReason
  bootstrapContent (Just a) = bootstrapContent a
  bootstrapContent Nothing = error "Called bootstrapContent on Nothing value"

-- | Represents a complete haskell module
data HaskellModule = HaskellModule
  { _haskellModulePragmas :: Maybe (NonEmpty Text),
    _haskellModuleName :: THS.ModName,
    _haskellModuleExports :: NonEmpty Text,
    _haskellModuleImports :: Maybe (NonEmpty HaskellImport),
    _haskellModuleDecs :: Maybe (NonEmpty THS.Dec)
  }

-- | Represents an import in haskell
data HaskellImport
  = -- | import ModName
    HaskellImportAll THS.ModName
  | -- | import ModName (name1, name2)
    HaskellImport THS.ModName [TH.Name]

-- | Creates a `HaskellModule` with the minimum required information
haskellModule ::
  THS.ModName ->
  -- | Exports
  NonEmpty Text ->
  HaskellModule
haskellModule modName exports =
  HaskellModule
    { _haskellModulePragmas = Nothing,
      _haskellModuleName = modName,
      _haskellModuleExports = exports,
      _haskellModuleImports = Nothing,
      _haskellModuleDecs = Nothing
    }

-- | A helper function when the generated file should be haskell code
bootstrapContentHaskell :: HaskellModule -> Text
bootstrapContentHaskell HaskellModule {..} =
  let THS.ModName (toText -> modName) = _haskellModuleName
   in (<> "\n")
        . T.intercalate "\n\n"
        $ catMaybes
          [ T.intercalate "\n" . toList <$> _haskellModulePragmas,
            Just $
              "module "
                <> modName
                <> " ("
                <> T.intercalate ", " (toList _haskellModuleExports)
                <> ") where",
            formatImports <$> _haskellModuleImports,
            formatDecs <$> _haskellModuleDecs
          ]
  where
    formatImports :: NonEmpty HaskellImport -> Text
    formatImports = T.intercalate "\n" . toList . fmap formatImport
    formatDecs :: NonEmpty THS.Dec -> Text
    formatDecs = toText . TH.pprint . toList
    formatImport :: HaskellImport -> Text
    formatImport (HaskellImportAll (THS.ModName iModName)) = "import " <> toText iModName
    formatImport (HaskellImport (THS.ModName iModName) names) =
      "import "
        <> toText iModName
        <> " ("
        <> formatNames names
        <> ")"
    formatNames :: [TH.Name] -> Text
    formatNames = T.intercalate ", " . fmap (toText . TH.pprint . TH.VarE)

-- | A helper function when the generated file should be nix code
bootstrapContentNix :: (IsNixExpr a, MonadIO m) => a -> m (Either String Text)
bootstrapContentNix a = do
  let expr = toNixExpr a
   in case isMostlyCorrectlyScoped expr of
        Right () -> do
          first
            ( ("Could not format nix expression: " <>)
                . displayException
            )
            <$> writeExprFormatted ShowComments expr
        Left (i1 :| iRest) ->
          Left
            . ( ( "Nix expression is incorrectly scoped; it references the out of scope "
                    <> ( if null iRest
                           then "identifier " <> toString (unIdentifier i1)
                           else
                             "identifiers "
                               <> toString
                                 ( foldr (\i acc -> unIdentifier i <> ", " <> acc) ("and " <> unIdentifier i1) iRest
                                 )
                       )
                    <> ". This is a bug in nix-bootstrap; please "
                    <> "contact the nix-bootstrap team to report it.\nBad expr was: "
                )
                  <>
              )
            . show
            <$> writeExprFormatted ShowComments expr

-- | A helper function when the generated file should be JSON
--
-- Keys in the first argument will appear first in their given order. Other keys
-- will be ordered alphabetically.
bootstrapContentPrettyJson :: (ToJSON a, Monad m) => [Text] -> a -> m (Either String Text)
bootstrapContentPrettyJson keysToPrioritise =
  pure
    . Right
    . (<> "\n")
    . decodeUtf8With lenientDecode
    . toStrict
    . encodePretty'
      defConfig {confCompare = keyOrder keysToPrioritise <> compare}

-- | A helper function when the generated file should be YAML
bootstrapContentYaml :: (ToJSON a) => a -> Text
bootstrapContentYaml = decodeUtf8With lenientDecode . Yaml.encode

makeLenses ''HaskellModule
