{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Evaluate
  ( NixBinaryPaths,
    runNix,
    evaluateNixExpression,
    getNixBinaryPaths,
    getNixConfig,
    getNixVersion,
    getVersionOfNixpkgsAttribute,
    extractNixVersionString,
    getAvailableGHCVersions,
  )
where

import Bootstrap.Data.GHCVersion (GHCVersion, parseGHCVersion)
import Bootstrap.Data.Version (MajorVersion, parseMajorVersion)
import Bootstrap.Error
  ( CanDieOnError (dieOnError),
    InProgressDuration (LongRunning),
    runWithProgressMsg,
  )
import Bootstrap.GitPod (resetPermissionsInGitPod)
import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.Nix.Expr
  ( Expr (ELetIn),
    Identifier (Identifier),
    Property (PIdent),
    nix,
    nixproperty,
    writeExprForTerminal,
    (|.),
  )
import Bootstrap.Nix.Expr.Nixpkgs (nixpkgsBinding)
import Bootstrap.Unix (runCommand, which)
import Control.Exception (IOException, ioError, try)
import qualified Data.Text as T
import qualified Relude.Extra.Map as M
import System.IO.Error (userError)
import System.IO.Silently (hSilence)
import System.Process (readProcess)
import Text.Megaparsec
  ( Parsec,
    anySingleBut,
    between,
    errorBundlePretty,
    many,
    parse,
    parseMaybe,
  )
import Text.Megaparsec.Char (char, space)
import Text.Regex (matchRegex, mkRegex, subRegex)

data NixBinaryPaths = NixBinaryPaths
  { nixPath :: FilePath,
    nixEnvPath :: FilePath,
    nixInstantiatePath :: FilePath
  }

runNix :: (MonadIO m) => NixBinaryPaths -> [String] -> m (Either IOException String)
runNix NixBinaryPaths {nixPath} = runCommand nixPath

-- | Gets the path to the nix binary on the system using `which`
getNixBinaryPaths :: (MonadIO m) => MaybeT m NixBinaryPaths
getNixBinaryPaths = do
  nixPath <- MaybeT $ which "nix"
  nixEnvPath <- MaybeT $ which "nix-env"
  nixInstantiatePath <- MaybeT $ which "nix-instantiate"
  pure $ NixBinaryPaths {..}

getNixConfig :: (MonadIO m) => NixBinaryPaths -> ExceptT IOException m (HashMap Text Text)
getNixConfig nixBinaryPaths = do
  configLines <-
    lines . toText
      <$> ExceptT
        ( runNix
            nixBinaryPaths
            [ "--extra-experimental-features",
              "nix-command",
              "show-config"
            ]
        )
  runNix nixBinaryPaths ["show-config"] >>= \case
    Left _ -> liftIO . ioError $ userError "Flakes are not configured properly; the new-style nix command isn't enabled."
    _ -> pass
  pure $ foldr buildConfigMap mempty configLines
  where
    buildConfigMap :: Text -> HashMap Text Text -> HashMap Text Text
    buildConfigMap nextLine acc = case T.split (== '=') nextLine of
      (k : vParts) -> do
        let kStripped = T.strip k
            vStripped = T.strip $ T.intercalate "=" vParts
        if not (T.null kStripped) && not (T.null vStripped)
          then M.insert kStripped vStripped acc
          else acc
      _ -> acc

getNixVersion :: (MonadIO m) => NixBinaryPaths -> ExceptT IOException m MajorVersion
getNixVersion NixBinaryPaths {nixEnvPath} = do
  versionString <- ExceptT (runCommand nixEnvPath ["--version"])
  case matchRegex (mkRegex "([.0-9]+)") versionString of
    Just (versionNumberString : _) -> case parseMajorVersion versionNumberString of
      Just majorVersion -> pure majorVersion
      Nothing -> liftIO . ioError $ userError "Version number extracted from nix-env --version was invalid"
    _ -> liftIO . ioError $ userError "Could not extract version number from nix-env --version"

-- | Encloses the expression in single quotes and evaluates it, giving back stdout in a `Right` value.
--
-- Gives a `Left` value if the command execution fails
evaluateNixExpression :: (MonadIO m) => NixBinaryPaths -> Expr -> m (Either IOException String)
evaluateNixExpression NixBinaryPaths {..} expr =
  liftIO
    . try
    . hSilence [stdout, stderr]
    $ readProcess
      nixPath
      ["eval", "--impure", "--expr", toString (writeExprForTerminal expr)]
      ""

-- | Gets the version of the derivation named that is provided by the pinned nixpkgs.
--
-- >>> getVersionOfNixpkgsAttribute (NixBinaryPaths {..}) (RunConfig {..}) "go"
-- "1.17.7"
getVersionOfNixpkgsAttribute :: (MonadBootstrap m) => NixBinaryPaths -> Text -> m (Either IOException String)
getVersionOfNixpkgsAttribute nixBinaryPaths attrName = do
  resetPermissionsInGitPod
  runWithProgressMsg LongRunning ("Getting version of " <> attrName <> " in the pinned version of nixpkgs") . ExceptT $
    evaluateNixExpression
      nixBinaryPaths
      ( ELetIn
          (one nixpkgsBinding)
          [nix|nixpkgs|]
          |. PIdent (Identifier attrName)
          |. [nixproperty|.version|]
      )
      <&> fmap extractNixVersionString

-- | Strips quotes and other sundries from a version number retrieved
-- using nix eval
extractNixVersionString :: String -> String
extractNixVersionString nixVersionValue =
  subRegex
    (mkRegex "^\"([0-9]+(\\.[0-9]+)*).*")
    (toString . T.strip $ toText nixVersionValue)
    "\\1"

-- | Gets the available GHC versions in the pinned nixpkgs.
getAvailableGHCVersions :: (MonadBootstrap m) => NixBinaryPaths -> m (Set GHCVersion)
getAvailableGHCVersions nixBinaryPaths =
  dieOnError
    (("Could not get list of available GHC versions: " <>) . toText . displayException)
    $ ExceptT do
      resetPermissionsInGitPod
      runWithProgressMsg LongRunning "Getting available versions of GHC in the pinned version of nixpkgs" . ExceptT $
        evaluateNixExpression
          nixBinaryPaths
          ( ELetIn
              (one nixpkgsBinding)
              [nix|builtins.attrNames nixpkgs.haskell.packages|]
          )
          <&> second (parse parseAttributes "")
          >>= \case
            Left e -> pure $ Left e
            Right (Left e) -> pure . Left . userError $ errorBundlePretty e
            Right (Right attrs) ->
              pure
                . Right
                . fromList
                . mapMaybe (parseMaybe parseGHCVersion)
                $ attrs
  where
    parseAttributes :: Parsec Void String [String]
    parseAttributes = between (char '[') (char ']') $ Text.Megaparsec.many do
      void space
      attrName <- between (char '\"') (char '\"') . Text.Megaparsec.many $ anySingleBut '\"'
      void space
      pure attrName
