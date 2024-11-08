-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Go.Modfile (GoModfile (GoModfile), goModfileFor) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason))
import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Error (CanDieOnError (dieOnErrorWithPrefix))
import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.Nix.Evaluate
  ( NixBinaryPaths,
    getVersionOfNixpkgsAttribute,
  )
import Text.Regex (mkRegex, subRegex)

data GoModfile = GoModfile {goModfileProjectName :: ProjectName, goModfileGoVersion :: Text}

instance Bootstrappable GoModfile where
  bootstrapName = const "go.mod"
  bootstrapReason = const "The declaration of your Go module and its dependencies"
  bootstrapContent GoModfile {..} =
    pure . Right $
      unlines
        [ "module " <> unProjectName goModfileProjectName,
          "",
          "go " <> goModfileGoVersion
        ]

goModfileFor :: MonadBootstrap m => NixBinaryPaths -> ProjectName -> m GoModfile
goModfileFor nixBinaryPaths projectName = do
  goVersion <-
    dieOnErrorWithPrefix "Could not get bootstrapped Go version"
      . ExceptT
      $ getVersionOfNixpkgsAttribute nixBinaryPaths "go"
  let majorMinor = toText $ subRegex (mkRegex "^([0-9]+\\.[0-9]+).*") goVersion "\\1"
  pure $ GoModfile projectName majorMinor
