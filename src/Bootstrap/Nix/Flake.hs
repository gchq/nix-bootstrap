{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Flake (generateIntermediateFlake, intermediateFlake) where

import Bootstrap.Cli (RunConfig (RunConfig, rcNonInteractive))
import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Error
  ( CanDieOnError (dieOnError'),
    InProgressDuration (LongRunning),
    runWithProgressMsg,
  )
import Bootstrap.GitPod (resetPermissionsInGitPod)
import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.Nix.Evaluate (NixBinaryPaths, runNix)
import Bootstrap.Nix.Expr
  ( Expr (ELit, ESet),
    Literal (LString),
    nixbinding,
    nixproperty,
    writeExpr,
    (|=),
  )
import Bootstrap.Terminal (promptYesNoWithDefault)
import Bootstrap.Unix (git)
import Control.Exception (IOException)
import Control.Monad.Catch (MonadCatch, try)
import System.Terminal (MonadPrinter (putTextLn))

generateIntermediateFlake :: MonadBootstrap m => NixBinaryPaths -> RunConfig -> ProjectName -> m ()
generateIntermediateFlake nixBinaryPaths RunConfig {rcNonInteractive} projectName =
  promptYesNoWithDefault
    (if rcNonInteractive then Just True else Nothing)
    "First, I need to pin a version of nixpkgs in flake.nix. Is that okay?"
    >>= \case
      True -> do
        resetPermissionsInGitPod
        void
          . dieOnError'
          . ExceptT
          . runWithProgressMsg LongRunning "Pinning a version of nixpkgs"
          $ do
            writeIntermediateFlake projectName
            void . ExceptT $ git ["add", "--intent-to-add", "flake.nix"]
            void . ExceptT $ runNix nixBinaryPaths ["flake", "lock"]
      False -> putTextLn "Okay, exiting." *> exitFailure

writeIntermediateFlake :: (MonadCatch m, MonadIO m) => ProjectName -> ExceptT IOException m ()
writeIntermediateFlake projectName =
  void
    . try @_ @IOException
    . writeFileText "flake.nix"
    . (<> "\n")
    . writeExpr
    $ intermediateFlake projectName

intermediateFlake :: ProjectName -> Expr
intermediateFlake projectName =
  ESet
    False
    [ [nixproperty|description|] |= ELit (LString ("Development infrastructure for " <> unProjectName projectName)),
      [nixbinding|inputs = {
        nixpkgs-src.url = "github:NixOS/nixpkgs";
      };|],
      [nixbinding|outputs = _: {};|]
    ]
