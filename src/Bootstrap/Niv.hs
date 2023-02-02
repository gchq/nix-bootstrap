-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Niv (initialiseNiv) where

import Bootstrap.Cli (RunConfig (RunConfig, rcNonInteractive))
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig, unPreCommitHooksConfig)
import Bootstrap.Error
  ( CanDieOnError (dieOnError'),
    InProgressDuration (LongRunning),
    runWithProgressMsg,
  )
import Bootstrap.GitPod (resetPermissionsInGitPod)
import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.Terminal (promptYesNoWithDefault)
import Bootstrap.Unix (niv)
import Control.Exception (IOException)
import Relude.Extra.Bifunctor (secondF)
import System.Terminal (putTextLn)

initialiseNiv :: MonadBootstrap m => RunConfig -> PreCommitHooksConfig -> m ()
initialiseNiv RunConfig {rcNonInteractive} preCommitHooksConfig =
  promptYesNoWithDefault
    (if rcNonInteractive then Just True else Nothing)
    "First, I need to pin a version of nixpkgs in the ./nix folder. Is that okay?"
    >>= \case
      True -> do
        resetPermissionsInGitPod
        void
          . dieOnError'
          . ExceptT
          $ runWithProgressMsg LongRunning "Pinning a version of nixpkgs" runInit
        resetPermissionsInGitPod
        void
          . when (unPreCommitHooksConfig preCommitHooksConfig)
          . dieOnError'
          $ ExceptT addOrUpdatePreCommitHooks
      False -> putTextLn "Okay, exiting." *> exitFailure

runInit :: MonadIO m => ExceptT IOException m ()
runInit = do
  void $ niv ["init"]
  void $ niv ["update", "nixpkgs", "-b", "nixpkgs-unstable"]

addOrUpdatePreCommitHooks :: MonadIO m => m (Either IOException ())
addOrUpdatePreCommitHooks = do
  niv ["show", "pre-commit-hooks"]
    >>= secondF (const ()) . \case
      Left _ -> niv ["add", "cachix/pre-commit-hooks.nix", "-n", "pre-commit-hooks"]
      Right _ -> niv ["update", "pre-commit-hooks"]
