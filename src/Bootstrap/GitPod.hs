-- | Description : Defines additional helpers used when nix-bootstrap is running in GitPod
-- Copyright : (c) Crown Copyright GCHQ
module Bootstrap.GitPod (resetPermissionsInGitPod) where

import Bootstrap.Error
  ( CanDieOnError (dieOnError'),
    InProgressDuration (LongRunning),
    runWithProgressMsg,
  )
import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.Terminal (putErrorLn)
import Bootstrap.Unix (runCommand, whoami)

-- | If running in GitPod, chowns the /nix directory to the gitpod
-- user to prevent permissions errors.
resetPermissionsInGitPod :: MonadBootstrap m => m ()
resetPermissionsInGitPod =
  whoami >>= \case
    Right "gitpod" -> do
      void
        . dieOnError'
        . ExceptT
        . runWithProgressMsg LongRunning "Fixing GitPod's permissions on /nix"
        . ExceptT
        $ runCommand "/usr/bin/sudo" ["chown", "-R", "gitpod:gitpod", "/nix"]
    Right _ -> pass
    Left e -> do
      putErrorLn $ "Could not get current user: " <> toText (displayException e)
      putErrorLn "If running in GitPod, later steps may fail."
