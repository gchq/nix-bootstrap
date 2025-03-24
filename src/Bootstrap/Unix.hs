{-# LANGUAGE TemplateHaskell #-}

-- | Description : Defines utilities for working with command-line tools
-- Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Unix (alejandra, git, runCommand, which, whoami) where

import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.Terminal (putErrorLn)
import Control.Exception (IOException)
import Control.Monad.Catch (try)
import qualified Data.Text as T
import GHC.IO.Exception (userError)
import GHC.IO.Handle (hGetContents)
import System.IO.Silently (hSilence)
import System.Process
  ( CreateProcess (std_err, std_in, std_out),
    StdStream (CreatePipe, UseHandle),
    createProcess,
    proc,
    readProcess,
  )
import qualified System.Which as Which

-- | Finds and runs git, returning the result of the command with the given args.
--
-- Shows an error message and exits with failure status (diverges)
-- if git is not available on the system.
git :: (MonadBootstrap m) => [String] -> m (Either IOException String)
git args =
  which "git" >>= \case
    Just g -> runCommand g args
    Nothing -> do
      putErrorLn "Git does not appear to be installed. Please install it and then re-run nix-bootstrap."
      exitFailure

-- | Runs alejandra against the given nix expression
alejandra :: (MonadIO m) => String -> ExceptT IOException m String
alejandra expr = do
  -- hExpr is the pipe from printf with the correctly sanitised (for terminal use)
  -- expression in it
  (_, hExpr, _, _) <-
    ExceptT
      . liftIO
      . try
      $ createProcess (proc "printf" ["%s", expr]) {std_out = CreatePipe}
  case hExpr of
    Just hExpr' -> do
      -- hFormatted is stdout from alejandra
      (_, hFormatted, hError, _) <-
        ExceptT
          . liftIO
          . try
          $ createProcess
            (proc $(Which.staticWhich "alejandra") [])
              { std_err = CreatePipe,
                std_in = UseHandle hExpr',
                std_out = CreatePipe
              }
      case hFormatted of
        Just h -> do
          formatted <- ExceptT . liftIO . try $ hGetContents h
          if not (null formatted)
            then pure formatted
            else case hError of
              Just hErr -> ExceptT (Left . userError <$> liftIO (hGetContents hErr))
              Nothing -> hoistEither (Left $ userError "hError was Nothing; should not happen")
        Nothing -> hoistEither (Left $ userError "hFormatted was Nothing; should not happen")
    Nothing -> hoistEither (Left $ userError "hExpr was Nothing; should not happen")

-- | Gets the path to the requested executable on the system using `which`
which :: (MonadIO m) => FilePath -> m (Maybe FilePath)
which = liftIO . Which.which

-- | Gets the name of the current user using `whoami`
whoami :: (MonadIO m) => m (Either IOException FilePath)
whoami = (toString . T.strip . toText) <<$>> runCommand "whoami" []

-- | Runs the given command, silencing (and capturing) output
runCommand :: (MonadIO m) => FilePath -> [String] -> m (Either IOException String)
runCommand binaryPath args =
  liftIO
    . try
    . hSilence [stdout, stderr]
    $ readProcess binaryPath args ""
