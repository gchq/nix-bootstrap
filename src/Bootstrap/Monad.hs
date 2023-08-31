{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Monad (MonadBootstrap) where

import Control.Monad.Catch (MonadCatch, MonadMask)
import Language.Haskell.TH (Quote (newName))
import System.Terminal
  ( MonadColorPrinter,
    MonadFormattingPrinter,
    MonadInput,
    MonadMarkupPrinter,
    MonadScreen,
    TerminalT,
  )

type MonadBootstrap m =
  ( MonadCatch m,
    MonadColorPrinter m,
    MonadFormattingPrinter m,
    MonadInput m,
    MonadIO m,
    MonadMarkupPrinter m,
    MonadMask m,
    MonadScreen m,
    Quote m
  )

instance (Monad (TerminalT m a), MonadIO (TerminalT m a)) => Quote (TerminalT m a) where
  newName = liftIO . newName
