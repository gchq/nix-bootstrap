{-# LANGUAGE ConstraintKinds #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Monad (MonadBootstrap) where

import Control.Monad.Catch (MonadCatch, MonadMask)
import System.Terminal
  ( MonadColorPrinter,
    MonadFormattingPrinter,
    MonadInput,
    MonadMarkupPrinter,
    MonadScreen,
  )

type MonadBootstrap m =
  ( MonadCatch m,
    MonadColorPrinter m,
    MonadFormattingPrinter m,
    MonadInput m,
    MonadIO m,
    MonadMarkupPrinter m,
    MonadMask m,
    MonadScreen m
  )
