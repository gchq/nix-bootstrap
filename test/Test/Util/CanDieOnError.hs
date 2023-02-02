{-# OPTIONS_GHC -Wno-orphans #-}

-- | Description : This module provides an alternate instance for
--     CanDieOnError which doesn't require connection to a terminal.
-- Copyright : (c) Crown Copyright GCHQ
module Test.Util.CanDieOnError () where

import Bootstrap.Error (CanDieOnError (dieOnError))

instance {-# OVERLAPPING #-} CanDieOnError IO where
  dieOnError toErr a =
    runExceptT a >>= \case
      Right v -> pure v
      Left e -> error (toErr e)
