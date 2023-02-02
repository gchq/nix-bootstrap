{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Prelude (module Relude, clamp) where

import Relude hiding (putText, putTextLn, state)

-- | Given a minimum value and a maximum value, clamp a value to that range
-- (values less than the minimum map to the minimum and values greater than
-- the maximum map to the maximum).
--
-- >>> clamp 1 10 11
-- 10
-- >>> clamp 1 10 2
-- 2
-- >>> clamp 5 10 1
-- 5
clamp ::
  Ord a =>
  -- | The minimum value
  a ->
  -- | The maximum value
  a ->
  -- | The value to clamp
  a ->
  a
clamp mn mx val = max mn (min val mx)
