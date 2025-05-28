{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- | Copyright : (c) Crown Copyright GCHQ
--   Description : A representation of the system to which we're bootstrapping
module Bootstrap.Data.Target (Target (..)) where

import Dhall (FromDhall, ToDhall)
import Dhall.Deriving (AsIs, Codec (Codec), Constructor)

-- | The system to which we're bootstrapping.
--
-- For public purposes this will always be `TargetDefault`.
data Target
  = TargetDefault
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) Target
