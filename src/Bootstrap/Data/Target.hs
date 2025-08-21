{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- | Copyright : (c) Crown Copyright GCHQ
--   Description : A representation of the system to which we're bootstrapping
module Bootstrap.Data.Target (Target (..), TargetV1 (..)) where

import Dhall (FromDhall, ToDhall)
import Dhall.Deriving (AsIs, Codec (Codec), Constructor, DropPrefix)

-- | The system to which we're bootstrapping.
--
-- For public purposes this will always be `TargetDefault`.
data Target
  = TargetDefault
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) Target

-- | An older representation of Target
data TargetV1
  = V1TargetDefault
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor (DropPrefix "V1")) TargetV1
