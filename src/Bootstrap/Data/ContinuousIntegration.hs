{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig (..),
    continuousIntegrationConfigCodec,
  )
where

import Dhall (FromDhall, ToDhall)
import Toml (TomlCodec)
import qualified Toml

newtype ContinuousIntegrationConfig = ContinuousIntegrationConfig {unContinuousIntegrationConfig :: Bool}
  deriving newtype (FromDhall, ToDhall)
  deriving stock (Eq, Show)

continuousIntegrationConfigCodec :: TomlCodec ContinuousIntegrationConfig
continuousIntegrationConfigCodec = Toml.diwrap (Toml.bool "setUpContinuousIntegration")
