-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig (..),
    continuousIntegrationConfigCodec,
  )
where

import Toml (TomlCodec)
import qualified Toml

newtype ContinuousIntegrationConfig = ContinuousIntegrationConfig {unContinuousIntegrationConfig :: Bool}
  deriving stock (Eq, Show)

continuousIntegrationConfigCodec :: TomlCodec ContinuousIntegrationConfig
continuousIntegrationConfigCodec = Toml.diwrap (Toml.bool "setUpContinuousIntegration")
