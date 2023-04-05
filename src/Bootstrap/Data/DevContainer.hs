{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.DevContainer (DevContainerConfig (..), devContainerConfigCodec) where

import Dhall (FromDhall, ToDhall)
import Toml (TomlCodec)
import qualified Toml

newtype DevContainerConfig = DevContainerConfig {unDevContainerConfig :: Bool}
  deriving newtype (FromDhall, ToDhall)
  deriving stock (Eq, Show)

devContainerConfigCodec :: TomlCodec DevContainerConfig
devContainerConfigCodec = Toml.diwrap (Toml.bool "setUpDevContainer")
