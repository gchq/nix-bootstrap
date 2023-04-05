{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (..), preCommitHooksConfigCodec) where

import Dhall (FromDhall, ToDhall)
import Toml (TomlCodec)
import qualified Toml

newtype PreCommitHooksConfig = PreCommitHooksConfig {unPreCommitHooksConfig :: Bool}
  deriving newtype (FromDhall, ToDhall)
  deriving stock (Eq, Show)

preCommitHooksConfigCodec :: TomlCodec PreCommitHooksConfig
preCommitHooksConfigCodec = Toml.diwrap (Toml.bool "setUpPreCommitHooks")
