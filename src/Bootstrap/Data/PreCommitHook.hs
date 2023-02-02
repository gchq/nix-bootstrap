-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (..), preCommitHooksConfigCodec) where

import Toml (TomlCodec)
import qualified Toml

newtype PreCommitHooksConfig = PreCommitHooksConfig {unPreCommitHooksConfig :: Bool}
  deriving stock (Eq, Show)

preCommitHooksConfigCodec :: TomlCodec PreCommitHooksConfig
preCommitHooksConfigCodec = Toml.diwrap (Toml.bool "setUpPreCommitHooks")
