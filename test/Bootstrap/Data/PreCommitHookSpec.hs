-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.PreCommitHookSpec (spec) where

import Bootstrap.Data.PreCommitHook
  ( PreCommitHooksConfig (PreCommitHooksConfig),
    preCommitHooksConfigCodec,
  )
import Test.Hspec (Spec, describe, it)
import Test.Util (tomlRoundtripTest)

spec :: Spec
spec = describe "PreCommitHooksConfig" do
  it "roundtrips to TOML when True" $ tomlRoundtripTest preCommitHooksConfigCodec (PreCommitHooksConfig True)
  it "roundtrips to TOML when False" $ tomlRoundtripTest preCommitHooksConfigCodec (PreCommitHooksConfig False)
