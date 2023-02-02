-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.DevContainerSpec (spec) where

import Bootstrap.Data.DevContainer
  ( DevContainerConfig (DevContainerConfig),
    devContainerConfigCodec,
  )
import Test.Hspec (Spec, describe, it)
import Test.Util (tomlRoundtripTest)

spec :: Spec
spec = describe "DevContainerConfig" do
  it "roundtrips to TOML when True" $ tomlRoundtripTest devContainerConfigCodec (DevContainerConfig True)
  it "roundtrips to TOML when False" $ tomlRoundtripTest devContainerConfigCodec (DevContainerConfig False)
