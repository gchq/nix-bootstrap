-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.VSCodeSettingsSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.VSCodeSettings (vsCodeSettingsFor)
import Bootstrap.Data.DevContainer (DevContainerConfig (DevContainerConfig))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = describe ".vscode/settings.json rendering" do
  it "renders the json correctly" do
    bootstrapContent (vsCodeSettingsFor (DevContainerConfig True))
      >>= ( `shouldBe`
              Right
                ( unlines
                    ["{", "    \"nixEnvSelector.nixFile\": \"${workspaceRoot}/flake.nix\",", "    \"nixEnvSelector.useFlakes\": true", "}"]
                )
          )
