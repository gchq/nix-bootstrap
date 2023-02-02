-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.GitPodYmlSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.GitPodYml (GitPodYml (GitPodYml))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = describe ".gitpod.yml rendering" do
  it "renders the yml correctly" do
    bootstrapContent GitPodYml >>= (`shouldBe` Right "tasks: []\n")
