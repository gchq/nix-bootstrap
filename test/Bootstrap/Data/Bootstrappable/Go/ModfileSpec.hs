-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Go.ModfileSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Go.Modfile (GoModfile (GoModfile))
import Bootstrap.Data.ProjectName (mkProjectName)
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = describe "go.mod rendering" do
  it "renders correctly given a dummy version" do
    let projectName = Unsafe.fromJust $ mkProjectName "test-project"
    bootstrapContent (GoModfile projectName "dummy-version")
      >>= (`shouldBe` Right "module test-project\n\ngo dummy-version\n")
