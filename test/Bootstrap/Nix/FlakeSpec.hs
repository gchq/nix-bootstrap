-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.FlakeSpec (spec) where

import Bootstrap.Data.ProjectName (mkProjectName)
import Bootstrap.Nix.Expr (writeExprFormatted)
import Bootstrap.Nix.Flake (intermediateFlake)
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = describe "intermediateFlake" do
  it "correctly writes the intermediate flake" do
    e <- writeExprFormatted (intermediateFlake $ Unsafe.fromJust $ mkProjectName "test-project")
    e
      `shouldBe` Right
        ( unlines
            [ "{",
              "  description = \"Development infrastructure for test-project\";",
              "  inputs = {",
              "    nixpkgs-src.url = \"github:NixOS/nixpkgs\";",
              "  };",
              "  outputs = _: {};",
              "}"
            ]
        )
