-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr.NixpkgsSpec (spec) where

import Bootstrap.Nix.Expr (isMostlyCorrectlyScoped)
import Bootstrap.Nix.Expr.Nixpkgs (nixpkgsExpr)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = do
  describe "nixpkgsExpr" do
    it "has no external scope requirements" do
      isMostlyCorrectlyScoped nixpkgsExpr `shouldBe` Right ()
