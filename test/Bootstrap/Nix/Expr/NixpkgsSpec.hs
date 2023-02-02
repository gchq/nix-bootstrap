-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr.NixpkgsSpec (spec) where

import Bootstrap.Nix.Expr (isMostlyCorrectlyScoped)
import Bootstrap.Nix.Expr.Nixpkgs (nixpkgsFromIntermediateFlake, nixpkgsFromNiv)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = do
  describe "nixpkgsFromIntermediateFlake" do
    it "has no external scope requirements" do
      isMostlyCorrectlyScoped nixpkgsFromIntermediateFlake `shouldBe` Right ()
  describe "nixpkgsFromNiv" do
    it "has no external scope requirements" do
      isMostlyCorrectlyScoped nixpkgsFromNiv `shouldBe` Right ()
