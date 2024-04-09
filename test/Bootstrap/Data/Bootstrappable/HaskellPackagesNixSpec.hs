{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.HaskellPackagesNixSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.HaskellPackagesNix (haskellPackagesNixFor)
import Bootstrap.Data.GHCVersion (GHCVersion (GHCVersion))
import Bootstrap.Data.ProjectType
  ( HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly),
    ProjectType (Go, Haskell),
    SetUpGoBuild (SetUpGoBuild),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "haskell-packages.nix rendering" do
  it "renders nothing for a non-haskell project" do
    haskellPackagesNixFor (Go $ SetUpGoBuild True)
      `shouldBe` Nothing
  it "renders correctly for a Haskell repl-only project" do
    case haskellPackagesNixFor (Haskell $ HaskellOptions (GHCVersion 9 4 2) HaskellProjectTypeReplOnly) of
      Just haskellPackagesNix ->
        bootstrapContent haskellPackagesNix
          >>= ( `shouldBe`
                  Right
                    [r|{nixpkgs}:
nixpkgs.haskell.packages.ghc942.override {
  overrides = _: super: {
    # You can overide packages here if you need any dependencies not in this set by default
  };
}
|]
              )
      Nothing -> fail "Gave nothing for a project which should've had a haskell-packages.nix generated."
  it "renders correctly for a full Haskell project" do
    case haskellPackagesNixFor (Haskell $ HaskellOptions (GHCVersion 9 4 2) HaskellProjectTypeBasic) of
      Just haskellPackagesNix ->
        bootstrapContent haskellPackagesNix
          >>= ( `shouldBe`
                  Right
                    [r|{nixpkgs}:
nixpkgs.haskell.packages.ghc942.override {
  overrides = _: super: {
    # The override of pretty-simple below may be needed to circumvent a bug in nixpkgs.
    # If the devshell builds successfully without it, feel free to remove it.
    pretty-simple = super.pretty-simple.overrideAttrs {
      doCheck = false;
    };
    # You can overide packages here if you need any dependencies not in this set by default
  };
}
|]
              )
      Nothing -> fail "Gave nothing for a project which should've had a haskell-packages.nix generated."
