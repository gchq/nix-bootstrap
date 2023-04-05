{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.NixPreCommitHookConfigSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig
  ( nixPreCommitHookConfigFor,
  )
import Bootstrap.Data.ProjectType
  ( InstallLombok (InstallLombok),
    InstallMinishift (InstallMinishift),
    JavaOptions (JavaOptions),
    NodePackageManager (NPM),
    ProjectType (Go, Java, Node),
    SetUpGoBuild (SetUpGoBuild),
    SetUpJavaBuild (NoJavaBuild),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Util.RunConfig (rcDefault, rcWithFlakes)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "nix/pre-commit-hooks.nix rendering" do
  it "renders correctly when using default Go hooks" do
    bootstrapContent (nixPreCommitHookConfigFor rcWithFlakes $ Go $ SetUpGoBuild False)
      >>= ( `shouldBe`
              Right
                [r|{
  pre-commit-hooks-lib,
  nixpkgs,
  system,
}: {
  hooks = pre-commit-hooks-lib.lib.${system}.run {
    src = ../.;
    hooks = {
      alejandra.enable = true;
      go-fmt = {
        enable = true;
        entry = "${nixpkgs.go}/bin/go fmt";
        files = "\\.go$";
        pass_filenames = false;
      };
      go-test = {
        enable = true;
        entry = "${nixpkgs.go}/bin/go test";
        files = "\\.(go|mod)$";
        pass_filenames = false;
      };
    };
  };
  tools = (with pre-commit-hooks-lib.packages.${system}; [alejandra]) ++ (with nixpkgs; [go]);
}
|]
          )
  it "renders correctly when using default NPM hooks" do
    bootstrapContent (nixPreCommitHookConfigFor rcDefault $ Node NPM)
      >>= ( `shouldBe`
              Right
                [r|{pre-commit-hooks-lib}: {
  hooks = pre-commit-hooks-lib.run {
    src = ../.;
    hooks = {
      alejandra.enable = true;
      prettier.enable = true;
    };
  };
  tools = with pre-commit-hooks-lib; [alejandra prettier];
}
|]
          )
  it "renders correctly when using default Java hooks" do
    bootstrapContent
      ( nixPreCommitHookConfigFor
          rcDefault
          (Java $ JavaOptions (InstallMinishift False) (InstallLombok False) NoJavaBuild)
      )
      >>= ( `shouldBe`
              Right
                [r|{
  pre-commit-hooks-lib,
  nixpkgs,
}: {
  hooks = pre-commit-hooks-lib.run {
    src = ../.;
    hooks = {
      alejandra.enable = true;
      google-java-format = {
        enable = true;
        entry = "${nixpkgs.google-java-format}/bin/google-java-format -i";
        files = "\\.java$";
        pass_filenames = true;
      };
    };
  };
  tools = (with pre-commit-hooks-lib; [alejandra]) ++ (with nixpkgs; [google-java-format]);
}
|]
          )
