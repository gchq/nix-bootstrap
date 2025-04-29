{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr.MkShellSpec (spec) where

import Bootstrap.Data.PreCommitHook
  ( PreCommitHooksConfig (PreCommitHooksConfig),
  )
import Bootstrap.Data.ProjectType
  ( ProjectType (Minimal, Rust),
  )
import Bootstrap.Nix.Expr (nix)
import Bootstrap.Nix.Expr.MkShell
  ( BuildInputSpec (BuildInputSpec),
    mkShell,
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = do
  describe "mkShell" do
    it "gives no shell hook when one isn't needed" do
      mkShell (BuildInputSpec [] [] (PreCommitHooksConfig False) Minimal [])
        `shouldBe` [nix|nixpkgs.mkShell {
  buildInputs = [
    # Insert any dependencies that should exist in the dev shell environment here
  ];
}|]
    it "gives a proper shell hook for projects with pre-commit hooks" do
      mkShell (BuildInputSpec [] [] (PreCommitHooksConfig True) Minimal [])
        `shouldBe` [nix|nixpkgs.mkShell {
  buildInputs = preCommitHooks.tools;
  inherit (preCommitHooks.allHooks) shellHook;
}|]
    it "gives a proper shell hook for Rust projects" do
      mkShell (BuildInputSpec [] [] (PreCommitHooksConfig False) Rust [])
        `shouldBe` [nix|nixpkgs.mkShell {
  buildInputs = [
    # Insert any dependencies that should exist in the dev shell environment here
  ];
  shellHook = ''
    export RUST_SRC_PATH=${nixpkgs.rustPlatform.rustLibSrc}
  '';
}|]
    it "gives a proper shell hook for Rust projects with pre-commit hooks" do
      mkShell (BuildInputSpec [] [] (PreCommitHooksConfig True) Rust [])
        `shouldBe` [nix|nixpkgs.mkShell {
  buildInputs = preCommitHooks.tools;
  shellHook = ''
    export RUST_SRC_PATH=${nixpkgs.rustPlatform.rustLibSrc}
    ${preCommitHooks.allHooks.shellHook}
  '';
}|]
