{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.NixShellSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig (nixPreCommitHookConfigFor)
import Bootstrap.Data.Bootstrappable.NixShell (nixShellFor)
import Bootstrap.Data.GHCVersion (GHCVersion (GHCVersion))
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (PreCommitHooksConfig))
import Bootstrap.Data.ProjectType (HaskellOptions (HaskellOptions), HaskellProjectType (HaskellProjectTypeBasic), NodePackageManager (NPM, PNPm), ProjectType (Go, Haskell, Node, Python, Rust), PythonVersion (Python39), SetUpGoBuild (SetUpGoBuild), SetUpHaskellBuild (SetUpHaskellBuild))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Util.RunConfig (rcDefault)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "shell.nix rendering" do
  it "renders correctly without pre-commit hooks" do
    bootstrapContent (nixShellFor rcDefault (Node NPM) (PreCommitHooksConfig False) Nothing)
      >>= ( `shouldBe`
              Right
                [r|let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};
in
  nixpkgs.mkShell {
    buildInputs = with nixpkgs; [
      awscli2
      niv
      nodejs
    ];
  }
|]
          )
  it "correctly sorts build inputs when some use property access" do
    bootstrapContent (nixShellFor rcDefault (Node PNPm) (PreCommitHooksConfig False) Nothing)
      >>= ( `shouldBe`
              Right
                [r|let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};
in
  nixpkgs.mkShell {
    buildInputs = with nixpkgs; [
      awscli2
      niv
      nodePackages.pnpm
      nodejs
    ];
  }
|]
          )
  it "renders correctly with pre-commit hooks" do
    let t = Go $ SetUpGoBuild False
    bootstrapContent (nixShellFor rcDefault t (PreCommitHooksConfig True) (Just $ nixPreCommitHookConfigFor rcDefault t))
      >>= ( `shouldBe`
              Right
                [r|let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};
  pre-commit-hooks-lib = import sources.pre-commit-hooks;
  preCommitHooks = import nix/pre-commit-hooks.nix {
    inherit pre-commit-hooks-lib nixpkgs;
  };
in
  nixpkgs.mkShell {
    buildInputs = preCommitHooks.tools ++ (with nixpkgs; [go niv]);
    inherit (preCommitHooks.allHooks) shellHook;
  }
|]
          )
  it "renders a Haskell project correctly" do
    bootstrapContent
      ( nixShellFor
          rcDefault
          (Haskell $ HaskellOptions (GHCVersion 9 0 2) (HaskellProjectTypeBasic $ SetUpHaskellBuild True))
          (PreCommitHooksConfig False)
          Nothing
      )
      >>= ( `shouldBe`
              Right
                [r|let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};
  haskellPackages = import nix/haskell-packages.nix {
    inherit nixpkgs;
  };
  haskellEnv = haskellPackages.ghcWithPackages (pkgs: with pkgs; [cabal-install haskell-language-server]);
in
  nixpkgs.mkShell {
    buildInputs = [haskellEnv] ++ (with nixpkgs; [niv]);
  }
|]
          )
  it "renders a python project correctly" do
    bootstrapContent (nixShellFor rcDefault (Python Python39) (PreCommitHooksConfig False) Nothing)
      >>= ( `shouldBe`
              Right
                [r|let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};
  mach-nix = import (builtins.fetchGit {
    url = "https://github.com/DavHau/mach-nix";
    ref = "refs/tags/3.5.0";
  }) {};
  pythonPackages = mach-nix.mkPython rec {
    requirements = builtins.readFile ./requirements.txt;
    python = "python39";
  };
in
  nixpkgs.mkShell {
    buildInputs = [pythonPackages] ++ (with nixpkgs; [niv]);
  }
|]
          )
  it "renders a python project correctly with pre commit-hooks" do
    let t = Python Python39
    bootstrapContent (nixShellFor rcDefault t (PreCommitHooksConfig True) (Just $ nixPreCommitHookConfigFor rcDefault t))
      >>= ( `shouldBe`
              Right
                [r|let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};
  pre-commit-hooks-lib = import sources.pre-commit-hooks;
  preCommitHooks = import nix/pre-commit-hooks.nix {
    inherit pre-commit-hooks-lib;
  };
  mach-nix = import (builtins.fetchGit {
    url = "https://github.com/DavHau/mach-nix";
    ref = "refs/tags/3.5.0";
  }) {};
  pythonPackages = mach-nix.mkPython rec {
    requirements = builtins.readFile ./requirements.txt;
    python = "python39";
  };
in
  nixpkgs.mkShell {
    buildInputs = preCommitHooks.tools ++ [pythonPackages] ++ (with nixpkgs; [niv]);
    inherit (preCommitHooks.allHooks) shellHook;
  }
|]
          )
  it "renders a Rust project correctly" do
    let t = Rust
    bootstrapContent (nixShellFor rcDefault t (PreCommitHooksConfig False) (Just $ nixPreCommitHookConfigFor rcDefault t))
      >>= ( `shouldBe`
              Right
                [r|let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};
in
  nixpkgs.mkShell {
    buildInputs = with nixpkgs; [libiconv niv];
    nativeBuildInputs = with nixpkgs; [
      cargo
      rust-analyzer
      rustc
    ];
    shellHook = ''
      export RUST_SRC_PATH=${nixpkgs.rustPlatform.rustLibSrc}
    '';
  }
|]
          )
