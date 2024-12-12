{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.FlakeNixSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.BuildNix (buildNixFor)
import Bootstrap.Data.Bootstrappable.FlakeNix (flakeNixFor)
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig (nixPreCommitHookConfigFor)
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (PreCommitHooksConfig))
import Bootstrap.Data.ProjectName (mkProjectName)
import Bootstrap.Data.ProjectType
  ( NodePackageManager (NPM, PNPm),
    ProjectType (Go, Node, Python, Rust),
    PythonVersion (Python39),
    SetUpGoBuild (SetUpGoBuild),
  )
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "flake.nix rendering" do
  let projectName = Unsafe.fromJust $ mkProjectName "test-project"
  it "renders correctly without pre-commit hooks" do
    bootstrapContent (flakeNixFor projectName (Node NPM) (PreCommitHooksConfig False) Nothing Nothing)
      >>= ( `shouldBe`
              Right
                [r|{
  description = "Development infrastructure for test-project";
  inputs = {
    nixpkgs-src.url = "github:NixOS/nixpkgs";
    flake-compat = {
      flake = false;
      url = github:edolstra/flake-compat;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    nixpkgs-src,
    flake-utils,
    self,
    ...
  }:
    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux]) (system: let
      nixpkgs = nixpkgs-src.legacyPackages.${system};
    in {
      devShell = self.devShells.${system}.default;
      devShells.default = nixpkgs.mkShell {
        buildInputs = with nixpkgs; [awscli2 nodejs];
      };
    });
}
|]
          )
  it "orders build inputs correctly when some use property access" do
    bootstrapContent (flakeNixFor projectName (Node PNPm) (PreCommitHooksConfig False) Nothing Nothing)
      >>= ( `shouldBe`
              Right
                [r|{
  description = "Development infrastructure for test-project";
  inputs = {
    nixpkgs-src.url = "github:NixOS/nixpkgs";
    flake-compat = {
      flake = false;
      url = github:edolstra/flake-compat;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    nixpkgs-src,
    flake-utils,
    self,
    ...
  }:
    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux]) (system: let
      nixpkgs = nixpkgs-src.legacyPackages.${system};
    in {
      devShell = self.devShells.${system}.default;
      devShells.default = nixpkgs.mkShell {
        buildInputs = with nixpkgs; [
          awscli2
          nodePackages.pnpm
          nodejs
        ];
      };
    });
}
|]
          )
  it "renders correctly with pre-commit hooks" do
    bootstrapContent
      ( flakeNixFor
          projectName
          (Node NPM)
          (PreCommitHooksConfig True)
          (Just . nixPreCommitHookConfigFor $ Node NPM)
          Nothing
      )
      >>= ( `shouldBe`
              Right
                [r|{
  description = "Development infrastructure for test-project";
  inputs = {
    nixpkgs-src.url = "github:NixOS/nixpkgs";
    flake-compat = {
      flake = false;
      url = github:edolstra/flake-compat;
    };
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks-lib.url = "github:cachix/pre-commit-hooks.nix";
  };
  outputs = {
    nixpkgs-src,
    flake-utils,
    pre-commit-hooks-lib,
    self,
    ...
  }:
    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux]) (system: let
      nixpkgs = nixpkgs-src.legacyPackages.${system};
      preCommitHooks = import nix/pre-commit-hooks.nix {
        inherit pre-commit-hooks-lib system;
      };
    in {
      checks.pre-commit-check = preCommitHooks.pureHooks;
      devShell = self.devShells.${system}.default;
      devShells.default = nixpkgs.mkShell {
        buildInputs = preCommitHooks.tools ++ (with nixpkgs; [awscli2 nodejs]);
        inherit (preCommitHooks.allHooks) shellHook;
      };
      # runChecks is a hack required to allow checks to run on a single system
      # when using Import from Deviation (https://discourse.nixos.org/t/nix-flake-check-for-current-system-only/18366)
      # Building it is the single-system equivalent of running "nix flake check".
      packages.runChecks = nixpkgs.runCommand "run-checks" {
        currentSystemChecks = builtins.attrValues self.checks.${system};
      } "echo $currentSystemChecks; touch $out";
    });
}
|]
          )
  it "renders correctly with a Go build" do
    bootstrapContent
      ( flakeNixFor
          projectName
          (Go $ SetUpGoBuild True)
          (PreCommitHooksConfig True)
          (Just . nixPreCommitHookConfigFor . Go $ SetUpGoBuild True)
          (buildNixFor projectName (Go $ SetUpGoBuild True))
      )
      >>= ( `shouldBe`
              Right
                [r|{
  description = "Development infrastructure for test-project";
  inputs = {
    nixpkgs-src.url = "github:NixOS/nixpkgs";
    flake-compat = {
      flake = false;
      url = github:edolstra/flake-compat;
    };
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks-lib.url = "github:cachix/pre-commit-hooks.nix";
  };
  outputs = {
    nixpkgs-src,
    flake-utils,
    pre-commit-hooks-lib,
    self,
    ...
  }:
    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux]) (system: let
      nixpkgs = nixpkgs-src.legacyPackages.${system};
      preCommitHooks = import nix/pre-commit-hooks.nix {
        inherit pre-commit-hooks-lib nixpkgs system;
      };
    in {
      checks.pre-commit-check = preCommitHooks.pureHooks;
      devShell = self.devShells.${system}.default;
      devShells.default = nixpkgs.mkShell {
        buildInputs = preCommitHooks.tools ++ (with nixpkgs; [go]);
        inherit (preCommitHooks.allHooks) shellHook;
      };
      defaultPackage = self.packages.${system}.default;
      packages.default = import nix/build.nix {
        inherit nixpkgs;
      };
      # runChecks is a hack required to allow checks to run on a single system
      # when using Import from Deviation (https://discourse.nixos.org/t/nix-flake-check-for-current-system-only/18366)
      # Building it is the single-system equivalent of running "nix flake check".
      packages.runChecks = nixpkgs.runCommand "run-checks" {
        currentSystemChecks = builtins.attrValues self.checks.${system};
      } "echo $currentSystemChecks; touch $out";
    });
}
|]
          )
  it "renders correctly with a Python project" do
    bootstrapContent (flakeNixFor projectName (Python Python39) (PreCommitHooksConfig False) Nothing Nothing)
      >>= ( `shouldBe`
              Right
                [r|{
  description = "Development infrastructure for test-project";
  inputs = {
    nixpkgs-src.url = "github:NixOS/nixpkgs";
    flake-compat = {
      flake = false;
      url = github:edolstra/flake-compat;
    };
    flake-utils.url = "github:numtide/flake-utils";
    mach-nix.url = "github:DavHau/mach-nix?ref=3.5.0";
  };
  outputs = {
    nixpkgs-src,
    flake-utils,
    mach-nix,
    self,
    ...
  }:
    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux]) (system: let
      nixpkgs = nixpkgs-src.legacyPackages.${system};
      pythonPackages = mach-nix.lib.${system}.mkPython rec {
        requirements = builtins.readFile ./requirements.txt;
        python = "python39";
      };
    in {
      devShell = self.devShells.${system}.default;
      devShells.default = nixpkgs.mkShell {
        buildInputs = [pythonPackages];
      };
    });
}
|]
          )
  it "renders correctly with a Rust project" do
    bootstrapContent
      ( flakeNixFor
          projectName
          Rust
          (PreCommitHooksConfig True)
          Nothing
          (buildNixFor projectName Rust)
      )
      >>= ( `shouldBe`
              Right
                [r|{
  description = "Development infrastructure for test-project";
  inputs = {
    nixpkgs-src.url = "github:NixOS/nixpkgs";
    flake-compat = {
      flake = false;
      url = github:edolstra/flake-compat;
    };
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks-lib.url = "github:cachix/pre-commit-hooks.nix";
  };
  outputs = {
    nixpkgs-src,
    flake-utils,
    pre-commit-hooks-lib,
    self,
    ...
  }:
    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux]) (system: let
      nixpkgs = nixpkgs-src.legacyPackages.${system};
      preCommitHooks = import nix/pre-commit-hooks.nix {
        inherit pre-commit-hooks-lib system;
      };
    in {
      checks.pre-commit-check = preCommitHooks.pureHooks;
      devShell = self.devShells.${system}.default;
      devShells.default = nixpkgs.mkShell {
        buildInputs = preCommitHooks.tools ++ (with nixpkgs; [libiconv]);
        nativeBuildInputs = with nixpkgs; [
          cargo
          rust-analyzer
          rustc
        ];
        shellHook = ''
          export RUST_SRC_PATH=${nixpkgs.rustPlatform.rustLibSrc}
          ${preCommitHooks.allHooks.shellHook}
        '';
      };
      defaultPackage = self.packages.${system}.default;
      packages.default = import nix/build.nix {
        inherit nixpkgs;
      };
      # runChecks is a hack required to allow checks to run on a single system
      # when using Import from Deviation (https://discourse.nixos.org/t/nix-flake-check-for-current-system-only/18366)
      # Building it is the single-system equivalent of running "nix flake check".
      packages.runChecks = nixpkgs.runCommand "run-checks" {
        currentSystemChecks = builtins.attrValues self.checks.${system};
      } "echo $currentSystemChecks; touch $out";
    });
}
|]
          )
