-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.FlakeNixSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.FlakeNix (flakeNixFor)
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig (nixPreCommitHookConfigFor)
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (PreCommitHooksConfig))
import Bootstrap.Data.ProjectName (mkProjectName)
import Bootstrap.Data.ProjectType
  ( NodePackageManager (NPM, PNPm),
    ProjectType (Go, Node, Python),
    PythonVersion (Python39),
    SetUpGoBuild (SetUpGoBuild),
  )
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Util.RunConfig (rcWithFlakes)

spec :: Spec
spec = describe "flake.nix rendering" do
  let projectName = Unsafe.fromJust $ mkProjectName "test-project"
  it "renders correctly without pre-commit hooks" do
    bootstrapContent (flakeNixFor rcWithFlakes projectName (Node NPM) (PreCommitHooksConfig False) Nothing)
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "{",
                      "  description = \"Development infrastructure for test-project\";",
                      "  inputs = {",
                      "    nixpkgs-src.url = \"github:NixOS/nixpkgs\";",
                      "    flake-utils.url = \"github:numtide/flake-utils\";",
                      "  };",
                      "  outputs = {",
                      "    nixpkgs-src,",
                      "    flake-utils,",
                      "    self,",
                      "  }:",
                      "    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux]) (system: let",
                      "      nixpkgs = nixpkgs-src.legacyPackages.${system};",
                      "    in {",
                      "      devShell = self.devShells.${system}.default;",
                      "      devShells.default = nixpkgs.mkShell {",
                      "        buildInputs = with nixpkgs; [",
                      "          awscli2",
                      "          nodejs",
                      "          rnix-lsp",
                      "        ];",
                      "      };",
                      "    });",
                      "}"
                    ]
                )
          )
  it "orders build inputs correctly when some use property access" do
    bootstrapContent (flakeNixFor rcWithFlakes projectName (Node PNPm) (PreCommitHooksConfig False) Nothing)
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "{",
                      "  description = \"Development infrastructure for test-project\";",
                      "  inputs = {",
                      "    nixpkgs-src.url = \"github:NixOS/nixpkgs\";",
                      "    flake-utils.url = \"github:numtide/flake-utils\";",
                      "  };",
                      "  outputs = {",
                      "    nixpkgs-src,",
                      "    flake-utils,",
                      "    self,",
                      "  }:",
                      "    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux]) (system: let",
                      "      nixpkgs = nixpkgs-src.legacyPackages.${system};",
                      "    in {",
                      "      devShell = self.devShells.${system}.default;",
                      "      devShells.default = nixpkgs.mkShell {",
                      "        buildInputs = with nixpkgs; [",
                      "          awscli2",
                      "          nodePackages.pnpm",
                      "          nodejs",
                      "          rnix-lsp",
                      "        ];",
                      "      };",
                      "    });",
                      "}"
                    ]
                )
          )
  it "renders correctly with pre-commit hooks" do
    bootstrapContent
      ( flakeNixFor
          rcWithFlakes
          projectName
          (Node NPM)
          (PreCommitHooksConfig True)
          (Just . nixPreCommitHookConfigFor rcWithFlakes $ Node NPM)
      )
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "{",
                      "  description = \"Development infrastructure for test-project\";",
                      "  inputs = {",
                      "    nixpkgs-src.url = \"github:NixOS/nixpkgs\";",
                      "    flake-utils.url = \"github:numtide/flake-utils\";",
                      "    pre-commit-hooks-lib = {",
                      "      inputs.flake-utils.follows = \"flake-utils\";",
                      "      url = \"github:cachix/pre-commit-hooks.nix\";",
                      "    };",
                      "  };",
                      "  outputs = {",
                      "    nixpkgs-src,",
                      "    flake-utils,",
                      "    pre-commit-hooks-lib,",
                      "    self,",
                      "  }:",
                      "    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux]) (system: let",
                      "      nixpkgs = nixpkgs-src.legacyPackages.${system};",
                      "      preCommitHooks = import nix/pre-commit-hooks.nix {",
                      "        inherit pre-commit-hooks-lib system;",
                      "      };",
                      "    in {",
                      "      checks.pre-commit-check = preCommitHooks.hooks;",
                      "      devShell = self.devShells.${system}.default;",
                      "      devShells.default = nixpkgs.mkShell {",
                      "        inherit (preCommitHooks.hooks) shellHook;",
                      "        buildInputs =",
                      "          preCommitHooks.tools",
                      "          ++ (with nixpkgs; [",
                      "            awscli2",
                      "            nodejs",
                      "            rnix-lsp",
                      "          ]);",
                      "      };",
                      "      # runChecks is a hack required to allow checks to run on a single system",
                      "      # when using Import from Deviation (https://discourse.nixos.org/t/nix-flake-check-for-current-system-only/18366)",
                      "      # Building it is the single-system equivalent of running \"nix flake check\".",
                      "      packages.runChecks = nixpkgs.runCommand \"run-checks\" {",
                      "        currentSystemChecks = builtins.attrValues self.checks.${system};",
                      "      } \"echo $currentSystemChecks; touch $out\";",
                      "    });",
                      "}"
                    ]
                )
          )
  it "renders correctly with a Go build" do
    bootstrapContent
      ( flakeNixFor
          rcWithFlakes
          projectName
          (Go $ SetUpGoBuild True)
          (PreCommitHooksConfig True)
          (Just . nixPreCommitHookConfigFor rcWithFlakes . Go $ SetUpGoBuild True)
      )
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "{",
                      "  description = \"Development infrastructure for test-project\";",
                      "  inputs = {",
                      "    nixpkgs-src.url = \"github:NixOS/nixpkgs\";",
                      "    flake-utils.url = \"github:numtide/flake-utils\";",
                      "    pre-commit-hooks-lib = {",
                      "      inputs.flake-utils.follows = \"flake-utils\";",
                      "      url = \"github:cachix/pre-commit-hooks.nix\";",
                      "    };",
                      "  };",
                      "  outputs = {",
                      "    nixpkgs-src,",
                      "    flake-utils,",
                      "    pre-commit-hooks-lib,",
                      "    self,",
                      "  }:",
                      "    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux]) (system: let",
                      "      nixpkgs = nixpkgs-src.legacyPackages.${system};",
                      "      preCommitHooks = import nix/pre-commit-hooks.nix {",
                      "        inherit pre-commit-hooks-lib nixpkgs system;",
                      "      };",
                      "    in {",
                      "      checks.pre-commit-check = preCommitHooks.hooks;",
                      "      devShell = self.devShells.${system}.default;",
                      "      devShells.default = nixpkgs.mkShell {",
                      "        inherit (preCommitHooks.hooks) shellHook;",
                      "        buildInputs = preCommitHooks.tools ++ (with nixpkgs; [go rnix-lsp]);",
                      "      };",
                      "      defaultPackage = self.packages.${system}.default;",
                      "      packages.default = nixpkgs.buildGoModule {",
                      "        pname = \"test-project\";",
                      "        version = \"0.1.0\";",
                      "        src = ./.;",
                      "        vendorSha256 = null;",
                      "        # Swap out the line above for the one below once you start adding dependencies.",
                      "        # After your dependencies change, builds will fail until you update the hash below.",
                      "        # When the build fails, it will tell you what the expected hash is.",
                      "        # vendorSha256 = \"sha256-00000000000000000000000000000000000000000000\";",
                      "      };",
                      "      # runChecks is a hack required to allow checks to run on a single system",
                      "      # when using Import from Deviation (https://discourse.nixos.org/t/nix-flake-check-for-current-system-only/18366)",
                      "      # Building it is the single-system equivalent of running \"nix flake check\".",
                      "      packages.runChecks = nixpkgs.runCommand \"run-checks\" {",
                      "        currentSystemChecks = builtins.attrValues self.checks.${system};",
                      "      } \"echo $currentSystemChecks; touch $out\";",
                      "    });",
                      "}"
                    ]
                )
          )
  it "renders correctly with a Python project" do
    bootstrapContent (flakeNixFor rcWithFlakes projectName (Python Python39) (PreCommitHooksConfig False) Nothing)
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "{",
                      "  description = \"Development infrastructure for test-project\";",
                      "  inputs = {",
                      "    nixpkgs-src.url = \"github:NixOS/nixpkgs\";",
                      "    flake-utils.url = \"github:numtide/flake-utils\";",
                      "    mach-nix.url = \"github:DavHau/mach-nix?ref=3.5.0\";",
                      "  };",
                      "  outputs = {",
                      "    nixpkgs-src,",
                      "    flake-utils,",
                      "    mach-nix,",
                      "    self,",
                      "  }:",
                      "    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux]) (system: let",
                      "      nixpkgs = nixpkgs-src.legacyPackages.${system};",
                      "      pythonPackages = mach-nix.lib.${system}.mkPython rec {",
                      "        requirements = builtins.readFile ./requirements.txt;",
                      "        python = \"python39\";",
                      "      };",
                      "    in {",
                      "      devShell = self.devShells.${system}.default;",
                      "      devShells.default = nixpkgs.mkShell {",
                      "        buildInputs = [pythonPackages] ++ (with nixpkgs; [rnix-lsp]);",
                      "      };",
                      "    });",
                      "}"
                    ]
                )
          )
