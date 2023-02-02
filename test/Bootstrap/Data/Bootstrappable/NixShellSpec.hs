-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.NixShellSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig (nixPreCommitHookConfigFor)
import Bootstrap.Data.Bootstrappable.NixShell (nixShellFor)
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (PreCommitHooksConfig))
import Bootstrap.Data.ProjectType
  ( NodePackageManager (NPM, PNPm),
    ProjectType (Go, Node, Python),
    PythonVersion (Python39),
    SetUpGoBuild (SetUpGoBuild),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Util.RunConfig (rcDefault)

spec :: Spec
spec = describe "shell.nix rendering" do
  it "renders correctly without pre-commit hooks" do
    bootstrapContent (nixShellFor rcDefault (Node NPM) (PreCommitHooksConfig False) Nothing)
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "let",
                      "  sources = import nix/sources.nix;",
                      "  nixpkgs = import sources.nixpkgs {};",
                      "in",
                      "  nixpkgs.mkShell {",
                      "    buildInputs = with nixpkgs; [",
                      "      awscli2",
                      "      niv",
                      "      nodejs",
                      "      rnix-lsp",
                      "    ];",
                      "  }"
                    ]
                )
          )
  it "correctly sorts build inputs when some use property access" do
    bootstrapContent (nixShellFor rcDefault (Node PNPm) (PreCommitHooksConfig False) Nothing)
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "let",
                      "  sources = import nix/sources.nix;",
                      "  nixpkgs = import sources.nixpkgs {};",
                      "in",
                      "  nixpkgs.mkShell {",
                      "    buildInputs = with nixpkgs; [",
                      "      awscli2",
                      "      niv",
                      "      nodePackages.pnpm",
                      "      nodejs",
                      "      rnix-lsp",
                      "    ];",
                      "  }"
                    ]
                )
          )
  it "renders correctly with pre-commit hooks" do
    let t = Go $ SetUpGoBuild False
    bootstrapContent (nixShellFor rcDefault t (PreCommitHooksConfig True) (Just $ nixPreCommitHookConfigFor rcDefault t))
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "let",
                      "  sources = import nix/sources.nix;",
                      "  nixpkgs = import sources.nixpkgs {};",
                      "  pre-commit-hooks-lib = import sources.pre-commit-hooks;",
                      "  preCommitHooks = import nix/pre-commit-hooks.nix {",
                      "    inherit pre-commit-hooks-lib nixpkgs;",
                      "  };",
                      "in",
                      "  nixpkgs.mkShell {",
                      "    inherit (preCommitHooks.hooks) shellHook;",
                      "    buildInputs =",
                      "      preCommitHooks.tools",
                      "      ++ (with nixpkgs; [",
                      "        go",
                      "        niv",
                      "        rnix-lsp",
                      "      ]);",
                      "  }"
                    ]
                )
          )
  it "renders a python project correctly" do
    bootstrapContent (nixShellFor rcDefault (Python Python39) (PreCommitHooksConfig False) Nothing)
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "let",
                      "  sources = import nix/sources.nix;",
                      "  nixpkgs = import sources.nixpkgs {};",
                      "  mach-nix = import (builtins.fetchGit {",
                      "    url = \"https://github.com/DavHau/mach-nix\";",
                      "    ref = \"refs/tags/3.5.0\";",
                      "  }) {};",
                      "  pythonPackages = mach-nix.mkPython rec {",
                      "    requirements = builtins.readFile ./requirements.txt;",
                      "    python = \"python39\";",
                      "  };",
                      "in",
                      "  nixpkgs.mkShell {",
                      "    buildInputs = [pythonPackages] ++ (with nixpkgs; [niv rnix-lsp]);",
                      "  }"
                    ]
                )
          )
  it "renders a python project correctly with pre commit-hooks" do
    let t = Python Python39
    bootstrapContent (nixShellFor rcDefault t (PreCommitHooksConfig True) (Just $ nixPreCommitHookConfigFor rcDefault t))
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "let",
                      "  sources = import nix/sources.nix;",
                      "  nixpkgs = import sources.nixpkgs {};",
                      "  pre-commit-hooks-lib = import sources.pre-commit-hooks;",
                      "  preCommitHooks = import nix/pre-commit-hooks.nix {",
                      "    inherit pre-commit-hooks-lib;",
                      "  };",
                      "  mach-nix = import (builtins.fetchGit {",
                      "    url = \"https://github.com/DavHau/mach-nix\";",
                      "    ref = \"refs/tags/3.5.0\";",
                      "  }) {};",
                      "  pythonPackages = mach-nix.mkPython rec {",
                      "    requirements = builtins.readFile ./requirements.txt;",
                      "    python = \"python39\";",
                      "  };",
                      "in",
                      "  nixpkgs.mkShell {",
                      "    inherit (preCommitHooks.hooks) shellHook;",
                      "    buildInputs = preCommitHooks.tools ++ [pythonPackages] ++ (with nixpkgs; [niv rnix-lsp]);",
                      "  }"
                    ]
                )
          )
