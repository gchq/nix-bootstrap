{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.FlakeNix (flakeNixFor) where

import Bootstrap.Cli (RunConfig (RunConfig, rcUseFlakes))
import Bootstrap.Data.Bootstrappable
  ( Bootstrappable
      ( bootstrapContent,
        bootstrapName,
        bootstrapReason
      ),
    bootstrapContentNix,
  )
import Bootstrap.Data.Bootstrappable.BuildNix (BuildNix)
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig (NixPreCommitHookConfig)
import Bootstrap.Data.Bootstrappable.NixShell
  ( NixShell
      ( NixShell,
        nixShellExtraBindings,
        nixShellHooksRequireNixpkgs,
        nixShellNixpkgsBuildInputs,
        nixShellNixpkgsNativeBuildInputs,
        nixShellOtherBuildInputs
      ),
    nixShellFor,
  )
import Bootstrap.Data.PreCommitHook
  ( PreCommitHooksConfig (unPreCommitHooksConfig),
  )
import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Data.ProjectType (ProjectType (Python))
import Bootstrap.Nix.Expr
  ( Binding,
    Expr (EGrouping, ELetIn, ELit, ESet),
    FunctionArgs (FASet),
    IsNixExpr (toNixExpr),
    Literal (LPath, LString),
    nix,
    nixargs,
    nixbinding,
    nixident,
    nixproperty,
    (|*),
    (|:),
    (|=),
  )
import Bootstrap.Nix.Expr.BuildInputs (BuildInputSpec (bisNativeNixpkgsPackages, bisOtherPackages))
import Bootstrap.Nix.Expr.MkShell
  ( BuildInputSpec
      ( BuildInputSpec,
        bisNixpkgsPackages,
        bisPreCommitHooksConfig,
        bisProjectType
      ),
    mkShell,
  )
import Bootstrap.Nix.Expr.PreCommitHooks (ImportPreCommitHooksArgs (ImportPreCommitHooksArgs, passNixpkgsThrough, passSystemThrough), importPreCommitHooks)
import Bootstrap.Nix.Expr.Python (machNixFlakeInput, pythonPackagesBinding)
import Control.Lens
  ( Field2 (_2),
    filtered,
    folded,
    (^..),
  )

data FlakeNix = FlakeNix
  { flakeNixPreCommitHooksConfig :: PreCommitHooksConfig,
    flakeNixProjectName :: ProjectName,
    flakeNixProjectType :: ProjectType,
    flakeNixExtraBindings :: [Binding],
    flakeNixNixpkgsBuildInputs :: [Expr],
    flakeNixOtherBuildInputs :: [Expr],
    flakeNixNixpkgsNativeBuildInputs :: [Expr],
    flakeNixBuildNix :: Maybe BuildNix,
    -- | Used to determine whether to pass the nixpkgs argument to the pre-commit-hooks config
    flakeNixHooksRequireNixpkgs :: Bool
  }

instance Bootstrappable FlakeNix where
  bootstrapName = const "flake.nix"
  bootstrapReason FlakeNix {flakeNixPreCommitHooksConfig} =
    "This configures what tools are available in your development environment"
      <> if unPreCommitHooksConfig flakeNixPreCommitHooksConfig
        then " and links in the pre-commit hooks."
        else "."
  bootstrapContent = bootstrapContentNix

flakeNixFor ::
  RunConfig ->
  ProjectName ->
  ProjectType ->
  PreCommitHooksConfig ->
  Maybe NixPreCommitHookConfig ->
  Maybe BuildNix ->
  Maybe FlakeNix
flakeNixFor
  rc@RunConfig {rcUseFlakes}
  flakeNixProjectName
  flakeNixProjectType
  flakeNixPreCommitHooksConfig
  nixPreCommitHookConfig
  flakeNixBuildNix =
    let NixShell
          { nixShellExtraBindings,
            nixShellNixpkgsBuildInputs,
            nixShellOtherBuildInputs,
            nixShellNixpkgsNativeBuildInputs,
            nixShellHooksRequireNixpkgs
          } =
            nixShellFor rc flakeNixProjectType flakeNixPreCommitHooksConfig nixPreCommitHookConfig
     in if rcUseFlakes
          then
            Just
              FlakeNix
                { flakeNixExtraBindings = (nixShellExtraBindings ^.. folded . filtered fst . _2) <> extraBindings,
                  flakeNixNixpkgsBuildInputs = nixShellNixpkgsBuildInputs,
                  flakeNixOtherBuildInputs = nixShellOtherBuildInputs,
                  flakeNixNixpkgsNativeBuildInputs = nixShellNixpkgsNativeBuildInputs,
                  flakeNixHooksRequireNixpkgs = nixShellHooksRequireNixpkgs,
                  ..
                }
          else Nothing
    where
      extraBindings :: [Binding]
      extraBindings = [pythonPackagesBinding True | case flakeNixProjectType of Python _ -> True; _ -> False]

instance IsNixExpr FlakeNix where
  toNixExpr FlakeNix {..} =
    let usingHooks :: Bool
        usingHooks = unPreCommitHooksConfig flakeNixPreCommitHooksConfig
        isPython :: Bool
        isPython = case flakeNixProjectType of Python _ -> True; _ -> False
     in ESet
          False
          [ [nixproperty|description|]
              |= ( ELit
                     . LString
                     $ "Development infrastructure for " <> unProjectName flakeNixProjectName
                 ),
            [nixproperty|inputs|]
              |= ESet
                False
                ( [ [nixbinding|nixpkgs-src.url = "github:NixOS/nixpkgs";|],
                    [nixbinding|flake-compat = {
                      flake = false;
                      url = github:edolstra/flake-compat;
                    };|],
                    [nixbinding|flake-utils.url = "github:numtide/flake-utils";|]
                  ]
                    <> [ [nixbinding|pre-commit-hooks-lib = {
                            inputs.flake-utils.follows = "flake-utils";
                            url = "github:cachix/pre-commit-hooks.nix";
                          };|]
                         | usingHooks
                       ]
                    <> [machNixFlakeInput | isPython]
                ),
            [nixproperty|outputs|]
              |= FASet
                ( [nixident|nixpkgs-src|]
                    :| ( [nixident|flake-utils|] :
                         [[nixident|mach-nix|] | isPython]
                           <> [[nixident|pre-commit-hooks-lib|] | usingHooks]
                           <> [[nixident|self|]]
                           <> [[nixident|...|]]
                       )
                )
                |: [nix|flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux])|]
                |* EGrouping
                  ( [nixargs|system:|]
                      |: ELetIn
                        ( [nixbinding|nixpkgs = nixpkgs-src.legacyPackages.${system};|]
                            :| [ [nixproperty|preCommitHooks|]
                                   |= importPreCommitHooks
                                     ImportPreCommitHooksArgs
                                       { passNixpkgsThrough = flakeNixHooksRequireNixpkgs,
                                         passSystemThrough = True
                                       }
                                 | usingHooks
                               ]
                            <> flakeNixExtraBindings
                        )
                        ( ESet False $
                            [[nixbinding|checks.pre-commit-check = preCommitHooks.hooks;|] | usingHooks]
                              <> [ [nixbinding|devShell = self.devShells.${system}.default;|],
                                   [nixproperty|devShells.default|]
                                     |= mkShell
                                       BuildInputSpec
                                         { bisNixpkgsPackages = flakeNixNixpkgsBuildInputs,
                                           bisOtherPackages = flakeNixOtherBuildInputs,
                                           bisPreCommitHooksConfig = flakeNixPreCommitHooksConfig,
                                           bisProjectType = flakeNixProjectType,
                                           bisNativeNixpkgsPackages = flakeNixNixpkgsNativeBuildInputs
                                         }
                                 ]
                              <> case flakeNixBuildNix of
                                Just buildNix ->
                                  [ [nixbinding|defaultPackage = self.packages.${system}.default;|],
                                    [nixproperty|packages.default|]
                                      |= ( [nix|import|]
                                             |* ELit (LPath . toText $ bootstrapName buildNix)
                                             |* [nix|{ inherit nixpkgs; }|]
                                         )
                                  ]
                                    <> (if usingHooks then runChecksDerivation else [])
                                Nothing -> if usingHooks then runChecksDerivation else []
                        )
                  )
          ]
    where
      runChecksDerivation :: [Binding]
      runChecksDerivation =
        [ [nixbinding|# runChecks is a hack required to allow checks to run on a single system|],
          [nixbinding|# when using Import from Deviation (https://discourse.nixos.org/t/nix-flake-check-for-current-system-only/18366)|],
          [nixbinding|# Building it is the single-system equivalent of running "nix flake check".|],
          [nixbinding|packages.runChecks = nixpkgs.runCommand "run-checks" {
            currentSystemChecks = builtins.attrValues self.checks.${system};
          } "echo $currentSystemChecks; touch $out";|]
        ]
