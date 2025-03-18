{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.FlakeNix (flakeNixFor) where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable
      ( bootstrapContent,
        bootstrapName,
        bootstrapReason
      ),
    bootstrapContentNix,
  )
import Bootstrap.Data.Bootstrappable.BuildNix (BuildNix (unBuildNix))
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig (NixPreCommitHookConfig (nixPreCommitHookConfigRequiresNixpkgs))
import Bootstrap.Data.PreCommitHook
  ( PreCommitHooksConfig (unPreCommitHooksConfig),
  )
import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeBare, ElmModeNode),
    ElmOptions (elmOptionElmMode),
    HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly),
    InstallMinishift (unInstallMinishift),
    JavaOptions (JavaOptions),
    JdkPackage (GraalVM, OpenJDK),
    NodePackageManager (NPM, PNPm, Yarn),
    ProjectType
      ( Elm,
        Go,
        Haskell,
        Java,
        Minimal,
        Node,
        Python,
        Rust
      ),
  )
import Bootstrap.Nix.Expr
  ( Binding (BInherit),
    Expr (EApplication, EFunc, EGrouping, EIdent, ELetIn, EList, ELit, EPropertyAccess, ESet, EWith),
    FunctionArgs (FASet),
    Identifier,
    IsNixExpr (toNixExpr),
    Literal (LPath, LString),
    nix,
    nixargs,
    nixbinding,
    nixident,
    nixproperty,
    unsafeSimplifyBindings,
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
import Bootstrap.Nix.Expr.ReproducibleBuild (ReproducibleBuildExpr (rbeRequirements), ReproducibleBuildRequirement (RBRHaskellPackages, RBRNixpkgs), reproducibleBuildRequirementIdentifier, sortRbeRequirements)

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
  ProjectName ->
  ProjectType ->
  PreCommitHooksConfig ->
  Maybe NixPreCommitHookConfig ->
  Maybe BuildNix ->
  Maybe FlakeNix
flakeNixFor
  flakeNixProjectName
  flakeNixProjectType
  flakeNixPreCommitHooksConfig
  nixPreCommitHookConfig
  flakeNixBuildNix =
    Just
      FlakeNix
        { flakeNixExtraBindings = extraBindingsFor flakeNixProjectType,
          flakeNixNixpkgsBuildInputs = nixpkgsBuildInputsFor flakeNixProjectType,
          flakeNixOtherBuildInputs = otherBuildInputsFor flakeNixProjectType,
          flakeNixNixpkgsNativeBuildInputs = nixpkgsNativeBuildInputsFor flakeNixProjectType,
          flakeNixHooksRequireNixpkgs = maybe False nixPreCommitHookConfigRequiresNixpkgs nixPreCommitHookConfig,
          ..
        }
    where
      extraBindingsFor :: ProjectType -> [Binding]
      extraBindingsFor = \case
        Minimal -> []
        Elm _ -> []
        Haskell (HaskellOptions _ haskellProjectType) ->
          [ [nixbinding|haskellPackages = import nix/haskell-packages.nix { inherit nixpkgs; };|],
            [nixproperty|haskellEnv|]
              |= ghcWithPackages
                ( case haskellProjectType of
                    HaskellProjectTypeReplOnly -> [[nixident|cabal-install|]]
                    HaskellProjectTypeBasic _ -> [[nixident|cabal-install|], [nixident|haskell-language-server|]]
                )
          ]
        Node _ -> []
        Go _ -> []
        Java _ -> []
        Python _ -> [pythonPackagesBinding]
        Rust -> []
        where
          ghcWithPackages :: [Identifier] -> Expr
          ghcWithPackages =
            EApplication [nix|haskellPackages.ghcWithPackages|]
              . EGrouping
              . EFunc [nixargs|pkgs:|]
              . EWith [nix|pkgs|]
              . EList
              . fmap EIdent
      nixpkgsBuildInputsFor :: ProjectType -> [Expr]
      nixpkgsBuildInputsFor =
        sortBy compareBuildInputs . \case
          Minimal -> []
          Elm elmOptions ->
            [ [nix|elmPackages.elm|],
              [nix|elmPackages.elm-json|],
              [nix|elmPackages.elm-language-server|]
            ]
              <> case elmOptionElmMode elmOptions of
                ElmModeBare -> []
                ElmModeNode packageManager -> [nix|nodejs|] : nodePackageManager packageManager
          Haskell (HaskellOptions _ _) -> []
          Node packageManager ->
            [[nix|awscli2|], [nix|nodejs|]] <> nodePackageManager packageManager
          Go _ -> [[nix|go|]]
          Java (JavaOptions installMinishift _ _ jdkName) ->
            [[nix|google-java-format|]]
              <> jdkPackage jdkName
              <> [[nix|minishift|] | unInstallMinishift installMinishift]
          Python _ -> []
          Rust -> [[nix|libiconv|]]
      nodePackageManager :: NodePackageManager -> [Expr]
      nodePackageManager = \case
        NPM -> []
        PNPm -> [[nix|nodePackages.pnpm|]]
        Yarn -> [[nix|yarn|]]
      jdkPackage :: JdkPackage -> [Expr]
      jdkPackage = \case
        OpenJDK -> [[nix|jdk|], [nix|maven|]]
        GraalVM -> [[nix|graalvm-ce|], [nix|(maven.override { jdk = graalvm-ce; })|]]
      otherBuildInputsFor :: ProjectType -> [Expr]
      otherBuildInputsFor =
        sortBy compareBuildInputs . \case
          Minimal -> []
          Elm _ -> []
          Haskell (HaskellOptions _ _) -> [[nix|haskellEnv|]]
          Node _ -> []
          Go _ -> []
          Java _ -> []
          Python _ -> []
          Rust -> []
      nixpkgsNativeBuildInputsFor :: ProjectType -> [Expr]
      nixpkgsNativeBuildInputsFor =
        sortBy compareBuildInputs . \case
          Minimal -> []
          Elm _ -> []
          Haskell _ -> []
          Node _ -> []
          Go _ -> []
          Java _ -> []
          Python _ -> []
          Rust -> [[nix|cargo|], [nix|rustc|], [nix|rust-analyzer|]]
      compareBuildInputs :: Expr -> Expr -> Ordering
      compareBuildInputs (EIdent i1) (EIdent i2) = compare i1 i2
      compareBuildInputs (EPropertyAccess (EIdent i1) _) (EIdent i2) = compare i1 i2
      compareBuildInputs (EIdent i1) (EPropertyAccess (EIdent i2) _) = compare i1 i2
      compareBuildInputs (EPropertyAccess (EIdent i1) _) (EPropertyAccess (EIdent i2) _) = compare i1 i2
      compareBuildInputs _ _ = EQ

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
                    <> [ [nixbinding|pre-commit-hooks-lib.url = "github:cachix/pre-commit-hooks.nix";|]
                         | usingHooks
                       ]
                    <> [machNixFlakeInput | isPython]
                ),
            [nixproperty|outputs|]
              |= FASet
                ( [nixident|nixpkgs-src|]
                    :| ( [nixident|flake-utils|]
                           : [[nixident|mach-nix|] | isPython]
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
                          [[nixbinding|checks.pre-commit-check = preCommitHooks.pureHooks;|] | usingHooks]
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
                                           |* ESet
                                             False
                                             ( toList
                                                 . unsafeSimplifyBindings
                                                 . fmap toBuildRequirementBinding
                                                 . sortRbeRequirements
                                                 . rbeRequirements
                                                 $ unBuildNix buildNix
                                             )
                                       )
                                ]
                                  <> (if usingHooks then runChecksDerivation else [])
                              Nothing -> if usingHooks then runChecksDerivation else []
                      )
                )
          ]
    where
      toBuildRequirementBinding :: ReproducibleBuildRequirement -> Binding
      toBuildRequirementBinding =
        -- Note: When adding bindings here, check they are not interdependent as they will
        -- be passed through `unsafeSimplifyBindings`.
        \case
          RBRNixpkgs -> buildRequirementBindingInherit RBRNixpkgs
          RBRHaskellPackages -> buildRequirementBindingInherit RBRHaskellPackages
      buildRequirementBindingInherit :: ReproducibleBuildRequirement -> Binding
      buildRequirementBindingInherit = BInherit . one . reproducibleBuildRequirementIdentifier
      runChecksDerivation :: [Binding]
      runChecksDerivation =
        [ [nixbinding|# runChecks is a hack required to allow checks to run on a single system|],
          [nixbinding|# when using Import from Deviation (https://discourse.nixos.org/t/nix-flake-check-for-current-system-only/18366)|],
          [nixbinding|# Building it is the single-system equivalent of running "nix flake check".|],
          [nixbinding|packages.runChecks = nixpkgs.runCommand "run-checks" {
            currentSystemChecks = builtins.attrValues self.checks.${system};
          } "echo $currentSystemChecks; touch $out";|]
        ]
