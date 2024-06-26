{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.NixShell
  ( NixShell
      ( NixShell,
        nixShellExtraBindings,
        nixShellNixpkgsBuildInputs,
        nixShellOtherBuildInputs,
        nixShellNixpkgsNativeBuildInputs,
        nixShellHooksRequireNixpkgs
      ),
    nixShellFor,
  )
where

import Bootstrap.Cli (RunConfig (RunConfig, rcUseFlakes))
import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentNix,
  )
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig
  ( NixPreCommitHookConfig (nixPreCommitHookConfigRequiresNixpkgs),
  )
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (unPreCommitHooksConfig))
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeBare, ElmModeNode),
    ElmOptions (elmOptionElmMode),
    HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly),
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
    unInstallMinishift,
  )
import Bootstrap.Nix.Expr
  ( Binding,
    Expr (EApplication, EFunc, EGrouping, EIdent, ELetIn, EList, EPropertyAccess, EWith),
    Identifier,
    IsNixExpr (toNixExpr),
    nix,
    nixargs,
    nixbinding,
    nixident,
    nixproperty,
    (|=),
  )
import Bootstrap.Nix.Expr.BuildInputs
  ( BuildInputSpec
      ( bisNativeNixpkgsPackages,
        bisOtherPackages
      ),
  )
import Bootstrap.Nix.Expr.MkShell
  ( BuildInputSpec
      ( BuildInputSpec,
        bisNixpkgsPackages,
        bisPreCommitHooksConfig,
        bisProjectType
      ),
    mkShell,
  )
import Bootstrap.Nix.Expr.PreCommitHooks
  ( ImportPreCommitHooksArgs (ImportPreCommitHooksArgs, passNixpkgsThrough, passSystemThrough),
    importPreCommitHooks,
  )
import Bootstrap.Nix.Expr.Python (machNixLegacyNixBinding, pythonPackagesBinding)

data NixShell = NixShell
  { nixShellPreCommitHooksConfig :: PreCommitHooksConfig,
    nixShellProjectType :: ProjectType,
    nixShellExtraBindings ::
      [ ( -- Relevant to flakes?
          Bool,
          Binding
        )
      ],
    nixShellNixpkgsBuildInputs :: [Expr],
    nixShellOtherBuildInputs :: [Expr],
    nixShellNixpkgsNativeBuildInputs :: [Expr],
    -- | Used to determine whether to pass the nixpkgs argument to the pre-commit-hooks config
    nixShellHooksRequireNixpkgs :: Bool
  }

instance Bootstrappable NixShell where
  bootstrapName = const "shell.nix"
  bootstrapReason NixShell {nixShellPreCommitHooksConfig} =
    "This configures what tools are available in your development environment"
      <> if unPreCommitHooksConfig nixShellPreCommitHooksConfig
        then " and links in the pre-commit hooks."
        else "."
  bootstrapContent = bootstrapContentNix

nixShellFor :: RunConfig -> ProjectType -> PreCommitHooksConfig -> Maybe NixPreCommitHookConfig -> NixShell
nixShellFor RunConfig {rcUseFlakes} projectType preCommitHooksConfig nixPreCommitHookConfig =
  NixShell
    { nixShellPreCommitHooksConfig = preCommitHooksConfig,
      nixShellProjectType = projectType,
      nixShellExtraBindings = extraBindingsFor projectType,
      nixShellNixpkgsBuildInputs = nixpkgsBuildInputsFor projectType,
      nixShellOtherBuildInputs = otherBuildInputsFor projectType,
      nixShellNixpkgsNativeBuildInputs = nixpkgsNativeBuildInputsFor projectType,
      nixShellHooksRequireNixpkgs = maybe False nixPreCommitHookConfigRequiresNixpkgs nixPreCommitHookConfig
    }
  where
    extraBindingsFor :: ProjectType -> [(Bool, Binding)]
    extraBindingsFor = \case
      Minimal -> []
      Elm _ -> []
      Haskell (HaskellOptions _ haskellProjectType) ->
        (True,)
          <$> [ [nixbinding|haskellPackages = import nix/haskell-packages.nix { inherit nixpkgs; };|],
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
      Python _ -> (False,) <$> (machNixLegacyNixBinding : one (pythonPackagesBinding False))
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
      sortBy compareBuildInputs . (([[nix|niv|] | not rcUseFlakes]) <>) . \case
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

instance IsNixExpr NixShell where
  toNixExpr NixShell {..} =
    ELetIn topLevelBindings $
      mkShell
        BuildInputSpec
          { bisNixpkgsPackages = nixShellNixpkgsBuildInputs,
            bisOtherPackages = nixShellOtherBuildInputs,
            bisPreCommitHooksConfig = nixShellPreCommitHooksConfig,
            bisProjectType = nixShellProjectType,
            bisNativeNixpkgsPackages = nixShellNixpkgsNativeBuildInputs
          }
    where
      topLevelBindings :: (NonEmpty Binding)
      topLevelBindings =
        [nixbinding|sources = import nix/sources.nix;|]
          :| ( [nixbinding|nixpkgs = import sources.nixpkgs {};|] :
               ( if unPreCommitHooksConfig nixShellPreCommitHooksConfig
                   then
                     [ [nixbinding|pre-commit-hooks-lib = import sources.pre-commit-hooks;|],
                       [nixproperty|preCommitHooks|]
                         |= importPreCommitHooks
                           ImportPreCommitHooksArgs
                             { passNixpkgsThrough = nixShellHooksRequireNixpkgs,
                               passSystemThrough = False
                             }
                     ]
                   else []
               )
                 <> (snd <$> nixShellExtraBindings)
             )
