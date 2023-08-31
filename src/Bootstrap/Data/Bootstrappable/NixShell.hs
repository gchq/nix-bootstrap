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
import Bootstrap.Data.GHCVersion (ghcVersionAttributeName)
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (unPreCommitHooksConfig))
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeBare, ElmModeNode),
    ElmOptions (elmOptionElmMode),
    HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly),
    JavaOptions (JavaOptions),
    NodePackageManager (NPM, PNPm, Yarn),
    ProjectType (Elm, Go, Haskell, Java, Minimal, Node, Python),
    unInstallMinishift,
  )
import Bootstrap.Nix.Expr
  ( Binding,
    Expr (EApplication, EFunc, EGrouping, EIdent, ELetIn, EList, ELit, EPropertyAccess, EWith),
    Identifier,
    IsNixExpr (toNixExpr),
    Literal (LString),
    nix,
    nixargs,
    nixbinding,
    nixident,
    nixproperty,
    (|*),
    (|.),
    (|=),
  )
import Bootstrap.Nix.Expr.BuildInputs (BuildInputSpec (bisOtherPackages))
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
      nixShellHooksRequireNixpkgs = maybe False nixPreCommitHookConfigRequiresNixpkgs nixPreCommitHookConfig
    }
  where
    extraBindingsFor :: ProjectType -> [(Bool, Binding)]
    extraBindingsFor = \case
      Haskell (HaskellOptions ghcVersion haskellProjectType) ->
        (True,)
          <$> [ [nixproperty|ghcAttribute|] |= ELit (LString $ ghcVersionAttributeName ghcVersion),
                [nixproperty|haskellPackages|]
                  |= applyOverridesFunc haskellProjectType [nix|nixpkgs.haskell.packages.${ghcAttribute}|],
                [nixproperty|haskellEnv|]
                  |= ghcWithPackages
                    ( case haskellProjectType of
                        HaskellProjectTypeReplOnly -> [[nixident|cabal-install|]]
                        HaskellProjectTypeBasic -> [[nixident|cabal-install|], [nixident|haskell-language-server|]]
                    )
              ]
      Python _ -> (False,) <$> (machNixLegacyNixBinding : one (pythonPackagesBinding False))
      _ -> []
      where
        applyOverridesFunc :: HaskellProjectType -> Expr -> Expr
        applyOverridesFunc = \case
          HaskellProjectTypeReplOnly -> id
          HaskellProjectTypeBasic -> \e ->
            (e |. [nixproperty|override|])
              |* [nix|{
            overrides = _: super: {
              # The line below may be needed to circumvent a bug in nixpkgs.
              # If the devshell builds successfully without it, feel free to remove it.
              pretty-simple = super.pretty-simple.overrideAttrs { doCheck = false; };
            };
          }|]
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
      sortBy compareBuildInputs . (([nix|rnix-lsp|] : [[nix|niv|] | not rcUseFlakes]) <>) . \case
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
        Java (JavaOptions installMinishift _ _) ->
          [[nix|maven|], [nix|google-java-format|], [nix|jdk|]]
            <> [[nix|minishift|] | unInstallMinishift installMinishift]
        Python _ -> []
    nodePackageManager :: NodePackageManager -> [Expr]
    nodePackageManager = \case
      NPM -> []
      PNPm -> [[nix|nodePackages.pnpm|]]
      Yarn -> [[nix|yarn|]]
    otherBuildInputsFor :: ProjectType -> [Expr]
    otherBuildInputsFor =
      sortBy compareBuildInputs . \case
        Haskell (HaskellOptions _ _) -> [[nix|haskellEnv|]]
        _ -> []
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
            bisProjectType = nixShellProjectType
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
