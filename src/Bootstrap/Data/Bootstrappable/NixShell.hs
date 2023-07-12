{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.NixShell
  ( NixShell (NixShell, nixShellBuildInputs, nixShellHooksRequireNixpkgs),
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
    JavaOptions (JavaOptions),
    NodePackageManager (NPM, PNPm, Yarn),
    ProjectType (Elm, Go, Java, Minimal, Node, Python),
    unInstallMinishift,
  )
import Bootstrap.Nix.Expr
  ( Binding,
    Expr (EIdent, ELetIn, EPropertyAccess),
    IsNixExpr (toNixExpr),
    nix,
    nixbinding,
    nixproperty,
    (|=),
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
    nixShellBuildInputs :: [Expr],
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
  NixShell preCommitHooksConfig projectType (buildInputsFor projectType) $
    maybe False nixPreCommitHookConfigRequiresNixpkgs nixPreCommitHookConfig
  where
    buildInputsFor :: ProjectType -> [Expr]
    buildInputsFor =
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
          { bisNixpkgsPackages = nixShellBuildInputs,
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
                 <> case nixShellProjectType of
                   Python _ -> machNixLegacyNixBinding : one (pythonPackagesBinding False)
                   _ -> []
             )
