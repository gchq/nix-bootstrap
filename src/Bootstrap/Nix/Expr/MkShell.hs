{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr.MkShell (BuildInputSpec (..), mkShell) where

import Bootstrap.Data.PreCommitHook
  ( PreCommitHooksConfig (unPreCommitHooksConfig),
  )
import Bootstrap.Data.ProjectType (HasProjectSuperType)
import Bootstrap.Nix.Expr
  ( Expr (ESet),
    nix,
    nixbinding,
    (|*),
  )
import Bootstrap.Nix.Expr.BuildInputs
  ( BuildInputSpec (BuildInputSpec, bisNixpkgsPackages, bisPreCommitHooksConfig, bisProjectType),
    buildInputsBinding,
  )

-- | A nixpkgs.mkShell expression. Expects `nixpkgs` to be in scope.
mkShell :: HasProjectSuperType t => BuildInputSpec t -> Expr
mkShell buildInputSpec@BuildInputSpec {bisPreCommitHooksConfig} =
  [nix|nixpkgs.mkShell|]
    |* ESet
      False
      ( catMaybes
          [ if unPreCommitHooksConfig bisPreCommitHooksConfig
              then Just [nixbinding|inherit (preCommitHooks.hooks) shellHook;|]
              else Nothing,
            Just $ buildInputsBinding buildInputSpec
          ]
      )
