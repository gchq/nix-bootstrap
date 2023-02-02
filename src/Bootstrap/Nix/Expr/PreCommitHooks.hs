{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr.PreCommitHooks (ImportPreCommitHooksArgs (..), importPreCommitHooks) where

import Bootstrap.Nix.Expr (Binding (BInherit), Expr (ESet), nix, nixident, (|*))

data ImportPreCommitHooksArgs = ImportPreCommitHooksArgs
  { passNixpkgsThrough :: Bool,
    passSystemThrough :: Bool
  }

-- | An expression which imports pre-commit hooks, assuming they're at nix/pre-commit-hooks.nix.
--
-- Passes pre-commit-hooks-lib and optionally nixpkgs and/or sustem through as arguments to the pre-commit hooks
-- config - these must be in scope.
importPreCommitHooks :: ImportPreCommitHooksArgs -> Expr
importPreCommitHooks ImportPreCommitHooksArgs {..} =
  [nix|import nix/pre-commit-hooks.nix|]
    |* ESet
      False
      [ BInherit
          ( [nixident|pre-commit-hooks-lib|]
              :| [[nixident|nixpkgs|] | passNixpkgsThrough]
              <> [[nixident|system|] | passSystemThrough]
          )
      ]
