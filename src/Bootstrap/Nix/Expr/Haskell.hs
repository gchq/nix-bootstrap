{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
--   Description : Nix expressions specific to haskell projects
module Bootstrap.Nix.Expr.Haskell (haskellPackagesExpr) where

import Bootstrap.Data.GHCVersion (ghcVersionProperty)
import Bootstrap.Data.ProjectType
  ( HaskellOptions (HaskellOptions, haskellOptionsGHCVersion, haskellOptionsHaskellProjectType),
    HaskellProjectType
      ( HaskellProjectTypeBasic,
        HaskellProjectTypeReplOnly
      ),
  )
import Bootstrap.Nix.Expr (Expr, nix, nixproperty, (|*), (|.))

-- | An expression representing the haskell package set bootstrapped. Depends on
-- nixpkgs being in scope.
haskellPackagesExpr :: HaskellOptions -> Expr
haskellPackagesExpr HaskellOptions {..} = case haskellOptionsHaskellProjectType of
  HaskellProjectTypeReplOnly -> basePackageSet
  HaskellProjectTypeBasic ->
    (basePackageSet |. [nixproperty|override|])
      |* [nix|{
        overrides = _: super: {
          # The line below may be needed to circumvent a bug in nixpkgs.
          # If the devshell builds successfully without it, feel free to remove it.
          pretty-simple = super.pretty-simple.overrideAttrs { doCheck = false; };
        };
      }|]
  where
    basePackageSet :: Expr
    basePackageSet = [nix|nixpkgs.haskell.packages|] |. ghcVersionProperty haskellOptionsGHCVersion
