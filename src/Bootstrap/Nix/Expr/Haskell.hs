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
import Bootstrap.Nix.Expr
  ( Expr (ESet),
    nix,
    nixargs,
    nixbinding,
    nixproperty,
    (|*),
    (|.),
    (|:),
    (|=),
  )

-- | An expression representing the haskell package set bootstrapped. Depends on
-- nixpkgs being in scope.
haskellPackagesExpr :: HaskellOptions -> Expr
haskellPackagesExpr HaskellOptions {..} =
  basePackageSet
    |. [nixproperty|override|]
    |* ESet
      False
      [ [nixproperty|overrides|]
          |= ( [nixargs|_:|]
                 |: ( [nixargs|super:|]
                        |: ESet
                          False
                          ( ( case haskellOptionsHaskellProjectType of
                                HaskellProjectTypeReplOnly -> []
                                HaskellProjectTypeBasic _ ->
                                  [ [nixbinding|# The override of pretty-simple below may be needed to circumvent a bug in nixpkgs.|],
                                    [nixbinding|# If the devshell builds successfully without it, feel free to remove it.|],
                                    [nixbinding|pretty-simple = super.pretty-simple.overrideAttrs { doCheck = false; };|]
                                  ]
                            )
                              <> [[nixbinding|# You can overide packages here if you need any dependencies not in this set by default|]]
                          )
                    )
             )
      ]
  where
    basePackageSet :: Expr
    basePackageSet = [nix|nixpkgs.haskell.packages|] |. ghcVersionProperty haskellOptionsGHCVersion
