{-# LANGUAGE QuasiQuotes #-}

-- |
-- Copyright : (c) Crown Copyright GCHQ
-- Description : A file defining a haskell package set
module Bootstrap.Data.Bootstrappable.HaskellPackagesNix (HaskellPackagesNix, haskellPackagesNixFor) where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentNix,
  )
import Bootstrap.Data.ProjectType (HaskellOptions, ProjectType (Haskell))
import Bootstrap.Nix.Expr (Expr (EFunc), FunctionArgs (FASet), IsNixExpr (toNixExpr), nixident)
import Bootstrap.Nix.Expr.Haskell (haskellPackagesExpr)

-- | A separate nix file defining the haskell package set
newtype HaskellPackagesNix = HaskellPackagesNix HaskellOptions
  deriving stock (Eq, Show)

instance Bootstrappable HaskellPackagesNix where
  bootstrapName = const "nix/haskell-packages.nix"
  bootstrapReason = const "This configures the haskell package set from which your dependencies will be pulled."
  bootstrapContent = bootstrapContentNix

instance IsNixExpr HaskellPackagesNix where
  toNixExpr (HaskellPackagesNix opts) =
    EFunc
      (FASet $ one [nixident|nixpkgs|])
      (haskellPackagesExpr opts)

-- | Gives a `HaskellPackagesNix` (or `Nothing`) as appropriate for the project details
-- given.
haskellPackagesNixFor :: ProjectType -> Maybe HaskellPackagesNix
haskellPackagesNixFor (Haskell haskellOptions) = Just $ HaskellPackagesNix haskellOptions
haskellPackagesNixFor _ = Nothing
