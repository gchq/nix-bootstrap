{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
--   Description : Definitions for reproducible build expressions
module Bootstrap.Nix.Expr.ReproducibleBuild
  ( ReproducibleBuildExpr (..),
    ReproducibleBuildRequirement (..),
    reproducibleBuildRequirementIdentifier,
    sortRbeRequirements,
  )
where

import Bootstrap.Nix.Expr (Expr, Identifier, nixident)
import qualified Data.List.NonEmpty as NE

-- | A representation of an identifier the reproducible build expression expects to be in scope
data ReproducibleBuildRequirement
  = RBRHaskellPackages
  | RBRNixpkgs
  deriving stock (Eq, Show)

reproducibleBuildRequirementIdentifier :: ReproducibleBuildRequirement -> Identifier
reproducibleBuildRequirementIdentifier = \case
  RBRHaskellPackages -> [nixident|haskellPackages|]
  RBRNixpkgs -> [nixident|nixpkgs|]

-- | An expression defining the reproducible build for some software
data ReproducibleBuildExpr = ReproducibleBuildExpr
  { rbeExpr :: Expr,
    -- | What identifiers the expression expects to be in scope
    rbeRequirements :: NonEmpty ReproducibleBuildRequirement
  }
  deriving stock (Eq, Show)

-- | Ensures requirements with dependencies on each other are properly ordered in a let-in style,
-- such that the dependencies are defined before the things that depend on them.
sortRbeRequirements :: NonEmpty ReproducibleBuildRequirement -> NonEmpty ReproducibleBuildRequirement
sortRbeRequirements = NE.sortBy f
  where
    f RBRNixpkgs _ = LT
    f _ RBRNixpkgs = GT
    f _ _ = EQ
