{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
--   Description : Definitions for reproducible build expressions
module Bootstrap.Nix.Expr.ReproducibleBuild
  ( ReproducibleBuildExpr (..),
    ReproducibleBuildRequirement (..),
    reproducibleBuildRequirementIdentifier,
  )
where

import Bootstrap.Nix.Expr (Expr, Identifier, nixident)

-- | A representation of an identifier the reproducible build expression expects to be in scope
data ReproducibleBuildRequirement
  = RBRNixpkgs
  deriving stock (Eq, Show)

reproducibleBuildRequirementIdentifier :: ReproducibleBuildRequirement -> Identifier
reproducibleBuildRequirementIdentifier = \case
  RBRNixpkgs -> [nixident|nixpkgs|]

-- | An expression defining the reproducible build for some software
data ReproducibleBuildExpr = ReproducibleBuildExpr
  { rbeExpr :: Expr,
    -- | What identifiers the expression expects to be in scope
    rbeRequirements :: NonEmpty ReproducibleBuildRequirement
  }
  deriving stock (Eq, Show)
