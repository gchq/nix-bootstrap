{-# LANGUAGE QuasiQuotes #-}

-- |
-- Copyright : (c) Crown Copyright GCHQ
-- Description : The code for build.nix
module Bootstrap.Data.Bootstrappable.BuildNix (BuildNix, buildNixFor) where

import Bootstrap.Cli (RunConfig (RunConfig, rcUseFlakes))
import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentNix,
  )
import Bootstrap.Data.Bootstrappable.DefaultNix
  ( DefaultNix (defaultNixInBlockExpr),
    defaultNixFor,
  )
import Bootstrap.Data.ProjectName (ProjectName)
import Bootstrap.Data.ProjectType (ProjectType)
import Bootstrap.Nix.Expr (Expr (EFunc), IsNixExpr (toNixExpr), nixargs)

-- | A separate nix file defining reproducible builds for flake projects,
-- to save it all being kept in flake.nix
newtype BuildNix = BuildNix {unBuildNix :: Expr}
  deriving stock (Eq, Show)

instance Bootstrappable BuildNix where
  bootstrapName = const "nix/build.nix"
  bootstrapReason = const "This configures your reproducible project builds."
  bootstrapContent = bootstrapContentNix

instance IsNixExpr BuildNix where
  toNixExpr = EFunc [nixargs|{ nixpkgs }:|] . unBuildNix

-- | Gives a `BuildNix` (or `Nothing`) as appropriate for the project details
-- given.
buildNixFor :: RunConfig -> ProjectName -> ProjectType -> Maybe BuildNix
buildNixFor RunConfig {rcUseFlakes} flakeNixProjectName flakeNixProjectType =
  let buildExpr =
        defaultNixInBlockExpr
          <$> defaultNixFor flakeNixProjectName flakeNixProjectType
   in if rcUseFlakes
        then BuildNix <$> buildExpr
        else Nothing
