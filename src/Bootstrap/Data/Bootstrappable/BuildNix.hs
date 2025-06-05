{-# LANGUAGE QuasiQuotes #-}

-- |
-- Copyright : (c) Crown Copyright GCHQ
-- Description : The code for build.nix
module Bootstrap.Data.Bootstrappable.BuildNix (BuildNix (unBuildNix), buildNixFor) where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentNix,
  )
import Bootstrap.Data.ProjectName (ProjectName)
import Bootstrap.Data.ProjectType
  ( JavaOptions (JavaOptions),
    ProjectType
      ( Elm,
        Go,
        Haskell,
        Java,
        Minimal,
        Node,
        Python,
        Rust
      ),
    SetUpGoBuild (SetUpGoBuild),
    SetUpJavaBuild (SetUpJavaBuild),
    haskellOptionsRequireBuild,
  )
import Bootstrap.Nix.Expr (Expr (EFunc), FunctionArgs (FASet), IsNixExpr (toNixExpr), nix)
import Bootstrap.Nix.Expr.ReproducibleBuild
  ( ReproducibleBuildExpr (ReproducibleBuildExpr, rbeExpr, rbeRequirements),
    reproducibleBuildRequirementIdentifier,
  )
import Bootstrap.Nix.Expr.ReproducibleBuild.Go (reproducibleGoBuild)
import Bootstrap.Nix.Expr.ReproducibleBuild.Haskell (reproducibleHaskellBuild)
import Bootstrap.Nix.Expr.ReproducibleBuild.Java (reproducibleJavaBuild)
import Bootstrap.Nix.Expr.ReproducibleBuild.Rust (reproducibleRustBuild)

-- | A separate nix file defining reproducible builds for flake projects,
-- to save it all being kept in flake.nix
newtype BuildNix = BuildNix {unBuildNix :: ReproducibleBuildExpr}
  deriving stock (Eq, Show)

instance Bootstrappable BuildNix where
  bootstrapName = const "nix/build.nix"
  bootstrapReason = const "This configures your reproducible project builds."
  bootstrapContent = bootstrapContentNix

instance IsNixExpr BuildNix where
  toNixExpr (BuildNix ReproducibleBuildExpr {..}) =
    EFunc
      (FASet $ reproducibleBuildRequirementIdentifier <$> rbeRequirements)
      rbeExpr

-- | Gives a `BuildNix` (or `Nothing`) as appropriate for the project details
-- given.
buildNixFor :: ProjectName -> ProjectType -> Maybe BuildNix
buildNixFor flakeNixProjectName flakeNixProjectType =
  BuildNix
    <$> buildExprFor SrcDirParent flakeNixProjectName flakeNixProjectType

-- | The source directory for the build
data SrcDir
  = -- | src = ./.;
    SrcDirCurrent
  | -- | src = ../.;
    SrcDirParent

srcDirExpr :: SrcDir -> Expr
srcDirExpr = \case
  SrcDirCurrent -> [nix|./.|]
  SrcDirParent -> [nix|../.|]

buildExprFor :: SrcDir -> ProjectName -> ProjectType -> Maybe ReproducibleBuildExpr
buildExprFor srcDir projectName = \case
  Minimal -> Nothing
  Elm _ -> Nothing
  Haskell haskellOptions
    | haskellOptionsRequireBuild haskellOptions ->
        Just . reproducibleHaskellBuild projectName $ srcDirExpr srcDir
  Haskell _ -> Nothing
  Node _ -> Nothing
  Go (SetUpGoBuild True) -> Just $ reproducibleGoBuild projectName
  Go _ -> Nothing
  Java (JavaOptions _ _ (SetUpJavaBuild artefactId) jdk) -> Just $ reproducibleJavaBuild projectName artefactId jdk
  Java _ -> Nothing
  Python _ -> Nothing
  Rust -> Just . reproducibleRustBuild $ srcDirExpr srcDir
