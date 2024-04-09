{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.DefaultNix
  ( DefaultNix (defaultNixReproducibleBuildExpr),
    SrcDir (..),
    defaultNixFor,
  )
where

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
  )
import Bootstrap.Nix.Expr
  ( Binding,
    Expr (ELetIn),
    IsNixExpr (toNixExpr),
    Property (PIdent),
    nix,
    (|=),
  )
import Bootstrap.Nix.Expr.Nixpkgs (nixpkgsFromNiv)
import Bootstrap.Nix.Expr.ReproducibleBuild
  ( ReproducibleBuildExpr (ReproducibleBuildExpr, rbeExpr, rbeRequirements),
    ReproducibleBuildRequirement (RBRNixpkgs),
    reproducibleBuildRequirementIdentifier,
  )
import Bootstrap.Nix.Expr.ReproducibleBuild.Go (reproducibleGoBuild)
import Bootstrap.Nix.Expr.ReproducibleBuild.Java (reproducibleJavaBuild)
import Bootstrap.Nix.Expr.ReproducibleBuild.Rust (reproducibleRustBuild)

newtype DefaultNix = DefaultNix {defaultNixReproducibleBuildExpr :: ReproducibleBuildExpr}

instance Bootstrappable DefaultNix where
  bootstrapName = const "default.nix"
  bootstrapReason = const "This configures your reproducible project builds."
  bootstrapContent = bootstrapContentNix

instance IsNixExpr DefaultNix where
  toNixExpr (DefaultNix ReproducibleBuildExpr {..}) =
    ELetIn (bindingFor <$> rbeRequirements) rbeExpr
    where
      bindingFor :: ReproducibleBuildRequirement -> Binding
      bindingFor r =
        PIdent (reproducibleBuildRequirementIdentifier r) |= case r of
          RBRNixpkgs -> nixpkgsFromNiv

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

defaultNixFor :: SrcDir -> ProjectName -> ProjectType -> Maybe DefaultNix
defaultNixFor srcDir projectName = \case
  Minimal -> Nothing
  Elm _ -> Nothing
  Haskell _ -> Nothing
  Node _ -> Nothing
  Go (SetUpGoBuild True) -> Just . DefaultNix $ reproducibleGoBuild projectName
  Go _ -> Nothing
  Java (JavaOptions _ _ (SetUpJavaBuild artefactId)) -> Just . DefaultNix $ reproducibleJavaBuild projectName artefactId
  Java _ -> Nothing
  Python _ -> Nothing
  Rust -> Just . DefaultNix . reproducibleRustBuild $ srcDirExpr srcDir
