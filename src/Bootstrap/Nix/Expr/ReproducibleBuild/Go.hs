{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr.ReproducibleBuild.Go (reproducibleGoBuild) where

import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Nix.Expr
  ( Binding (BLineComment),
    CommentsPolicy (ShowComments),
    Expr (ELit, ESet),
    Literal (LString),
    nix,
    nixbinding,
    nixproperty,
    writeBinding,
    (|*),
    (|=),
  )
import Bootstrap.Nix.Expr.ReproducibleBuild
  ( ReproducibleBuildExpr (ReproducibleBuildExpr),
    ReproducibleBuildRequirement (RBRNixpkgs),
  )

-- | Produces a reproducible build for a Golang project.
reproducibleGoBuild :: ProjectName -> ReproducibleBuildExpr
reproducibleGoBuild projectName =
  ReproducibleBuildExpr
    ( [nix|nixpkgs.buildGoModule|]
        |* ESet
          False
          [ [nixproperty|pname|] |= ELit (LString $ unProjectName projectName),
            [nixbinding|version = "0.1.0";|],
            [nixbinding|src = ./.;|],
            [nixbinding|vendorSha256 = null;|],
            [nixbinding|# Swap out the line above for the one below once you start adding dependencies.
        |],
            [nixbinding|# After your dependencies change, builds will fail until you update the hash below.
        |],
            [nixbinding|# When the build fails, it will tell you what the expected hash is.
        |],
            BLineComment (writeBinding ShowComments [nixbinding|vendorSha256 = "sha256-00000000000000000000000000000000000000000000";|])
          ]
    )
    (one RBRNixpkgs)
