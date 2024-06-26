{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr.ReproducibleBuild.Rust (reproducibleRustBuild) where

import Bootstrap.Nix.Expr
  ( Expr (ELetIn),
    nix,
    nixbinding,
    nixproperty,
    (|=),
  )
import Bootstrap.Nix.Expr.ReproducibleBuild
  ( ReproducibleBuildExpr (ReproducibleBuildExpr),
    ReproducibleBuildRequirement (RBRNixpkgs),
  )

reproducibleRustBuild ::
  -- | src path
  Expr ->
  ReproducibleBuildExpr
reproducibleRustBuild srcDir =
  ReproducibleBuildExpr
    ( ELetIn
        ( ([nixproperty|src|] |= srcDir)
            :| [[nixbinding|cargoToml = builtins.fromTOML (builtins.readFile (src + "/Cargo.toml"));|]]
        )
        [nix|nixpkgs.rustPlatform.buildRustPackage {
  inherit src;
  inherit (cargoToml.package) name version;
  cargoLock.lockFile = src + "/Cargo.lock";
}|]
    )
    (one RBRNixpkgs)
