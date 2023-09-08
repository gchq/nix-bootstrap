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

reproducibleRustBuild ::
  -- | src path
  Expr ->
  Expr
reproducibleRustBuild srcDir =
  ELetIn
    ( ([nixproperty|src|] |= srcDir)
        :| [[nixbinding|cargoToml = builtins.fromTOML (builtins.readFile (src + "/Cargo.toml"));|]]
    )
    [nix|nixpkgs.rustPlatform.buildRustPackage {
  inherit src;
  inherit (cargoToml.package) name version;
  cargoLock.lockFile = src + "/Cargo.lock";
}|]
