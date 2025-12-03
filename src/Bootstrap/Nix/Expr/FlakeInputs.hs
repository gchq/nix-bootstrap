{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
--   Description : Manages inputs for bootstrapped flakes
module Bootstrap.Nix.Expr.FlakeInputs (nixpkgsSrcInputBinding) where

import Bootstrap.Data.Target (Target (TargetDefault))
import Bootstrap.Nix.Expr
  ( Binding,
    nix,
    nixproperty,
    (|=),
  )

nixpkgsSrcInputBinding :: Target -> Binding
nixpkgsSrcInputBinding target =
  [nixproperty|nixpkgs-src.url|] |= case target of
    TargetDefault -> [nix|"nixpkgs/25.11"|]
