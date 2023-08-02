{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
-- | Description : Compatibility for pre-flake tools to work with the bootstraped flake.nix
module Bootstrap.Data.Bootstrappable.NixShellCompat
  ( NixShellCompat,
    nixShellCompatFor,
  )
where

import Bootstrap.Cli (RunConfig (RunConfig, rcUseFlakes))
import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentNix,
  )
import Bootstrap.Nix.Expr (IsNixExpr (toNixExpr), nix)

-- | A shell.nix file used to allow access to the devshell defined in flake.nix
-- in a non-flake environment
data NixShellCompat = NixShellCompat
  deriving stock (Eq, Show)

instance Bootstrappable NixShellCompat where
  bootstrapName = const "shell.nix"
  bootstrapReason NixShellCompat =
    "This enables you to use your development shell "
      <> "when Nix flakes aren't available."
  bootstrapContent = bootstrapContentNix

instance IsNixExpr NixShellCompat where
  toNixExpr NixShellCompat =
    [nix|(import (
    let
      lock = builtins.fromJSON (builtins.readFile ./flake.lock);
    in
      fetchTarball {
        url = "https://github.com/edolstra/flake-compat/archive/${
          lock.nodes.flake-compat.locked.rev
        }.tar.gz";
        sha256 = lock.nodes.flake-compat.locked.narHash;
        }) {src = ./.;}).shellNix|]

nixShellCompatFor :: RunConfig -> Maybe NixShellCompat
nixShellCompatFor RunConfig {rcUseFlakes} =
  if rcUseFlakes
    then Just NixShellCompat
    else Nothing
