{-# LANGUAGE QuasiQuotes #-}

-- |
-- Copyright : (c) Crown Copyright GCHQ
-- Description : The code for nix/systems.nix
module Bootstrap.Data.Bootstrappable.SystemsNix (SystemsNix (SystemsNix)) where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentNix,
  )
import Bootstrap.Nix.Expr (IsNixExpr (toNixExpr), nix)

-- | A separate nix file defining functions for working with multiple systems easily,
-- based on numtide/flake-utils
data SystemsNix = SystemsNix
  deriving stock (Eq, Show)

instance Bootstrappable SystemsNix where
  bootstrapName = const "nix/systems.nix"
  bootstrapReason = const "This contains helpers for working with different systems."
  bootstrapContent = bootstrapContentNix

instance IsNixExpr SystemsNix where
  toNixExpr SystemsNix =
    [nix|# A minimal subset of numtide/flake-utils
    {
  forEachSystem = systems: f:
    builtins.foldl' (
      attrs: system: let
        ret = f system;
      in
        builtins.foldl' (attrs: key:
          attrs //
          {
            ${key} =
              (attrs.${key} or {})
              // {
                ${system} = ret.${key};
              };
          })
        attrs (builtins.attrNames ret)) {}
    systems;

  system = allSystems:
    builtins.listToAttrs
    (builtins.map (system: {
        name = system;
        value = system;
      })
      allSystems);
}|]
