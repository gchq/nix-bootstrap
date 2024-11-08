{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.NixShellCompatSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.NixShellCompat
  ( NixShellCompat (NixShellCompat),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "shell.nix (compat) rendering" do
  it "renders correctly" do
    bootstrapContent NixShellCompat
      >>= ( `shouldBe`
              Right
                [r|(import (let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
in
  fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${
      lock.nodes.flake-compat.locked.rev
    }.tar.gz";
    sha256 = lock.nodes.flake-compat.locked.narHash;
  }) {
  src = ./.;
})
.shellNix
|]
          )
