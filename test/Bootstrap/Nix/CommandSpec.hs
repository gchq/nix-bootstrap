{-# LANGUAGE TypeApplications #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.CommandSpec (spec) where

import Bootstrap.Nix.Command
  ( NixCommand (NixCommand),
    NixCommandStyle (NCSNew, NCSOld),
    NixCommandVariant (NCVBuild),
    writeNixCommand,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "writeNixCommand" do
  it "correctly writes an old-style build command" $
    writeNixCommand @Text (NixCommand NCSOld NCVBuild) `shouldBe` "nix-build"
  it "correctly writes an new-style build command" $
    writeNixCommand @Text (NixCommand NCSNew NCVBuild) `shouldBe` "nix build"
