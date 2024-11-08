{-# LANGUAGE TypeApplications #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.CommandSpec (spec) where

import Bootstrap.Nix.Command
  ( NixCommand (NixCommand),
    NixCommandVariant (NCVBuild),
    writeNixCommand,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "writeNixCommand" do
  it "correctly writes build command" $
    writeNixCommand @Text (NixCommand NCVBuild) `shouldBe` "nix build"
