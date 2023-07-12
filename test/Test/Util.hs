-- | Copyright : (c) Crown Copyright GCHQ
module Test.Util (dhallRoundtripTest, tomlRoundtripTest) where

import Dhall
  ( Encoder (embed),
    FromDhall,
    ToDhall,
    auto,
    inject,
    input,
  )
import Dhall.Core (pretty)
import Test.Hspec.Expectations.Pretty (Expectation, shouldBe)
import Toml (TomlCodec)
import qualified Toml

dhallRoundtripTest :: (Eq a, FromDhall a, Show a, ToDhall a) => a -> Expectation
dhallRoundtripTest a = input auto (pretty $ embed inject a) >>= (`shouldBe` a)

tomlRoundtripTest :: (Show a, Eq a) => TomlCodec a -> a -> Expectation
tomlRoundtripTest codec a = Toml.decode codec (Toml.encode codec a) `shouldBe` Right a
