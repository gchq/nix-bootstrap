-- | Copyright : (c) Crown Copyright GCHQ
module Test.Util (tomlRoundtripTest) where

import Test.Hspec.Expectations.Pretty (Expectation, shouldBe)
import Toml (TomlCodec)
import qualified Toml

tomlRoundtripTest :: (Show a, Eq a) => TomlCodec a -> a -> Expectation
tomlRoundtripTest codec a = Toml.decode codec (Toml.encode codec a) `shouldBe` Right a
