-- | Copyright : (c) Crown Copyright GCHQ
module PreludeSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = describe "clamp" do
  it "successfully clamps below the lower end" $ clamp 5 10 0 `shouldBe` (5 :: Int)
  it "successfully clamps above the upper end" $ clamp 5 10 100 `shouldBe` (10 :: Int)
  it "does not modify on the lower end" $ clamp 5 10 5 `shouldBe` (5 :: Int)
  it "does not modify on the upper end" $ clamp 5 10 10 `shouldBe` (10 :: Int)
  it "does not modify in the allowed range" $ clamp 5 10 7 `shouldBe` (7 :: Int)
