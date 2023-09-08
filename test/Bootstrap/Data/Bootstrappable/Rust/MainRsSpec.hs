{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Rust.MainRsSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Rust.MainRs
  ( mainRsFor,
  )
import Bootstrap.Data.ProjectType
  ( ProjectType (Rust),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "main.rs rendering" do
  it "renders correctly" do
    bootstrapContent (mainRsFor Rust)
      >>= ( `shouldBe`
              Right
                [r|fn main() {
    println!("Hello, world!");
}
|]
          )
