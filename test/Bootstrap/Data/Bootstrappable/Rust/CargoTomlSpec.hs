{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Rust.CargoTomlSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Rust.CargoToml
  ( cargoTomlFor,
  )
import Bootstrap.Data.ProjectName (mkProjectName)
import Bootstrap.Data.ProjectType
  ( ProjectType (Rust),
  )
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "Cargo.toml rendering" do
  it "renders correctly" do
    bootstrapContent (cargoTomlFor Rust . Unsafe.fromJust $ mkProjectName "test-project")
      >>= ( `shouldBe`
              Right
                [r|[package]
name = "test-project"
version = "0.1.0"
edition = "2021"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
|]
          )
