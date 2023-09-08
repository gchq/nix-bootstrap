{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Rust.CargoToml
  ( CargoToml,
    cargoTomlFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
  )
import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Data.ProjectType
  ( ProjectType (Rust),
  )
import Text.RawString.QQ (r)

newtype CargoToml = CargoToml ProjectName

instance Bootstrappable CargoToml where
  bootstrapName = const "Cargo.toml"
  bootstrapReason = const "The configuration of your rust project"
  bootstrapContent (CargoToml (unProjectName -> n)) =
    pure . Right $
      [r|[package]
name = "|]
        <> n
        <> [r|"
version = "0.1.0"
edition = "2021"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
|]

cargoTomlFor :: ProjectType -> ProjectName -> Maybe CargoToml
cargoTomlFor = \case
  Rust -> Just . CargoToml
  _ -> const Nothing
