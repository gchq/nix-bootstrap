{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

-- | Copyright : (c) Crown Copyright GCHQ
--   Description : Represent versions of the Glasgow Haskell Compiler
module Bootstrap.Data.GHCVersion
  ( -- * Data
    GHCVersion (..),

    -- * Parsing
    parseGHCVersion,

    -- * Formatting
    printGHCVersion,

    -- * Use in Nix expressions
    ghcVersionProperty,
  )
where

import Bootstrap.Nix.Expr (Identifier (Identifier), Property (PIdent))
import qualified Data.Char as C
import qualified Data.Text as T
import Dhall (FromDhall, ToDhall)
import Dhall.Deriving
  ( CamelCase,
    Codec (Codec),
    DropPrefix,
    Field,
    type (<<<),
  )
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec
  ( MonadParsec (label, takeWhile1P),
    Parsec,
    failure,
  )
import Text.Megaparsec.Char (string)
import qualified Text.Megaparsec.Error as ParseError

-- | Represents a version of the Glasgow Haskell Compiler
data GHCVersion = GHCVersion
  { ghcVersionMajor :: Natural,
    ghcVersionMinor :: Natural,
    ghcVersionPatch :: Natural
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving (FromDhall, ToDhall) via Codec (Field (CamelCase <<< DropPrefix "ghcVersion")) GHCVersion

-- | Gets the attribute name of the GHC version, able to be queried in nixpkgs
ghcVersionProperty :: GHCVersion -> Property
ghcVersionProperty = PIdent . Identifier . ("ghc" <>) . T.filter (/= '.') . printGHCVersion

-- | Parses a nixpkgs attribute representing a GHC version
--
-- Use `ghcVersionAttributeName` to produce this form
parseGHCVersion :: Parsec Void String GHCVersion
parseGHCVersion = label "GHC Version attribute" do
  void $ string "ghc"
  -- Safe because we have the `isDigit` predicate
  digits <- Unsafe.read . one <<$>> takeWhile1P (Just "version digits") C.isDigit
  case digits of
    [x, y, z] -> pure $ GHCVersion x y z
    [x, y1, y2, z] -> pure $ GHCVersion x (twoDigitVersionNumber y1 y2) z
    [x1, x2, y1, y2, z] -> pure $ GHCVersion (twoDigitVersionNumber x1 x2) (twoDigitVersionNumber y1 y2) z
    _ -> failure Nothing $ fromList [ParseError.Label ('v' :| "ersion digits")]
  where
    twoDigitVersionNumber :: Natural -> Natural -> Natural
    twoDigitVersionNumber x1 x2 = Unsafe.read $ show x1 <> show x2

-- | Print out the version in a human-readable format
--
-- Not an inverse of `parseGHCVersion` - see `ghcVersionAttributeName`
printGHCVersion :: GHCVersion -> Text
printGHCVersion (GHCVersion major minor patch) =
  T.intercalate "." $ show <$> [major, minor, patch]
