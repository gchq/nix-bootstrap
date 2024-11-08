{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
--   Description : Represent dependencies of haskell projects
module Bootstrap.Data.HaskellDependency
  ( HaskellDependency,
    VersionKnown (..),
    getHaskellDependencyVersions,
    hdep,
  )
where

import Bootstrap.Data.ProjectType (HaskellOptions)
import Bootstrap.Nix.Evaluate
  ( NixBinaryPaths,
    evaluateNixExpression,
    extractNixVersionString,
  )
import Bootstrap.Nix.Expr
  ( Expr (EGrouping, ELetIn),
    Identifier (Identifier),
    Property (PIdent),
    nixproperty,
    (|.),
  )
import Bootstrap.Nix.Expr.Haskell (haskellPackagesExpr)
import Bootstrap.Nix.Expr.Nixpkgs (nixpkgsBinding)
import Control.Exception (IOException)
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Language.Haskell.TH (ExpQ)
import qualified Relude.Extra.Map as M

-- | Whether the version of a particular dependency is known
data VersionKnown = VersionKnown | VersionUnknown

-- | Represents a library on which a haskell project depends
data HaskellDependency (versionKnown :: VersionKnown) where
  HaskellDependencyBase :: HaskellDependency versionKnown
  HaskellDependencyBoot :: Text -> HaskellDependency versionKnown
  HaskellDependencyVersioned ::
    Text ->
    HaskellDependencyVersion versionKnown ->
    HaskellDependency versionKnown

instance ToJSON (HaskellDependency 'VersionKnown) where
  toJSON = \case
    HaskellDependencyBase ->
      Aeson.object
        [ ("name", "base"),
          ("mixin", Aeson.Array $ fromList ["hiding (Prelude)"])
        ]
    HaskellDependencyBoot d -> Aeson.String d
    HaskellDependencyVersioned d (HaskellDependencyVersionExact v) ->
      Aeson.String $
        d <> " == " <> v

data HaskellDependencyVersion (versionKnown :: VersionKnown) where
  HaskellDependencyVersionUnknown :: HaskellDependencyVersion 'VersionUnknown
  HaskellDependencyVersionExact :: Text -> HaskellDependencyVersion 'VersionKnown

dependencies :: Map Text (HaskellDependency 'VersionUnknown)
dependencies =
  fromList
    [ ("base", HaskellDependencyBase),
      hDepBoot "Cabal",
      hDepBoot "Cabal-syntax",
      hDepBoot "Win32",
      hDepBoot "array",
      hDepBoot "bin-package-db",
      hDepBoot "binary",
      hDepBoot "bytestring",
      hDepBoot "containers",
      hDepBoot "deepseq",
      hDepBoot "directory",
      hDepBoot "exceptions",
      hDepBoot "extensible-exceptions",
      hDepBoot "ffi",
      hDepBoot "filepath",
      hDepBoot "ghc",
      hDepBoot "ghc-bignum",
      hDepBoot "ghc-binary",
      hDepBoot "ghc-boot",
      hDepBoot "ghc-boot-th",
      hDepBoot "ghc-compact",
      hDepBoot "ghc-heap",
      hDepBoot "ghc-prim",
      hDepBoot "ghci",
      hDepBoot "haskeline",
      hDepBoot "haskell2010",
      hDepBoot "haskell98",
      hDepBoot "hoopl",
      hDepBoot "hpc",
      hDepBoot "integer-gmp",
      hDepBoot "libiserv",
      hDepBoot "mtl",
      hDepBoot "old-locale",
      hDepBoot "old-time",
      hDepBoot "parsec",
      hDepBoot "pretty",
      hDepBoot "process",
      hDepBoot "random",
      hDepBoot "rts",
      hDepBoot "stm",
      hDepBoot "system-cxx-std-lib",
      hDepBoot "template-haskell",
      hDepBoot "terminfo",
      hDepBoot "text",
      hDepBoot "time",
      hDepBoot "transformers",
      hDepBoot "unix",
      hDepBoot "xhtml",
      hDepUnknown "relude"
    ]
  where
    hDepBoot :: Text -> (Text, HaskellDependency 'VersionUnknown)
    hDepBoot name = (name, HaskellDependencyBoot name)
    hDepUnknown :: Text -> (Text, HaskellDependency 'VersionUnknown)
    hDepUnknown name = (name, HaskellDependencyVersioned name HaskellDependencyVersionUnknown)

-- | Get a `HaskellDependency 'VersionUnknown` for the given dependency name
hdep :: Text -> ExpQ
hdep name = case M.lookup name dependencies of
  Just dep -> case dep of
    HaskellDependencyBase -> [|HaskellDependencyBase|]
    HaskellDependencyBoot _ ->
      [|HaskellDependencyBoot name|]
    HaskellDependencyVersioned _ HaskellDependencyVersionUnknown ->
      [|HaskellDependencyVersioned name HaskellDependencyVersionUnknown|]
  Nothing -> error $ "Could not find " <> name <> " in dependency map in Bootstrap.Data.HaskellDependency"

getHaskellDependencyVersions ::
  MonadIO m =>
  NixBinaryPaths ->
  HaskellOptions ->
  [HaskellDependency 'VersionUnknown] ->
  m (Either IOException [HaskellDependency 'VersionKnown])
getHaskellDependencyVersions nixBinaryPaths haskellOptions deps = do
  results <- forM deps \case
    HaskellDependencyBase -> pure $ Right HaskellDependencyBase
    HaskellDependencyBoot d -> pure . Right $ HaskellDependencyBoot d
    HaskellDependencyVersioned d HaskellDependencyVersionUnknown -> do
      evaluateNixExpression
        nixBinaryPaths
        ( ELetIn
            (one nixpkgsBinding)
            ( EGrouping (haskellPackagesExpr haskellOptions)
                |. PIdent (Identifier d)
                |. [nixproperty|version|]
            )
        )
        <&> fmap
          ( HaskellDependencyVersioned d
              . HaskellDependencyVersionExact
              . toText
              . extractNixVersionString
          )
  pure $ foldr throwAnyErrors (Right []) results
  where
    -- If we hit any errors, fail the whole thing
    throwAnyErrors _ e@(Left _) = e
    throwAnyErrors (Left e) _ = Left e
    -- Keep building up the accumulator as long as we're succeeding
    throwAnyErrors (Right next) (Right acc) = Right (next : acc)
