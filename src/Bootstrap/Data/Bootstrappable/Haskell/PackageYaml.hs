{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Haskell.PackageYaml
  ( PackageYaml,
    packageYamlFor,
  )
where

import Bootstrap.Cli (RunConfig)
import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentYaml,
  )
import Bootstrap.Data.HaskellDependency
  ( HaskellDependency,
    VersionKnown (VersionKnown),
    getHaskellDependencyVersions,
    hdep,
  )
import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Data.ProjectType (HaskellOptions (HaskellOptions, haskellOptionsHaskellProjectType), HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly), ProjectType (Haskell))
import Bootstrap.Nix.Evaluate (NixBinaryPaths)
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Relude.Extra.Bifunctor (firstF)

-- | The haskell project's package.yaml
data PackageYaml = PackageYaml NixBinaryPaths RunConfig ProjectName HaskellOptions

instance Bootstrappable PackageYaml where
  bootstrapName = const "package.yaml"
  bootstrapReason = const "The configuration of your haskell project"
  bootstrapContent (PackageYaml nbps rc n opts@HaskellOptions {haskellOptionsHaskellProjectType}) = runExceptT do
    dependencies <- ExceptT
      . firstF (("Could not get haskell dependency versions: " <>) . displayException)
      . getHaskellDependencyVersions nbps rc opts
      $ case haskellOptionsHaskellProjectType of
        HaskellProjectTypeReplOnly -> []
        HaskellProjectTypeBasic -> [$(hdep "base"), $(hdep "relude")]
    pure . bootstrapContentYaml $ PackageYamlWithDependencies n dependencies

data PackageYamlWithDependencies = PackageYamlWithDependencies ProjectName [HaskellDependency 'VersionKnown]

instance ToJSON PackageYamlWithDependencies where
  toJSON (PackageYamlWithDependencies n deps) =
    Aeson.object
      [ ("name", Aeson.String $ unProjectName n),
        ("version", "0.1.0.0"),
        ( "default-extensions",
          Aeson.Array $
            fromList
              [ "BlockArguments",
                "DerivingStrategies",
                "GADTs",
                "LambdaCase",
                "NamedFieldPuns",
                "OverloadedStrings",
                "RecordWildCards",
                "StrictData"
              ]
        ),
        ( "dependencies",
          Aeson.Array . fromList $ toJSON <$> deps
        ),
        ( "flags",
          Aeson.object
            [ ( "prod",
                Aeson.object
                  [ ("default", Aeson.Bool False),
                    ("description", "Enable production defaults"),
                    ("manual", Aeson.Bool True)
                  ]
              )
            ]
        ),
        ( "ghc-options",
          Aeson.Array $
            fromList
              [ "-Wall",
                "-Wcpp-undef",
                "-Widentities",
                "-Wincomplete-patterns",
                "-Wincomplete-record-updates",
                "-Wincomplete-uni-patterns",
                "-Wmissing-deriving-strategies",
                "-Wmissing-export-lists",
                "-Wmissing-import-lists",
                "-Wmissing-signatures",
                "-Wpartial-fields",
                "-Wredundant-constraints"
              ]
        ),
        ( "when",
          Aeson.Array $
            fromList
              [ Aeson.object
                  [ ("condition", "flag(prod)"),
                    ("ghc-options", Aeson.Array $ fromList ["-O2", "-Werror"])
                  ]
              ]
        ),
        ( "library",
          Aeson.object
            [ ("source-dirs", "src"),
              ("dependencies", Aeson.Array mempty)
            ]
        ),
        ( "executables",
          Aeson.object
            [ ( "app",
                Aeson.object
                  [ ("main", "Main.hs"),
                    ("source-dirs", "app"),
                    ( "ghc-options",
                      Aeson.Array $
                        fromList ["-O2", "-threaded", "-rtsopts", "-with-rtsopts=-N"]
                    ),
                    ("dependencies", Aeson.String $ unProjectName n)
                  ]
              )
            ]
        )
      ]

packageYamlFor :: NixBinaryPaths -> RunConfig -> ProjectName -> ProjectType -> Maybe PackageYaml
packageYamlFor nbps rc projectName = \case
  Haskell haskellOptions@(HaskellOptions _ haskellProjectType) -> case haskellProjectType of
    HaskellProjectTypeReplOnly -> Nothing
    HaskellProjectTypeBasic -> Just $ PackageYaml nbps rc projectName haskellOptions
  _ -> Nothing
