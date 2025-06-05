{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Haskell.PackageYaml
  ( PackageYaml,
    packageYamlFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentYaml,
  )
import Bootstrap.Data.HaskellDependency
  ( HaskellDependency,
    VersionKnown (VersionKnown),
    getHaskellDependencyVersions,
    hdeps,
  )
import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Data.ProjectType (HaskellOptions (HaskellOptions, haskellOptionsHaskellProjectType), HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly, HaskellProjectTypeServer), ProjectType (Haskell))
import Bootstrap.Nix.Evaluate (NixBinaryPaths)
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Relude.Extra.Bifunctor (firstF)

-- | The haskell project's package.yaml
data PackageYaml = PackageYaml NixBinaryPaths ProjectName HaskellOptions

instance Bootstrappable PackageYaml where
  bootstrapName = const "package.yaml"
  bootstrapReason = const "The configuration of your haskell project"
  bootstrapContent (PackageYaml nbps n opts@HaskellOptions {haskellOptionsHaskellProjectType}) = runExceptT do
    let getDependencies deps =
          ExceptT
            . firstF (("Could not get haskell dependency versions: " <>) . displayException)
            $ getHaskellDependencyVersions nbps opts deps
    globalDeps <- getDependencies $
      case haskellOptionsHaskellProjectType of
        HaskellProjectTypeReplOnly -> []
        HaskellProjectTypeBasic _ -> $(hdeps ["base", "relude"])
        HaskellProjectTypeServer _ -> $(hdeps ["base", "relude"])
    libraryDeps <- getDependencies $
      case haskellOptionsHaskellProjectType of
        HaskellProjectTypeReplOnly -> []
        HaskellProjectTypeBasic _ -> []
        HaskellProjectTypeServer _ -> $(hdeps ["aeson", "servant-server"])
    executableDeps <- getDependencies $
      case haskellOptionsHaskellProjectType of
        HaskellProjectTypeReplOnly -> []
        HaskellProjectTypeBasic _ -> []
        HaskellProjectTypeServer _ -> $(hdeps ["warp"])
    pure . bootstrapContentYaml $
      PackageYamlWithDependencies
        { packageYamlWithDependendenciesProjectName = n,
          packageYamlWithDependenciesGlobalDependencies = globalDeps,
          packageYamlWithDependenciesLibraryDependencies = libraryDeps,
          packageYamlWithDependenciesExecutableDependencies = executableDeps
        }

data PackageYamlWithDependencies = PackageYamlWithDependencies
  { packageYamlWithDependendenciesProjectName :: ProjectName,
    packageYamlWithDependenciesGlobalDependencies :: [HaskellDependency 'VersionKnown],
    packageYamlWithDependenciesLibraryDependencies :: [HaskellDependency 'VersionKnown],
    packageYamlWithDependenciesExecutableDependencies :: [HaskellDependency 'VersionKnown]
  }

instance ToJSON PackageYamlWithDependencies where
  toJSON (PackageYamlWithDependencies {..}) =
    Aeson.object
      [ ("name", Aeson.String $ unProjectName packageYamlWithDependendenciesProjectName),
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
          Aeson.Array . fromList $ toJSON <$> packageYamlWithDependenciesGlobalDependencies
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
              ( "dependencies",
                Aeson.Array . fromList $ toJSON <$> packageYamlWithDependenciesLibraryDependencies
              )
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
                    ( "dependencies",
                      Aeson.Array
                        . fromList
                        . ((Aeson.String $ unProjectName packageYamlWithDependendenciesProjectName) :)
                        $ toJSON <$> packageYamlWithDependenciesExecutableDependencies
                    )
                  ]
              )
            ]
        )
      ]

packageYamlFor :: NixBinaryPaths -> ProjectName -> ProjectType -> Maybe PackageYaml
packageYamlFor nbps projectName = \case
  Haskell haskellOptions@(HaskellOptions _ haskellProjectType) -> case haskellProjectType of
    HaskellProjectTypeReplOnly -> Nothing
    HaskellProjectTypeBasic _ -> Just $ PackageYaml nbps projectName haskellOptions
    HaskellProjectTypeServer _ -> Just $ PackageYaml nbps projectName haskellOptions
  _ -> Nothing
