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
import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Data.ProjectType (HaskellOptions (HaskellOptions), HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly), ProjectType (Haskell))
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as Aeson

-- | The haskell project's package.yaml
newtype PackageYaml = PackageYaml ProjectName

instance Bootstrappable PackageYaml where
  bootstrapName = const "package.yaml"
  bootstrapReason = const "The configuration of your haskell project"
  bootstrapContent = pure . pure . bootstrapContentYaml

instance ToJSON PackageYaml where
  toJSON (PackageYaml n) =
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
          Aeson.Array $
            fromList
              [ Aeson.object
                  [ ("name", "base"),
                    ("mixin", Aeson.Array $ fromList ["hiding (Prelude)"])
                  ],
                "relude"
              ]
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

packageYamlFor :: ProjectType -> ProjectName -> Maybe PackageYaml
packageYamlFor = \case
  Haskell (HaskellOptions _ haskellProjectType) -> case haskellProjectType of
    HaskellProjectTypeReplOnly -> const Nothing
    HaskellProjectTypeBasic -> Just . PackageYaml
  _ -> const Nothing
