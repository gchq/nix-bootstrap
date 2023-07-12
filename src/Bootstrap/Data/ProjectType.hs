{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.ProjectType
  ( ProjectSuperType (..),
    projectSuperTypeName,
    HasProjectSuperType (projectSuperType),
    ProjectType (..),
    ElmOptions (..),
    ElmModeSimple (..),
    ElmMode (..),
    NodePackageManager (..),
    nodePackageManagerName,
    SetUpGoBuild (..),
    JavaOptions (..),
    InstallMinishift (..),
    InstallLombok (..),
    ArtefactId (..),
    SetUpJavaBuild (..),
    projectTypeCodec,
    pythonVersionName,
    PythonVersion (Python39),
    ProjectTypeV2 (..),
    migrateProjectTypeToV3,
  )
where

import Data.Tuple.Extra (uncurry3)
import Dhall (FromDhall, ToDhall)
import Dhall.Deriving (AsIs, CamelCase, Codec (Codec), Constructor, DropPrefix, Field, type (<<<))
import Relude.Extra.Tuple (toFst)
import Toml (TomlCodec)
import qualified Toml

data ProjectSuperType
  = PSTMinimal
  | PSTElm
  | PSTNode
  | PSTGo
  | PSTJava
  | PSTPython
  deriving stock (Bounded, Enum, Eq, Ord, Show)

projectSuperTypeName :: ProjectSuperType -> Text
projectSuperTypeName = \case
  PSTMinimal -> "Minimal (No Language-Specific Tooling)"
  PSTElm -> "Elm"
  PSTNode -> "NodeJS"
  PSTGo -> "Go"
  PSTJava -> "Java"
  PSTPython -> "Python 3.9"

class HasProjectSuperType a where
  projectSuperType :: a -> ProjectSuperType

instance HasProjectSuperType ProjectSuperType where
  projectSuperType = id

data ProjectType
  = Minimal
  | Elm ElmOptions
  | Node NodePackageManager
  | Go SetUpGoBuild
  | Java JavaOptions
  | Python PythonVersion
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) ProjectType

instance HasProjectSuperType ProjectType where
  projectSuperType = \case
    Minimal -> PSTMinimal
    Elm _ -> PSTElm
    Node _ -> PSTNode
    Go _ -> PSTGo
    Java {} -> PSTJava
    Python _ -> PSTPython

data ElmOptions = ElmOptions
  { elmOptionElmMode :: ElmMode,
    elmOptionProvideElmReview :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Field (CamelCase <<< DropPrefix "elmOption")) ElmOptions

-- | Simplified version of `ElmMode` with only nullary constructors,
-- used for prompting
data ElmModeSimple = ElmModeSimpleBare | ElmModeSimpleNode
  deriving stock (Bounded, Enum, Eq, Show)

data ElmMode
  = -- | Use elm as a standalone compiler
    ElmModeBare
  | -- | Provide a full node-based web development environment
    ElmModeNode NodePackageManager
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor (DropPrefix "ElmMode")) ElmMode

data NodePackageManager = NPM | PNPm | Yarn
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) NodePackageManager

nodePackageManagerName :: NodePackageManager -> Text
nodePackageManagerName = \case
  NPM -> "npm"
  PNPm -> "PNPm"
  Yarn -> "Yarn 1.x"

newtype SetUpGoBuild = SetUpGoBuild {unSetUpGoBuild :: Bool}
  deriving newtype (Bounded, Enum, FromDhall, ToDhall)
  deriving stock (Eq, Show)

data JavaOptions = JavaOptions
  { installMinishift :: InstallMinishift,
    installLombok :: InstallLombok,
    setUpJavaBuild :: SetUpJavaBuild
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Field AsIs) JavaOptions

newtype InstallMinishift = InstallMinishift {unInstallMinishift :: Bool}
  deriving newtype (Bounded, Enum, FromDhall, ToDhall)
  deriving stock (Eq, Show)

newtype InstallLombok = InstallLombok {unInstallLombok :: Bool}
  deriving newtype (Bounded, Enum, FromDhall, ToDhall)
  deriving stock (Eq, Show)

newtype ArtefactId = ArtefactId {unArtefactId :: Text}
  deriving newtype (FromDhall, ToDhall)
  deriving stock (Eq, Show)

data SetUpJavaBuild = SetUpJavaBuild ArtefactId | NoJavaBuild
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) SetUpJavaBuild

setUpJavaBuildCodec :: TomlCodec SetUpJavaBuild
setUpJavaBuildCodec =
  Toml.dimap toMaybe toSetUpJavaBuild (Toml.dioptional . Toml.diwrap $ Toml.text "artefactId")
  where
    toMaybe :: SetUpJavaBuild -> Maybe ArtefactId
    toMaybe = \case
      SetUpJavaBuild a -> Just a
      NoJavaBuild -> Nothing
    toSetUpJavaBuild :: Maybe ArtefactId -> SetUpJavaBuild
    toSetUpJavaBuild = maybe NoJavaBuild SetUpJavaBuild

data PythonVersion = Python39
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) PythonVersion

pythonVersionName :: PythonVersion -> Text
pythonVersionName = \case
  Python39 -> "Python 3.9"

projectTypeCodec :: TomlCodec ProjectTypeV2
projectTypeCodec =
  flip Toml.table "projectType" $
    Toml.match minimalBiMap "projectSuperType"
      <|> Toml.dimap
        (toFst projectSuperType)
        snd
        ( Toml.pair
            (Toml.enumBounded "projectSuperType")
            ( Toml.dimatch matchNode PTV2Node (Toml.enumBounded "nodePackageManager")
                <|> Toml.dimatch matchGo PTV2Go (Toml.enumBounded "setUpGoBuild")
                <|> Toml.dimatch
                  matchJava
                  (PTV2Java . uncurry3 JavaOptions)
                  ( Toml.triple
                      (Toml.diwrap $ Toml.bool "installMinishift")
                      (Toml.diwrap $ Toml.bool "installLombok")
                      setUpJavaBuildCodec
                  )
                <|> Toml.dimatch matchPython PTV2Python (Toml.enumBounded "pythonVersion")
            )
        )
  where
    matchNode :: ProjectTypeV2 -> Maybe NodePackageManager
    matchNode (PTV2Node packageManager) = Just packageManager
    matchNode _ = Nothing

    matchGo :: ProjectTypeV2 -> Maybe SetUpGoBuild
    matchGo (PTV2Go setUpGoBuild) = Just setUpGoBuild
    matchGo _ = Nothing

    matchJava :: ProjectTypeV2 -> Maybe (InstallMinishift, InstallLombok, SetUpJavaBuild)
    matchJava (PTV2Java (JavaOptions installMinishift installLombok setUpJavaBuild)) =
      Just (installMinishift, installLombok, setUpJavaBuild)
    matchJava _ = Nothing

    matchPython :: ProjectTypeV2 -> Maybe PythonVersion
    matchPython (PTV2Python pythonVersion) = Just pythonVersion
    matchPython _ = Nothing

    minimalBiMap :: Toml.TomlBiMap ProjectTypeV2 Toml.AnyValue
    minimalBiMap = Toml.BiMap {forward, backward}
      where
        forward :: ProjectTypeV2 -> Either Toml.TomlBiMapError Toml.AnyValue
        forward = \case
          PTV2Minimal -> Right . Toml.AnyValue . Toml.Text $ show PSTMinimal
          v -> Left . Toml.ArbitraryError $ "Expected PTV2Minimal constructor but got " <> show v
        backward :: Toml.AnyValue -> Either Toml.TomlBiMapError ProjectTypeV2
        backward =
          let mkErr v = "Expected " <> show PSTMinimal <> " but got " <> v
           in \case
                (Toml.AnyValue (Toml.Text v))
                  | v == show PSTMinimal -> Right PTV2Minimal
                  | otherwise -> Left . Toml.ArbitraryError $ mkErr v
                v -> Left . Toml.ArbitraryError . mkErr $ show v

-- | Historical representation of `ProjectType`
data ProjectTypeV2
  = PTV2Minimal
  | PTV2Node NodePackageManager
  | PTV2Go SetUpGoBuild
  | PTV2Java JavaOptions
  | PTV2Python PythonVersion
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor (DropPrefix "PTV2")) ProjectTypeV2

instance HasProjectSuperType ProjectTypeV2 where
  projectSuperType = \case
    PTV2Minimal -> PSTMinimal
    PTV2Node _ -> PSTNode
    PTV2Go _ -> PSTGo
    PTV2Java {} -> PSTJava
    PTV2Python _ -> PSTPython

migrateProjectTypeToV3 :: ProjectTypeV2 -> ProjectType
migrateProjectTypeToV3 = \case
  PTV2Minimal -> Minimal
  PTV2Node x -> Node x
  PTV2Go x -> Go x
  PTV2Java x -> Java x
  PTV2Python x -> Python x
