{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.ProjectType
  ( ProjectSuperType (..),
    projectSuperTypeName,
    HasProjectSuperType (projectSuperType),
    ProjectType (..),
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
  )
where

import Data.Tuple.Extra (uncurry3)
import Dhall (FromDhall, ToDhall)
import Dhall.Deriving (AsIs, Codec (Codec), Constructor, Field)
import Toml (TomlCodec)
import qualified Toml

data ProjectSuperType
  = PSTMinimal
  | PSTNode
  | PSTGo
  | PSTJava
  | PSTPython
  deriving stock (Bounded, Enum, Eq, Ord, Show)

projectSuperTypeName :: ProjectSuperType -> Text
projectSuperTypeName = \case
  PSTMinimal -> "Minimal (No Language-Specific Tooling)"
  PSTNode -> "NodeJS"
  PSTGo -> "Go"
  PSTJava -> "Java"
  PSTPython -> "Python 3.9"

class HasProjectSuperType a where
  projectSuperType :: a -> ProjectSuperType

instance HasProjectSuperType ProjectSuperType where
  projectSuperType = id

projectSuperTypeToFirst :: ProjectType -> (ProjectSuperType, ProjectType)
projectSuperTypeToFirst t = (projectSuperType t, t)

data ProjectType
  = Minimal
  | Node NodePackageManager
  | Go SetUpGoBuild
  | Java JavaOptions
  | Python PythonVersion
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) ProjectType

instance HasProjectSuperType ProjectType where
  projectSuperType = \case
    Minimal -> PSTMinimal
    Node _ -> PSTNode
    Go _ -> PSTGo
    Java {} -> PSTJava
    Python _ -> PSTPython

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

projectTypeCodec :: TomlCodec ProjectType
projectTypeCodec =
  flip Toml.table "projectType" $
    Toml.match minimalBiMap "projectSuperType"
      <|> Toml.dimap
        projectSuperTypeToFirst
        snd
        ( Toml.pair
            (Toml.enumBounded "projectSuperType")
            ( Toml.dimatch matchNode Node (Toml.enumBounded "nodePackageManager")
                <|> Toml.dimatch matchGo Go (Toml.enumBounded "setUpGoBuild")
                <|> Toml.dimatch
                  matchJava
                  (Java . uncurry3 JavaOptions)
                  ( Toml.triple
                      (Toml.diwrap $ Toml.bool "installMinishift")
                      (Toml.diwrap $ Toml.bool "installLombok")
                      setUpJavaBuildCodec
                  )
                <|> Toml.dimatch matchPython Python (Toml.enumBounded "pythonVersion")
            )
        )
  where
    matchNode :: ProjectType -> Maybe NodePackageManager
    matchNode (Node packageManager) = Just packageManager
    matchNode _ = Nothing

    matchGo :: ProjectType -> Maybe SetUpGoBuild
    matchGo (Go setUpGoBuild) = Just setUpGoBuild
    matchGo _ = Nothing

    matchJava :: ProjectType -> Maybe (InstallMinishift, InstallLombok, SetUpJavaBuild)
    matchJava (Java (JavaOptions installMinishift installLombok setUpJavaBuild)) =
      Just (installMinishift, installLombok, setUpJavaBuild)
    matchJava _ = Nothing

    matchPython :: ProjectType -> Maybe PythonVersion
    matchPython (Python pythonVersion) = Just pythonVersion
    matchPython _ = Nothing

    minimalBiMap :: Toml.TomlBiMap ProjectType Toml.AnyValue
    minimalBiMap = Toml.BiMap {forward, backward}
      where
        forward :: ProjectType -> Either Toml.TomlBiMapError Toml.AnyValue
        forward = \case
          Minimal -> Right . Toml.AnyValue . Toml.Text $ show PSTMinimal
          v -> Left . Toml.ArbitraryError $ "Expected Minimal constructor but got " <> show v
        backward :: Toml.AnyValue -> Either Toml.TomlBiMapError ProjectType
        backward =
          let mkErr v = "Expected " <> show PSTMinimal <> " but got " <> v
           in \case
                (Toml.AnyValue (Toml.Text v))
                  | v == show PSTMinimal -> Right Minimal
                  | otherwise -> Left . Toml.ArbitraryError $ mkErr v
                v -> Left . Toml.ArbitraryError . mkErr $ show v
