{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
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
    HaskellOptions (..),
    HaskellProjectType (..),
    promptHaskellProjectType,
    SetUpHaskellBuild (..),
    NodePackageManager (..),
    nodePackageManagerName,
    SetUpGoBuild (..),
    JavaOptions (..),
    JavaOptionsV2 (JavaOptionsV2),
    JdkPackage (..),
    jdkName,
    jdkPackageName,
    InstallMinishift (..),
    InstallLombok (..),
    ArtefactId (..),
    SetUpJavaBuild (..),
    projectTypeCodec,
    pythonVersionName,
    PythonVersion (Python39),

    -- * Historical ProjectType representations
    ProjectTypeV2 (..),
    migrateProjectTypeFromV2,
    ProjectTypeV3 (..),
    migrateProjectTypeFromV3,
    ProjectTypeV4 (..),
    migrateProjectTypeFromV4,
    ProjectTypeV5 (..),
    migrateProjectTypeFromV5,
    ProjectTypeV6 (..),
    migrateProjectTypeFromV6,
  )
where

import Bootstrap.Data.GHCVersion (GHCVersion)
import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.Terminal (askIfReproducibleBuildRequired, promptChoice)
import qualified Data.Text as T
import Data.Tuple.Extra (uncurry3)
import Dhall (FromDhall, ToDhall)
import Dhall.Deriving
  ( AsIs,
    CamelCase,
    Codec (Codec),
    Constructor,
    DropPrefix,
    Field,
    TextFunction (textFunction),
    type (<<<),
  )
import Relude.Extra.Tuple (toFst)
import Toml (TomlCodec)
import qualified Toml

data ProjectSuperType
  = PSTMinimal
  | PSTElm
  | PSTHaskell
  | PSTNode
  | PSTGo
  | PSTJava
  | PSTPython
  | PSTRust
  deriving stock (Bounded, Enum, Eq, Ord, Show)

projectSuperTypeName :: ProjectSuperType -> Text
projectSuperTypeName = \case
  PSTMinimal -> "Minimal (No Language-Specific Tooling)"
  PSTElm -> "Elm"
  PSTHaskell -> "Haskell"
  PSTNode -> "NodeJS"
  PSTGo -> "Go"
  PSTJava -> "Java"
  PSTPython -> "Python 3.9"
  PSTRust -> "Rust"

class HasProjectSuperType a where
  projectSuperType :: a -> ProjectSuperType

instance HasProjectSuperType ProjectSuperType where
  projectSuperType = id

data ProjectType
  = Minimal
  | Elm ElmOptions
  | Haskell HaskellOptions
  | Node NodePackageManager
  | Go SetUpGoBuild
  | Java JavaOptions
  | Python PythonVersion
  | Rust
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) ProjectType

instance HasProjectSuperType ProjectType where
  projectSuperType = \case
    Minimal -> PSTMinimal
    Elm _ -> PSTElm
    Haskell _ -> PSTHaskell
    Node _ -> PSTNode
    Go _ -> PSTGo
    Java _ -> PSTJava
    Python _ -> PSTPython
    Rust -> PSTRust

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

-- | Configures how `HaskellOptions` fields should be named in dhall expressions
data HaskellOptionsDhallFields

instance TextFunction HaskellOptionsDhallFields where
  textFunction = \case
    "haskellOptionsGHCVersion" -> "ghcVersion"
    _anythingElse
      | "haskellOptionsV4" `T.isPrefixOf` _anythingElse -> textFunction @(CamelCase <<< DropPrefix "haskellOptionsV4") _anythingElse
      | otherwise -> textFunction @(CamelCase <<< DropPrefix "haskellOptions") _anythingElse

data HaskellOptionsV4 = HaskellOptionsV4
  { haskellOptionsV4GHCVersion :: GHCVersion,
    haskellOptionsV4HaskellProjectType :: HaskellProjectTypeV4
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Field HaskellOptionsDhallFields) HaskellOptionsV4

data HaskellOptions = HaskellOptions
  { haskellOptionsGHCVersion :: GHCVersion,
    haskellOptionsHaskellProjectType :: HaskellProjectType
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Field HaskellOptionsDhallFields) HaskellOptions

data HaskellProjectTypeV4
  = HaskellProjectV4TypeReplOnly
  | HaskellProjectV4TypeBasic
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor (DropPrefix "HaskellProjectTypeV4")) HaskellProjectTypeV4

data HaskellProjectType
  = HaskellProjectTypeReplOnly
  | HaskellProjectTypeBasic SetUpHaskellBuild
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor (DropPrefix "HaskellProjectType")) HaskellProjectType

data HaskellProjectTypeSimple
  = HaskellProjectTypeSimpleReplOnly
  | HaskellProjectTypeSimpleBasic
  deriving stock (Bounded, Enum, Eq, Generic, Show)

haskellProjectTypeSimpleName :: HaskellProjectTypeSimple -> Text
haskellProjectTypeSimpleName = \case
  HaskellProjectTypeSimpleReplOnly -> "REPL only"
  HaskellProjectTypeSimpleBasic -> "Basic Library + Executable"

newtype SetUpHaskellBuild = SetUpHaskellBuild {unSetUpHaskellBuild :: Bool}
  deriving newtype (Bounded, Enum, FromDhall, ToDhall)
  deriving stock (Eq, Show)

promptHaskellProjectType :: (MonadBootstrap m) => m HaskellProjectType
promptHaskellProjectType =
  promptChoice
    "What kind of Haskell project would you like?"
    universeNonEmpty
    haskellProjectTypeSimpleName
    >>= \case
      HaskellProjectTypeSimpleReplOnly -> pure HaskellProjectTypeReplOnly
      HaskellProjectTypeSimpleBasic -> HaskellProjectTypeBasic . SetUpHaskellBuild <$> askIfReproducibleBuildRequired

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

data JavaOptionsV2 = JavaOptionsV2
  { javaOptionsV2InstallMinishift :: InstallMinishift,
    javaOptionsV2InstallLombok :: InstallLombok,
    javaOptionsV2SetUpJavaBuild :: SetUpJavaBuild
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Field (CamelCase <<< DropPrefix "javaOptionsV2")) JavaOptionsV2

migrateJavaOptionsFromV2 :: JavaOptionsV2 -> JavaOptions
migrateJavaOptionsFromV2 JavaOptionsV2 {..} =
  JavaOptions
    { installMinishift = javaOptionsV2InstallMinishift,
      installLombok = javaOptionsV2InstallLombok,
      setUpJavaBuild = javaOptionsV2SetUpJavaBuild,
      jdk = OpenJDK
    }

data JavaOptions = JavaOptions
  { installMinishift :: InstallMinishift,
    installLombok :: InstallLombok,
    setUpJavaBuild :: SetUpJavaBuild,
    jdk :: JdkPackage
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

data JdkPackage = OpenJDK | GraalVM
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) JdkPackage

jdkName :: JdkPackage -> Text
jdkName = \case
  OpenJDK -> "OpenJDK"
  GraalVM -> "GraalVM CE"

jdkPackageName :: ProjectType -> Text
jdkPackageName = \case
  Java (JavaOptions _ _ _ jdk) -> case jdk of
    OpenJDK -> "jdk"
    GraalVM -> "graalvm-ce"
  _ -> ""

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
                  (PTV2Java . uncurry3 JavaOptionsV2)
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
    matchJava (PTV2Java (JavaOptionsV2 installMinishift installLombok setUpJavaBuild)) =
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

-- | Historical representations of `ProjectType`
data ProjectTypeV2
  = PTV2Minimal
  | PTV2Node NodePackageManager
  | PTV2Go SetUpGoBuild
  | PTV2Java JavaOptionsV2
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

migrateProjectTypeFromV2 :: ProjectTypeV2 -> ProjectType
migrateProjectTypeFromV2 =
  migrateProjectTypeFromV3 . \case
    PTV2Minimal -> PTV3Minimal
    PTV2Node x -> PTV3Node x
    PTV2Go x -> PTV3Go x
    PTV2Java x -> PTV3Java x
    PTV2Python x -> PTV3Python x

-- | Historical representations of `ProjectType`
data ProjectTypeV3
  = PTV3Minimal
  | PTV3Elm ElmOptions
  | PTV3Node NodePackageManager
  | PTV3Go SetUpGoBuild
  | PTV3Java JavaOptionsV2
  | PTV3Python PythonVersion
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) ProjectTypeV3

instance HasProjectSuperType ProjectTypeV3 where
  projectSuperType = \case
    PTV3Minimal -> PSTMinimal
    PTV3Elm _ -> PSTElm
    PTV3Node _ -> PSTNode
    PTV3Go _ -> PSTGo
    PTV3Java {} -> PSTJava
    PTV3Python _ -> PSTPython

migrateProjectTypeFromV3 :: ProjectTypeV3 -> ProjectType
migrateProjectTypeFromV3 =
  migrateProjectTypeFromV4 . \case
    PTV3Minimal -> PTV4Minimal
    PTV3Elm x -> PTV4Elm x
    PTV3Node x -> PTV4Node x
    PTV3Go x -> PTV4Go x
    PTV3Java x -> PTV4Java x
    PTV3Python x -> PTV4Python x

-- | Historical representations of `ProjectType`
data ProjectTypeV4
  = PTV4Minimal
  | PTV4Elm ElmOptions
  | PTV4Haskell HaskellOptionsV4
  | PTV4Node NodePackageManager
  | PTV4Go SetUpGoBuild
  | PTV4Java JavaOptionsV2
  | PTV4Python PythonVersion
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) ProjectTypeV4

instance HasProjectSuperType ProjectTypeV4 where
  projectSuperType = \case
    PTV4Minimal -> PSTMinimal
    PTV4Elm _ -> PSTElm
    PTV4Haskell _ -> PSTHaskell
    PTV4Node _ -> PSTNode
    PTV4Go _ -> PSTGo
    PTV4Java {} -> PSTJava
    PTV4Python _ -> PSTPython

migrateProjectTypeFromV4 :: ProjectTypeV4 -> ProjectType
migrateProjectTypeFromV4 = \case
  PTV4Minimal -> Minimal
  PTV4Elm x -> Elm x
  PTV4Haskell x -> Haskell $ migrateHaskellOptionsFromV4 x
  PTV4Node x -> Node x
  PTV4Go x -> Go x
  PTV4Java x -> Java $ migrateJavaOptionsFromV2 x
  PTV4Python x -> Python x

migrateHaskellOptionsFromV4 :: HaskellOptionsV4 -> HaskellOptions
migrateHaskellOptionsFromV4 HaskellOptionsV4 {..} =
  HaskellOptions
    { haskellOptionsGHCVersion = haskellOptionsV4GHCVersion,
      haskellOptionsHaskellProjectType = migrateHaskellProjectTypeFromV4 haskellOptionsV4HaskellProjectType
    }

migrateHaskellProjectTypeFromV4 :: HaskellProjectTypeV4 -> HaskellProjectType
migrateHaskellProjectTypeFromV4 = \case
  HaskellProjectV4TypeReplOnly -> HaskellProjectTypeReplOnly
  HaskellProjectV4TypeBasic -> HaskellProjectTypeBasic $ SetUpHaskellBuild False

data ProjectTypeV5
  = PTV5Minimal
  | PTV5Elm ElmOptions
  | PTV5Haskell HaskellOptionsV4
  | PTV5Node NodePackageManager
  | PTV5Go SetUpGoBuild
  | PTV5Java JavaOptionsV2
  | PTV5Python PythonVersion
  | PTV5Rust
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) ProjectTypeV5

instance HasProjectSuperType ProjectTypeV5 where
  projectSuperType = \case
    PTV5Minimal -> PSTMinimal
    PTV5Elm _ -> PSTElm
    PTV5Haskell _ -> PSTHaskell
    PTV5Node _ -> PSTNode
    PTV5Go _ -> PSTGo
    PTV5Java {} -> PSTJava
    PTV5Python _ -> PSTPython
    PTV5Rust -> PSTRust

migrateProjectTypeFromV5 :: ProjectTypeV5 -> ProjectType
migrateProjectTypeFromV5 = \case
  PTV5Minimal -> Minimal
  PTV5Elm x -> Elm x
  PTV5Haskell x -> Haskell $ migrateHaskellOptionsFromV4 x
  PTV5Node x -> Node x
  PTV5Go x -> Go x
  PTV5Java x -> Java $ migrateJavaOptionsFromV2 x
  PTV5Python x -> Python x
  PTV5Rust -> Rust

data ProjectTypeV6
  = PTV6Minimal
  | PTV6Elm ElmOptions
  | PTV6Haskell HaskellOptions
  | PTV6Node NodePackageManager
  | PTV6Go SetUpGoBuild
  | PTV6Java JavaOptionsV2
  | PTV6Python PythonVersion
  | PTV6Rust
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Constructor AsIs) ProjectTypeV6

migrateProjectTypeFromV6 :: ProjectTypeV6 -> ProjectType
migrateProjectTypeFromV6 = \case
  PTV6Minimal -> Minimal
  PTV6Elm x -> Elm x
  PTV6Haskell x -> Haskell x
  PTV6Node x -> Node x
  PTV6Go x -> Go x
  PTV6Java x -> Java $ migrateJavaOptionsFromV2 x
  PTV6Python x -> Python x
  PTV6Rust -> Rust
