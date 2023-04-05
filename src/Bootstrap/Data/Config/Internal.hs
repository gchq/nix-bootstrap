{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Config.Internal where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable
      ( bootstrapContent,
        bootstrapName,
        bootstrapReason
      ),
  )
import Bootstrap.Data.Bootstrappable.BootstrapState
  ( BootstrapState
      ( stateContinuousIntegrationConfig,
        stateDevContainerConfig,
        statePreCommitHooksConfig,
        stateProjectName,
        stateProjectType,
        stateUseFlakes
      ),
    bootstrapStateCodec,
  )
import Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig,
  )
import Bootstrap.Data.DevContainer (DevContainerConfig)
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig)
import Bootstrap.Data.ProjectName (ProjectName)
import Bootstrap.Data.ProjectType (JavaOptions, ProjectType)
import Bootstrap.Monad (MonadBootstrap)
import Control.Lens (Iso', iso, makeLenses)
import Control.Monad.Catch (MonadThrow (throwM), catchAll, handleAll)
import Data.Singletons
  ( Sing,
    SingI (sing),
    SingKind (Demote, fromSing, toSing),
    SomeSing (SomeSing),
  )
import Dhall
  ( Encoder (Encoder, declared, embed),
    FromDhall,
    ToDhall (injectWith),
    auto,
    inject,
    input,
  )
import Dhall.Core (Expr, pretty)
import qualified Dhall.Core as DCore
import Dhall.Deriving
  ( CamelCase,
    Codec (Codec),
    DropPrefix,
    Field,
    type (<<<),
  )
import Dhall.Src (Src)
import System.Directory (doesFileExist)
import System.Terminal (MonadPrinter (putTextLn))
import qualified Toml as TOML

-- | The version of `Config` being used
data ConfigVersion
  = V1
  | V2

-- | Singled `ConfigVersion`
data SConfigVersion (configVersion :: ConfigVersion) where
  SV1 :: SConfigVersion 'V1
  SV2 :: SConfigVersion 'V2

type instance Sing = SConfigVersion

instance SingKind ConfigVersion where
  type Demote ConfigVersion = ConfigVersion
  fromSing = \case SV1 -> V1; SV2 -> V2
  toSing = \case V1 -> SomeSing SV1; V2 -> SomeSing SV2

-- | The most recent version of the config
type Current = 'V2

instance SingI Current where
  sing = SV2

-- | nix-bootstrap's configuration
type Config = VersionedConfig Current

-- | Config parameterised by `ConfigVersion`
data VersionedConfig version where
  VersionedConfigV1 :: BootstrapState -> VersionedConfig 'V1
  VersionedConfigV2 :: ConfigV2 -> VersionedConfig 'V2

deriving stock instance Eq (VersionedConfig version)

deriving stock instance Show (VersionedConfig version)

instance Bootstrappable Config where
  bootstrapName = const configPath
  bootstrapReason = const "This holds nix-bootstrap's configuration to ensure upgrades are reliable."
  bootstrapContent = pure . Right . (<> "\n") . (introComment <>) . pretty . extractUnions . embed inject
    where
      introComment :: Text
      introComment =
        unlines
          [ "-- This file was generated by nix-bootstrap.",
            "-- It should be checked into version control.",
            "-- It is used to aid migration between nix-bootstrap versions and preserve idempotence.",
            ""
          ]
      extractUnions :: Expr Src Void -> Expr Src Void
      extractUnions e =
        DCore.Let (DCore.makeBinding "JavaOptions" . declared $ inject @JavaOptions)
          . DCore.Let
            ( DCore.makeBinding "ProjectType"
                . runIdentity
                . replaceFullTypes
                $ declared (inject @ProjectType)
            )
          . runIdentity
          $ DCore.subExpressions replaceFullTypes e
      replaceFullTypes :: Expr Src Void -> Identity (Expr Src Void)
      replaceFullTypes =
        \case
          e@(DCore.Record _)
            | e == declared (inject @JavaOptions) -> Identity $ DCore.Var "JavaOptions"
            | otherwise -> DCore.subExpressions replaceFullTypes e
          e@(DCore.Field u@(DCore.Union _) fieldSelection)
            | u == declared (inject @ProjectType) ->
              Identity $ DCore.Field (DCore.Var "ProjectType") fieldSelection
            | otherwise -> DCore.subExpressions replaceFullTypes e
          e -> DCore.subExpressions replaceFullTypes e

instance ToDhall Config where
  injectWith inputNormaliser =
    let innerEncoder = injectWith inputNormaliser
     in Encoder
          { embed = \(VersionedConfigV2 c) -> embed innerEncoder c,
            declared = declared innerEncoder
          }

-- | The location of a bootstrapped config file
configPath :: FilePath
configPath = ".nix-bootstrap.dhall"

-- | Errors which occur during TOML decoding
newtype TomlDecodeException = TomlDecodeException {unTomlDecodeException :: [TOML.TomlDecodeError]}
  deriving stock (Show)

instance Exception TomlDecodeException where
  displayException = toString . TOML.prettyTomlDecodeErrors . unTomlDecodeException

-- | Parses the given text as a nix-bootstrap config file
parseVersionedConfig ::
  MonadBootstrap m =>
  SConfigVersion version ->
  Text ->
  m (Either SomeException (VersionedConfig version))
parseVersionedConfig v s = case v of
  SV1 ->
    pure
      . bimap (SomeException . TomlDecodeException) VersionedConfigV1
      $ TOML.decode bootstrapStateCodec s
  SV2 -> handleAll (pure . Left) . fmap (Right . VersionedConfigV2) . liftIO $ input auto s

-- | The second version of the config
data ConfigV2 = ConfigV2
  { _configV2ProjectName :: ProjectName,
    _configV2ProjectType :: ProjectType,
    _configV2SetUpPreCommitHooks :: PreCommitHooksConfig,
    _configV2SetUpContinuousIntegration :: ContinuousIntegrationConfig,
    _configV2SetUpVSCodeDevContainer :: DevContainerConfig,
    _configV2UseNixFlakes :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Field (CamelCase <<< DropPrefix "_configV2")) ConfigV2

-- | The outcome of trying to load the `Config`
data LoadConfigResult
  = -- | The config is up-to-date, or has been migrated from a previous version
    -- to the current version
    LoadConfigResultFound Config
  | -- | The config is invalid, and doesn't match any config version
    LoadConfigResultError SomeException
  | -- | There is no existing config
    LoadConfigResultNotFound
  deriving stock (Show)

makeLenses ''ConfigV2

-- | Isomorphism to the current config version
_Current :: Iso' Config ConfigV2
_Current = iso (\(VersionedConfigV2 c) -> c) VersionedConfigV2

-- | Loads the config from the appropriate file
loadConfig :: MonadBootstrap m => m LoadConfigResult
loadConfig = loadConfig' @Current sing

-- | Tries to load the specified config version, then tries previous versions
-- until one is found to succeed (or none are).
loadConfig' :: forall version m. MonadBootstrap m => SConfigVersion version -> m LoadConfigResult
loadConfig' nextToTry = do
  ((Right <$> loadConfigAtVersion nextToTry) `catchAll` (pure . Left)) >>= \case
    Right Nothing -> pure LoadConfigResultNotFound
    Right (Just c) -> pure $ LoadConfigResultFound c
    Left e -> tryPreviousConfigVersion e nextToTry
  where
    tryPreviousConfigVersion :: SomeException -> SConfigVersion v -> m LoadConfigResult
    tryPreviousConfigVersion e v = case v of
      SV2 -> loadConfig' SV1
      SV1 -> pure $ LoadConfigResultError e

-- | Loads and parses the config at the specified version.
--
-- ### Throws
--
-- * `IOException` if the file can't be loaded
-- * `SomeException` if the config can't be loaded or parsed.
loadConfigAtVersion :: MonadBootstrap m => SConfigVersion version -> m (Maybe Config)
loadConfigAtVersion v = do
  let path = case v of SV1 -> ".nix-bootstrap.toml"; _ -> configPath
  liftIO ((,) <$> doesFileExist ".nix-bootstrap.toml" <*> doesFileExist configPath) >>= \case
    (False, False) -> pure Nothing
    _ -> do
      fileContents <- decodeUtf8With lenientDecode <$> readFileBS path
      either throwM (pure . pure . upgradeConfig v) =<< showPath path =<< parseVersionedConfig v fileContents
  where
    showPath ::
      MonadBootstrap m =>
      FilePath ->
      Either SomeException (VersionedConfig version) ->
      m (Either SomeException (VersionedConfig version))
    showPath path c = c <$ putTextLn ("Loaded config from " <> toText path)

-- | Migrates an outdated config version to the current version
upgradeConfig :: SConfigVersion version -> VersionedConfig version -> Config
upgradeConfig _ = \case
  VersionedConfigV1 s ->
    VersionedConfigV2
      ConfigV2
        { _configV2ProjectName = stateProjectName s,
          _configV2ProjectType = stateProjectType s,
          _configV2SetUpPreCommitHooks = statePreCommitHooksConfig s,
          _configV2SetUpContinuousIntegration = stateContinuousIntegrationConfig s,
          _configV2SetUpVSCodeDevContainer = stateDevContainerConfig s,
          _configV2UseNixFlakes = stateUseFlakes s
        }
  c@(VersionedConfigV2 _) -> c
