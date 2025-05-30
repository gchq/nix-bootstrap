{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Bootstrap.Data.Config.Internal.CurrentVersion
  ( currentVersionNumber,
    versionUniverse,
  )
import Bootstrap.Data.Config.Internal.THHelpers (isoForName)
import Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig,
  )
import Bootstrap.Data.DevContainer (DevContainerConfig)
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig)
import Bootstrap.Data.ProjectName (ProjectName)
import Bootstrap.Data.ProjectType
  ( ElmMode,
    ElmOptions,
    HaskellOptions,
    HaskellProjectType,
    JavaOptions,
    NodePackageManager,
    ProjectType,
    ProjectTypeV2,
    ProjectTypeV3,
    ProjectTypeV4,
    ProjectTypeV5,
    ProjectTypeV6,
    migrateProjectTypeFromV2,
    migrateProjectTypeFromV3,
    migrateProjectTypeFromV4,
    migrateProjectTypeFromV5,
    migrateProjectTypeFromV6,
  )
import Bootstrap.Data.Target (Target (TargetDefault))
import Bootstrap.Monad (MonadBootstrap)
import Control.Lens (Iso', makeLenses)
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM), catchAll, handleAll)
import Data.Singletons
  ( Sing,
    SingI (sing),
    SingKind (Demote, fromSing, toSing),
    SomeSing (SomeSing),
  )
import Dhall
  ( Decoder (Decoder, expected, extract),
    Encoder (Encoder, declared, embed),
    FromDhall (autoWith),
    InputNormalizer,
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
import qualified Language.Haskell.TH as TH
import System.Directory (doesFileExist)
import System.Terminal (MonadPrinter (putTextLn))
import Text.Show (Show (show))
import qualified Toml as TOML

{-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Adding a new config version?
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

The first thing to do is update currentVersionNumber in Bootstrap.Data.Config.Internal.CurrentVersion.
Then come back here and start following the compiler errors.
-}

{- data ConfigVersion = V1 | V2 | ...

   Creates constructors up to (including) `currentVersionNumber` -}
$( TH.newName "ConfigVersion" >>= \decName ->
     one
       <$> TH.dataD_doc
         (pure [])
         decName
         []
         Nothing
         (versionUniverse <&> \n -> ((`TH.NormalC` []) <$> TH.newName ('V' : Prelude.show n), Nothing, []))
         []
         (Just "The version of `Config` being used")
 )

-- Singled `ConfigVersion`
$( TH.newName "SConfigVersion" >>= \decName -> do
     let makeCon n = do
           conName <- TH.newName ("SV" <> Prelude.show n)
           Just unsingled <- TH.lookupValueName ('V' : Prelude.show n)
           TH.gadtC [conName] [] (pure $ TH.AppT (TH.ConT decName) (TH.PromotedT unsingled))
     one
       <$> TH.dataD_doc
         (pure [])
         decName
         [TH.newName "configVersion" <&> \name -> TH.KindedTV name TH.BndrReq (TH.ConT ''ConfigVersion)]
         Nothing
         (versionUniverse <&> \n -> (makeCon n, Nothing, []))
         []
         (Just "Singled `ConfigVersion`")
 )

type instance Sing = SConfigVersion

-- Instance of SingKind ConfigVersion
$( do
     Just fromSingName <- TH.lookupValueName "fromSing"
     Just toSingName <- TH.lookupValueName "toSing"
     one
       <$> TH.instanceD
         (pure [])
         [t|SingKind ConfigVersion|]
         [ TH.tySynInstD (TH.tySynEqn Nothing [t|Demote ConfigVersion|] [t|ConfigVersion|]),
           TH.funD fromSingName $
             versionUniverse <&> \n ->
               do
                 Just sv <- TH.lookupValueName $ "SV" <> Prelude.show n
                 Just v <- TH.lookupValueName $ "V" <> Prelude.show n
                 pure $ TH.Clause [TH.ConP sv [] []] (TH.NormalB $ TH.ConE v) [],
           TH.funD
             toSingName
             $ versionUniverse <&> \n -> do
               Just someSing <- TH.lookupValueName "SomeSing"
               Just sv <- TH.lookupValueName $ "SV" <> Prelude.show n
               Just v <- TH.lookupValueName $ "V" <> Prelude.show n
               pure $ TH.Clause [TH.ConP v [] []] (TH.NormalB $ TH.ConE someSing `TH.AppE` TH.ConE sv) []
         ]
 )

-- Type synonym for Vx, where x is `currentVersionNumber`
$( TH.newName "Current" >>= \decName ->
     one
       <$> TH.withDecDoc
         "The most recent version of the config"
         ( TH.tySynD
             decName
             []
             do
               Just current <- TH.lookupValueName $ 'V' : Prelude.show currentVersionNumber
               pure $ TH.PromotedT current
         )
 )

-- Instance of SingI for the current config version
$( one
     <$> TH.instanceD
       (pure [])
       [t|SingI Current|]
       [ do
           Just singName <- TH.lookupValueName "sing"
           Just svCurrent <- TH.lookupValueName $ "SV" <> Prelude.show currentVersionNumber
           TH.funD singName [pure $ TH.Clause [] (TH.NormalB $ TH.ConE svCurrent) []]
       ]
 )

-- | nix-bootstrap's configuration
type Config = VersionedConfig Current

-- | Config parameterised by `ConfigVersion`
data VersionedConfig version where
  VersionedConfigV1 :: BootstrapState -> VersionedConfig 'V1
  VersionedConfigV2 :: ConfigV2 -> VersionedConfig 'V2
  VersionedConfigV3 :: ConfigV3Plus 'V3 -> VersionedConfig 'V3
  VersionedConfigV4 :: ConfigV3Plus 'V4 -> VersionedConfig 'V4
  VersionedConfigV5 :: ConfigV3Plus 'V5 -> VersionedConfig 'V5
  VersionedConfigV6 :: ConfigV3Plus 'V6 -> VersionedConfig 'V6
  VersionedConfigV7 :: ConfigV3Plus 'V7 -> VersionedConfig 'V7
  VersionedConfigV8 :: ConfigV8 -> VersionedConfig 'V8
  VersionedConfigV9 :: ConfigV9 -> VersionedConfig 'V9

deriving stock instance (Eq (VersionedProjectType version)) => Eq (VersionedConfig version)

deriving stock instance (Show (VersionedProjectType version)) => Show (VersionedConfig version)

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
            "-- It is used to aid migration between nix-bootstrap versions and preserve idempotence."
          ]
      extractUnions :: Expr Src Void -> Expr Src Void
      extractUnions e =
        DCore.Let (DCore.makeBinding "NodePackageManager" . declared $ inject @NodePackageManager)
          . DCore.Let
            ( DCore.makeBinding "ElmMode"
                . runIdentity
                . DCore.subExpressions replaceFullTypes
                . declared
                $ inject @ElmMode
            )
          . DCore.Let
            ( DCore.makeBinding "ElmOptions"
                . runIdentity
                . DCore.subExpressions replaceFullTypes
                . declared
                $ inject @ElmOptions
            )
          . DCore.Let
            ( DCore.makeBinding "HaskellProjectType"
                . runIdentity
                . DCore.subExpressions replaceFullTypes
                . declared
                $ inject @HaskellProjectType
            )
          . DCore.Let
            ( DCore.makeBinding "HaskellOptions"
                . runIdentity
                . DCore.subExpressions replaceFullTypes
                . declared
                $ inject @HaskellOptions
            )
          . DCore.Let (DCore.makeBinding "JavaOptions" . declared $ inject @JavaOptions)
          . DCore.Let
            ( DCore.makeBinding "ProjectType"
                . runIdentity
                . DCore.subExpressions replaceFullTypes
                . declared
                $ inject @ProjectType
            )
          . runIdentity
          $ DCore.subExpressions replaceFullTypes e
      replaceFullTypes :: Expr Src Void -> Identity (Expr Src Void)
      replaceFullTypes =
        \case
          e@(DCore.Record _)
            | e == declared (inject @ElmOptions) -> Identity $ DCore.Var "ElmOptions"
            | e == declared (inject @HaskellOptions) -> Identity $ DCore.Var "HaskellOptions"
            | e == declared (inject @JavaOptions) -> Identity $ DCore.Var "JavaOptions"
            | otherwise -> DCore.subExpressions replaceFullTypes e
          e@(DCore.Field u@(DCore.Union _) fieldSelection)
            | u == declared (inject @ProjectType) ->
                Identity $ DCore.Field (DCore.Var "ProjectType") fieldSelection
            | otherwise -> DCore.subExpressions replaceFullTypes e
          e@(DCore.Union _)
            | e == declared (inject @ElmMode) -> Identity $ DCore.Var "ElmMode"
            | e == declared (inject @HaskellProjectType) -> Identity $ DCore.Var "HaskellProjectType"
            | e == declared (inject @NodePackageManager) -> Identity $ DCore.Var "NodePackageManager"
          e -> DCore.subExpressions replaceFullTypes e

instance ToDhall Config where
  injectWith inputNormaliser =
    let innerEncoder = injectWith inputNormaliser
     in Encoder
          { embed = \(VersionedConfigV9 c) -> embed innerEncoder c,
            declared = declared innerEncoder
          }

data VersionedProjectType (version :: ConfigVersion) where
  VPT3 :: ProjectTypeV3 -> VersionedProjectType 'V3
  VPT4 :: ProjectTypeV4 -> VersionedProjectType 'V4
  VPT5 :: ProjectTypeV5 -> VersionedProjectType 'V5
  VPT6 :: ProjectTypeV6 -> VersionedProjectType 'V6
  VPT7 :: ProjectType -> VersionedProjectType 'V7
  VPT8 :: ProjectType -> VersionedProjectType 'V8
  VPT9 :: ProjectType -> VersionedProjectType 'V9

instance FromDhall (VersionedProjectType 'V3) where
  autoWith = versionedProjectTypeFromDhall (Proxy @ProjectTypeV3) VPT3

instance FromDhall (VersionedProjectType 'V4) where
  autoWith = versionedProjectTypeFromDhall (Proxy @ProjectTypeV4) VPT4

instance FromDhall (VersionedProjectType 'V5) where
  autoWith = versionedProjectTypeFromDhall (Proxy @ProjectTypeV5) VPT5

instance FromDhall (VersionedProjectType 'V6) where
  autoWith = versionedProjectTypeFromDhall (Proxy @ProjectTypeV6) VPT6

instance FromDhall (VersionedProjectType 'V7) where
  autoWith = versionedProjectTypeFromDhall (Proxy @ProjectType) VPT7

instance FromDhall (VersionedProjectType 'V8) where
  autoWith = versionedProjectTypeFromDhall (Proxy @ProjectType) VPT8

instance FromDhall (VersionedProjectType 'V9) where
  autoWith = versionedProjectTypeFromDhall (Proxy @ProjectType) VPT9

instance ToDhall (VersionedProjectType 'V3) where
  injectWith = versionedProjectTypeToDhall (Proxy @ProjectTypeV3) (\(VPT3 x) -> x)

instance ToDhall (VersionedProjectType 'V4) where
  injectWith = versionedProjectTypeToDhall (Proxy @ProjectTypeV4) (\(VPT4 x) -> x)

instance ToDhall (VersionedProjectType 'V5) where
  injectWith = versionedProjectTypeToDhall (Proxy @ProjectTypeV5) (\(VPT5 x) -> x)

instance ToDhall (VersionedProjectType 'V6) where
  injectWith = versionedProjectTypeToDhall (Proxy @ProjectTypeV6) (\(VPT6 x) -> x)

instance ToDhall (VersionedProjectType 'V7) where
  injectWith = versionedProjectTypeToDhall (Proxy @ProjectType) (\(VPT7 x) -> x)

instance ToDhall (VersionedProjectType 'V8) where
  injectWith = versionedProjectTypeToDhall (Proxy @ProjectType) (\(VPT8 x) -> x)

instance ToDhall (VersionedProjectType 'V9) where
  injectWith = versionedProjectTypeToDhall (Proxy @ProjectType) (\(VPT9 x) -> x)

versionedProjectTypeFromDhall ::
  forall underlying version.
  (FromDhall underlying) =>
  Proxy underlying ->
  (underlying -> VersionedProjectType version) ->
  InputNormalizer ->
  Decoder (VersionedProjectType version)
versionedProjectTypeFromDhall Proxy constructor normaliser =
  Decoder {extract = constructor <<$>> extract, ..}
  where
    Decoder {..} = autoWith @underlying normaliser

versionedProjectTypeToDhall ::
  forall underlying version.
  (ToDhall underlying) =>
  Proxy underlying ->
  (VersionedProjectType version -> underlying) ->
  InputNormalizer ->
  Encoder (VersionedProjectType version)
versionedProjectTypeToDhall Proxy unwrap normaliser =
  Encoder {embed = embed . unwrap, ..}
  where
    Encoder {..} = injectWith @underlying normaliser

{- iso (\(VPT9 x) -> x) VPT9
   (or whatever the current version is)
-}
_VersionedProjectType :: Iso' (VersionedProjectType Current) ProjectType
_VersionedProjectType =
  $( do
       Just vptCurrentName <- TH.lookupValueName $ "VPT" <> Prelude.show currentVersionNumber
       isoForName vptCurrentName
   )

-- | The location of a bootstrapped config file
configPath :: FilePath
configPath = ".nix-bootstrap.dhall"

-- | Errors which occur during TOML decoding
newtype TomlDecodeException = TomlDecodeException {unTomlDecodeException :: [TOML.TomlDecodeError]}
  deriving stock (Show)

instance Exception TomlDecodeException where
  displayException = toString . TOML.prettyTomlDecodeErrors . unTomlDecodeException

-- | The second version of the config
data ConfigV2 = ConfigV2
  { _configV2ProjectName :: ProjectName,
    _configV2ProjectType :: ProjectTypeV2,
    _configV2SetUpPreCommitHooks :: PreCommitHooksConfig,
    _configV2SetUpContinuousIntegration :: ContinuousIntegrationConfig,
    _configV2SetUpVSCodeDevContainer :: DevContainerConfig,
    _configV2UseNixFlakes :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving (FromDhall, ToDhall) via Codec (Field (CamelCase <<< DropPrefix "_configV2")) ConfigV2

-- | The third version of the config
data ConfigV3Plus (version :: ConfigVersion) = ConfigV3Plus
  { _configV3ProjectName :: ProjectName,
    _configV3ProjectType :: VersionedProjectType version,
    _configV3SetUpPreCommitHooks :: PreCommitHooksConfig,
    _configV3SetUpContinuousIntegration :: ContinuousIntegrationConfig,
    _configV3SetUpVSCodeDevContainer :: DevContainerConfig,
    _configV3UseNixFlakes :: Bool
  }
  deriving stock (Generic)

deriving stock instance (Eq (VersionedProjectType version)) => Eq (ConfigV3Plus version)

deriving stock instance (Show (VersionedProjectType version)) => Show (ConfigV3Plus version)

deriving via
  (Codec (Field (CamelCase <<< DropPrefix "_configV3")) (ConfigV3Plus version))
  instance
    (FromDhall (VersionedProjectType version)) => FromDhall (ConfigV3Plus version)

deriving via
  (Codec (Field (CamelCase <<< DropPrefix "_configV3")) (ConfigV3Plus version))
  instance
    (ToDhall (VersionedProjectType version)) => ToDhall (ConfigV3Plus version)

-- | The eighth version of the config
data ConfigV8 = ConfigV8
  { _configV8ProjectName :: ProjectName,
    _configV8ProjectType :: VersionedProjectType 'V8,
    _configV8SetUpPreCommitHooks :: PreCommitHooksConfig,
    _configV8SetUpContinuousIntegration :: ContinuousIntegrationConfig,
    _configV8SetUpVSCodeDevContainer :: DevContainerConfig
  }
  deriving stock (Generic)
  deriving (FromDhall, ToDhall) via Codec (Field (CamelCase <<< DropPrefix "_configV8")) ConfigV8

deriving stock instance (Eq (VersionedProjectType 'V8)) => Eq ConfigV8

deriving stock instance (Show (VersionedProjectType 'V8)) => Show ConfigV8

-- | The ninth version of the config
data ConfigV9 = ConfigV9
  { _configV9ProjectName :: ProjectName,
    _configV9ProjectType :: VersionedProjectType 'V9,
    _configV9SetUpPreCommitHooks :: PreCommitHooksConfig,
    _configV9SetUpContinuousIntegration :: ContinuousIntegrationConfig,
    _configV9SetUpVSCodeDevContainer :: DevContainerConfig,
    _configV9Target :: Target
  }
  deriving stock (Generic)
  deriving (FromDhall, ToDhall) via Codec (Field (CamelCase <<< DropPrefix "_configV9")) ConfigV9

deriving stock instance (Eq (VersionedProjectType 'V9)) => Eq ConfigV9

deriving stock instance (Show (VersionedProjectType 'V9)) => Show ConfigV9

-- | Used by parseVersionConfig to parse when the version is known statically
parseVersionedConfigFor ::
  (FromDhall config, MonadCatch m, MonadIO m) =>
  (config -> versionedConfig) ->
  Text ->
  m (Either SomeException versionedConfig)
parseVersionedConfigFor constructor contents = handleAll (pure . Left) . fmap (Right . constructor) . liftIO $ input auto contents

-- | Parses the given text as a nix-bootstrap config file
parseVersionedConfig ::
  forall m version.
  (MonadBootstrap m) =>
  SConfigVersion version ->
  Text ->
  m (Either SomeException (VersionedConfig version))
parseVersionedConfig v contents =
  $( TH.caseE [|v|] $
       ( do
           pat <- [p|SV1|]
           body <-
             TH.NormalB
               <$> [|
                 pure
                   . bimap (SomeException . TomlDecodeException) VersionedConfigV1
                   $ TOML.decode bootstrapStateCodec contents
                 |]
           pure $ TH.Match pat body []
       )
         : ( [2 .. currentVersionNumber] <&> \n -> do
               Just svName <- TH.lookupValueName $ "SV" <> Prelude.show n
               Just vcName <- TH.lookupValueName $ "VersionedConfigV" <> Prelude.show n
               body <-
                 TH.NormalB
                   <$> TH.appE
                     (TH.appE [|parseVersionedConfigFor|] (pure $ TH.ConE vcName))
                     [|contents|]
               pure $ TH.Match (TH.ConP svName [] []) body []
           )
   )

-- | An exception thrown when a config specifies that flakes are not to be used;
-- this is an exception because non-flake support has been withdrawn and migration
-- to flakes is not automatic.
data NonFlakeConfigException = NonFlakeConfigException

instance Show NonFlakeConfigException where
  show NonFlakeConfigException =
    "NonFlakeConfigException: "
      <> "Cannot upgrade config; nix-bootstrap now requires all projects to use flakes. To force an upgrade, "
      <> "set the use flakes parameter in your config file to true. Note that this will result in some old infrastructure "
      <> "remaining for you to clean up."

instance Exception NonFlakeConfigException

-- | The outcome of trying to load the `Config`
data LoadConfigResult
  = -- | The config is up-to-date, or has been migrated from a previous version
    -- to the current version
    LoadConfigResultFound Config
  | -- | The config is invalid, and doesn't match any config version
    LoadConfigResultError SomeException
  | -- | There is no existing config
    LoadConfigResultNotFound

deriving stock instance (Show Config) => Show LoadConfigResult

$( do
     Just nameToMakeLensesFor <- TH.lookupTypeName $ "ConfigV" <> Prelude.show currentVersionNumber
     makeLenses nameToMakeLensesFor
 )

{- _Current :: Iso' Config ConfigV9
   _Current = iso (\(VersionedConfigV9 c) -> c) VersionedConfigV9
   (or whatever the current version is)
-}
$( do
     decName <- TH.newName "_Current"
     Just configVxTypeName <- TH.lookupTypeName $ "ConfigV" <> Prelude.show currentVersionNumber
     Just versionedConfigName <- TH.lookupValueName $ "VersionedConfigV" <> Prelude.show currentVersionNumber
     sigD <- TH.sigD decName $ [t|Iso' Config|] `TH.appT` TH.conT configVxTypeName
     funD <- TH.funD_doc decName [TH.clause [] (TH.NormalB <$> isoForName versionedConfigName) []] (Just "Isomorphism to the current config version") []
     pure [sigD, funD]
 )

-- | Loads the config from the appropriate file
loadConfig :: (MonadBootstrap m) => m LoadConfigResult
loadConfig = loadConfig' @Current sing

-- | Tries to load the specified config version, then tries previous versions
-- until one is found to succeed (or none are).
loadConfig' :: forall version m. (MonadBootstrap m) => SConfigVersion version -> m LoadConfigResult
loadConfig' nextToTry = do
  ((Right <$> loadConfigAtVersion nextToTry) `catchAll` (pure . Left)) >>= \case
    Right Nothing -> pure LoadConfigResultNotFound
    Right (Just c) -> pure $ LoadConfigResultFound c
    Left e -> tryPreviousConfigVersion e nextToTry

tryPreviousConfigVersion :: (MonadBootstrap m) => SomeException -> SConfigVersion v -> m LoadConfigResult
tryPreviousConfigVersion e v = case fromException e of
  Just NonFlakeConfigException -> pure $ LoadConfigResultError e
  Nothing ->
    $( TH.caseE [|v|] $
         (TH.Match <$> [p|SV1|] <*> (TH.NormalB <$> [|pure $ LoadConfigResultError e|]) <*> pure [])
           : ( [2 .. currentVersionNumber] <&> \n -> do
                 Just svName <- TH.lookupValueName $ "SV" <> Prelude.show n
                 Just previousSvName <- TH.lookupValueName $ "SV" <> Prelude.show (n - 1)
                 body <-
                   TH.NormalB
                     <$> TH.appE [|loadConfig'|] (pure $ TH.ConE previousSvName)
                 pure $ TH.Match (TH.ConP svName [] []) body []
             )
     )

-- | Loads and parses the config at the specified version.
--
-- ### Throws
--
-- * `IOException` if the file can't be loaded.
-- * `NonFlakeConfigException` if the config was set up not to use flakes, as this is no longer supported.
-- * `SomeException` if the config can't be loaded or parsed.
loadConfigAtVersion :: (MonadBootstrap m) => SConfigVersion version -> m (Maybe Config)
loadConfigAtVersion v = do
  let path = case v of SV1 -> ".nix-bootstrap.toml"; _ -> configPath
  liftIO ((,) <$> doesFileExist ".nix-bootstrap.toml" <*> doesFileExist configPath) >>= \case
    (False, False) -> pure Nothing
    _ -> do
      fileContents <- decodeUtf8With lenientDecode <$> readFileBS path
      either throwM (pure . pure)
        =<< either throwM (pure . upgradeConfig v)
        =<< showPath path
        =<< parseVersionedConfig v fileContents
  where
    showPath ::
      (MonadBootstrap m) =>
      FilePath ->
      Either SomeException (VersionedConfig version) ->
      m (Either SomeException (VersionedConfig version))
    showPath path c = c <$ putTextLn ("Loaded config from " <> toText path)

-- | Migrates an outdated config version to the current version
upgradeConfig :: SConfigVersion version -> VersionedConfig version -> Either NonFlakeConfigException Config
upgradeConfig _ =
  \case
    (False, _) -> Left NonFlakeConfigException
    (True, c) -> Right c
    . upgradeConfig'
  where
    upgradeConfig' :: VersionedConfig version -> (Bool, Config)
    upgradeConfig' = \case
      VersionedConfigV1 s ->
        ( stateUseFlakes s,
          VersionedConfigV9
            ConfigV9
              { _configV9ProjectName = stateProjectName s,
                _configV9ProjectType = VPT9 . migrateProjectTypeFromV2 $ stateProjectType s,
                _configV9SetUpPreCommitHooks = statePreCommitHooksConfig s,
                _configV9SetUpContinuousIntegration = stateContinuousIntegrationConfig s,
                _configV9SetUpVSCodeDevContainer = stateDevContainerConfig s,
                _configV9Target = TargetDefault
              }
        )
      VersionedConfigV2 ConfigV2 {..} ->
        ( _configV2UseNixFlakes,
          VersionedConfigV9
            ConfigV9
              { _configV9ProjectName = _configV2ProjectName,
                _configV9ProjectType = VPT9 $ migrateProjectTypeFromV2 _configV2ProjectType,
                _configV9SetUpPreCommitHooks = _configV2SetUpPreCommitHooks,
                _configV9SetUpContinuousIntegration = _configV2SetUpContinuousIntegration,
                _configV9SetUpVSCodeDevContainer = _configV2SetUpVSCodeDevContainer,
                _configV9Target = TargetDefault
              }
        )
      (VersionedConfigV3 c) -> migrateFromV3Plus c (\(VPT3 x) -> x) migrateProjectTypeFromV3
      (VersionedConfigV4 c) -> migrateFromV3Plus c (\(VPT4 x) -> x) migrateProjectTypeFromV4
      (VersionedConfigV5 c) -> migrateFromV3Plus c (\(VPT5 x) -> x) migrateProjectTypeFromV5
      (VersionedConfigV6 c) -> migrateFromV3Plus c (\(VPT6 x) -> x) migrateProjectTypeFromV6
      (VersionedConfigV7 c) -> migrateFromV3Plus c (\(VPT7 x) -> x) id
      (VersionedConfigV8 ConfigV8 {..}) ->
        ( True,
          VersionedConfigV9
            ( ConfigV9
                { _configV9ProjectName = _configV8ProjectName,
                  _configV9ProjectType = let VPT8 pt = _configV8ProjectType in VPT9 pt,
                  _configV9SetUpPreCommitHooks = _configV8SetUpPreCommitHooks,
                  _configV9SetUpContinuousIntegration = _configV8SetUpContinuousIntegration,
                  _configV9SetUpVSCodeDevContainer = _configV8SetUpVSCodeDevContainer,
                  _configV9Target = TargetDefault
                }
            )
        )
      c@(VersionedConfigV9 _) -> (True, c)
    migrateFromV3Plus ConfigV3Plus {..} deconstruct migrate =
      let oldProjectType = deconstruct _configV3ProjectType
       in ( _configV3UseNixFlakes,
            VersionedConfigV9
              ( ConfigV9
                  { _configV9ProjectName = _configV3ProjectName,
                    _configV9ProjectType = VPT9 $ migrate oldProjectType,
                    _configV9SetUpPreCommitHooks = _configV3SetUpPreCommitHooks,
                    _configV9SetUpContinuousIntegration = _configV3SetUpContinuousIntegration,
                    _configV9SetUpVSCodeDevContainer = _configV3SetUpVSCodeDevContainer,
                    _configV9Target = TargetDefault
                  }
              )
          )
