-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.BootstrapState
  {-# DEPRECATED "Use Bootstrap.Data.Config instead." #-}
  ( BootstrapState
      ( stateVersion,
        stateProjectName,
        stateProjectType,
        statePreCommitHooksConfig,
        stateContinuousIntegrationConfig,
        stateDevContainerConfig,
        stateUseFlakes
      ),
    bootstrapStateCodec,
    bootstrapStateFor,
    unBootstrapVersion,
    BootstrapVersion,
    bootstrapStateFileName,
  )
where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason))
import Bootstrap.Data.ContinuousIntegration (ContinuousIntegrationConfig, continuousIntegrationConfigCodec)
import Bootstrap.Data.DevContainer (DevContainerConfig, devContainerConfigCodec)
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig, preCommitHooksConfigCodec)
import Bootstrap.Data.ProjectName (ProjectName)
import qualified Bootstrap.Data.ProjectName as ProjectName
import Bootstrap.Data.ProjectType (ProjectTypeV2, projectTypeCodec)
import Data.Version (showVersion)
import Paths_nix_bootstrap (version)
import Toml (TomlCodec, (.=))
import qualified Toml

data BootstrapState = BootstrapState
  { stateVersion :: BootstrapVersion,
    stateProjectName :: ProjectName,
    stateProjectType :: ProjectTypeV2,
    statePreCommitHooksConfig :: PreCommitHooksConfig,
    stateContinuousIntegrationConfig :: ContinuousIntegrationConfig,
    stateDevContainerConfig :: DevContainerConfig,
    stateUseFlakes :: Bool
  }
  deriving stock (Eq, Show)

instance Bootstrappable BootstrapState where
  bootstrapName = const bootstrapStateFileName
  bootstrapReason = const "This holds nix-bootstrap's configuration to ensure upgrades are reliable."
  bootstrapContent state = pure . Right $ introComment <> Toml.encode bootstrapStateCodec state
    where
      introComment :: Text
      introComment =
        unlines
          [ "# This file was generated by nix-bootstrap.",
            "# It should be checked into version control.",
            "# It is used to aid migration between nix-bootstrap versions and preserve idempotence.",
            ""
          ]

bootstrapStateFileName :: FilePath
bootstrapStateFileName = ".nix-bootstrap.toml"

bootstrapStateCodec :: TomlCodec BootstrapState
bootstrapStateCodec =
  BootstrapState
    <$> Toml.diwrap (Toml.string "version") .= stateVersion
    <*> Toml.match ProjectName.tomlBiMap "projectName" .= stateProjectName
    <*> projectTypeCodec .= stateProjectType
    <*> preCommitHooksConfigCodec .= statePreCommitHooksConfig
    <*> continuousIntegrationConfigCodec .= stateContinuousIntegrationConfig
    <*> devContainerConfigCodec .= stateDevContainerConfig
    <*> Toml.diwrap (Toml.bool "useFlakes") .= stateUseFlakes

newtype BootstrapVersion = BootstrapVersion {unBootstrapVersion :: String}
  deriving stock (Eq, Show)

bootstrapStateFor ::
  ProjectName ->
  ProjectTypeV2 ->
  PreCommitHooksConfig ->
  ContinuousIntegrationConfig ->
  DevContainerConfig ->
  Bool ->
  BootstrapState
bootstrapStateFor = BootstrapState (BootstrapVersion (showVersion version))
