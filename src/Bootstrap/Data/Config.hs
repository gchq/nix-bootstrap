{-# LANGUAGE TemplateHaskell #-}

-- |
-- Description : Utilities for working with nix-bootstrap's configuration
-- Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Config
  ( -- * Config Data Types
    Config,

    -- * Loading an existing `Config`
    LoadConfigResult (..),
    loadConfig,

    -- * Creating a `Config` from scratch
    configFor,

    -- * Accessing `Config` values
    configProjectName,
    configProjectType,
    configSetUpPreCommitHooks,
    configSetUpContinuousIntegration,
    configSetUpVSCodeDevContainer,
    configUseNixFlakes,
  )
where

import Bootstrap.Data.Config.Internal
  ( Config,
    ConfigV2 (ConfigV2),
    LoadConfigResult
      ( LoadConfigResultError,
        LoadConfigResultFound,
        LoadConfigResultNotFound
      ),
    VersionedConfig (VersionedConfigV2),
    configV2ProjectName,
    configV2ProjectType,
    configV2SetUpContinuousIntegration,
    configV2SetUpPreCommitHooks,
    configV2SetUpVSCodeDevContainer,
    configV2UseNixFlakes,
    loadConfig,
    _Current,
  )
import Bootstrap.Data.Config.Internal.TH (makeConfigLenses)
import Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig,
  )
import Bootstrap.Data.DevContainer (DevContainerConfig)
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig)
import Bootstrap.Data.ProjectName (ProjectName)
import Bootstrap.Data.ProjectType (ProjectType)

makeConfigLenses 'ConfigV2

-- | Initialise a new `Config` from scratch
configFor ::
  ProjectName ->
  ProjectType ->
  PreCommitHooksConfig ->
  ContinuousIntegrationConfig ->
  DevContainerConfig ->
  -- | Whether to enable Nix Flakes
  Bool ->
  Config
configFor a1 a2 a3 a4 a5 a6 =
  VersionedConfigV2 $
    ConfigV2 a1 a2 a3 a4 a5 a6
