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

    -- * Exceptions
    NonFlakeConfigException,
  )
where

import Bootstrap.Data.Config.Internal
  ( Config,
    ConfigV8 (ConfigV8),
    LoadConfigResult
      ( LoadConfigResultError,
        LoadConfigResultFound,
        LoadConfigResultNotFound
      ),
    NonFlakeConfigException,
    VersionedConfig (VersionedConfigV8),
    VersionedProjectType (VPT8),
    configV8ProjectName,
    configV8ProjectType,
    configV8SetUpContinuousIntegration,
    configV8SetUpPreCommitHooks,
    configV8SetUpVSCodeDevContainer,
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

makeConfigLenses 'ConfigV8

-- | Initialise a new `Config` from scratch
configFor ::
  ProjectName ->
  ProjectType ->
  PreCommitHooksConfig ->
  ContinuousIntegrationConfig ->
  DevContainerConfig ->
  Config
configFor a1 a2 a3 a4 a5 =
  VersionedConfigV8 $
    ConfigV8 a1 (VPT8 a2) a3 a4 a5
