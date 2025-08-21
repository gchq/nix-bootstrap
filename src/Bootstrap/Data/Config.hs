{-# LANGUAGE DataKinds #-}
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
    configTarget,

    -- * Exceptions
    NonFlakeConfigException,
  )
where

import Bootstrap.Data.Config.Internal
  ( Config,
    ConfigV11 (ConfigV11),
    LoadConfigResult
      ( LoadConfigResultError,
        LoadConfigResultFound,
        LoadConfigResultNotFound
      ),
    NonFlakeConfigException,
    VersionedConfig (VersionedConfigV11),
    VersionedProjectType (VPT11),
    configV11ProjectName,
    configV11ProjectType,
    configV11SetUpContinuousIntegration,
    configV11SetUpPreCommitHooks,
    configV11SetUpVSCodeDevContainer,
    configV11Target,
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
import Bootstrap.Data.Target (Target)

makeConfigLenses 'ConfigV11

-- | Initialise a new `Config` from scratch
configFor ::
  ProjectName ->
  ProjectType ->
  PreCommitHooksConfig ->
  ContinuousIntegrationConfig ->
  DevContainerConfig ->
  Target ->
  Config
configFor a1 a2 a3 a4 a5 a6 =
  VersionedConfigV11 $
    ConfigV11 a1 (VPT11 a2) a3 a4 a5 a6
