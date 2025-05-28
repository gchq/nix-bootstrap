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
    ConfigV9 (ConfigV9),
    LoadConfigResult
      ( LoadConfigResultError,
        LoadConfigResultFound,
        LoadConfigResultNotFound
      ),
    NonFlakeConfigException,
    VersionedConfig (VersionedConfigV9),
    VersionedProjectType (VPT9),
    configV9ProjectName,
    configV9ProjectType,
    configV9SetUpContinuousIntegration,
    configV9SetUpPreCommitHooks,
    configV9SetUpVSCodeDevContainer,
    configV9Target,
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

makeConfigLenses 'ConfigV9

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
  VersionedConfigV9 $
    ConfigV9 a1 (VPT9 a2) a3 a4 a5 a6
