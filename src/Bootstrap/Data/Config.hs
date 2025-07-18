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
    ConfigV10 (ConfigV10),
    LoadConfigResult
      ( LoadConfigResultError,
        LoadConfigResultFound,
        LoadConfigResultNotFound
      ),
    NonFlakeConfigException,
    VersionedConfig (VersionedConfigV10),
    VersionedProjectType (VPT10),
    configV10ProjectName,
    configV10ProjectType,
    configV10SetUpContinuousIntegration,
    configV10SetUpPreCommitHooks,
    configV10SetUpVSCodeDevContainer,
    configV10Target,
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

makeConfigLenses 'ConfigV10

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
  VersionedConfigV10 $
    ConfigV10 a1 (VPT10 a2) a3 a4 a5 a6
