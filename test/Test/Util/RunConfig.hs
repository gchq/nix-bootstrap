-- | Copyright : (c) Crown Copyright GCHQ
module Test.Util.RunConfig (rcDefault, rcWithFlakes) where

import Bootstrap.Cli
  ( RunConfig
      ( RunConfig,
        rcAllowDirty,
        rcFromScratch,
        rcNonInteractive,
        rcUseFlakes,
        rcWithDevContainer
      ),
  )

-- | A RunConfig with all its fields set to the simplest possible values
rcDefault :: RunConfig
rcDefault =
  RunConfig
    { rcAllowDirty = False,
      rcFromScratch = False,
      rcNonInteractive = False,
      rcUseFlakes = False,
      rcWithDevContainer = Nothing
    }

rcWithFlakes :: RunConfig
rcWithFlakes = rcDefault {rcUseFlakes = True}
