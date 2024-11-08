-- | Copyright : (c) Crown Copyright GCHQ
module Test.Util.RunConfig (rcDefault) where

import Bootstrap.Cli
  ( RunConfig
      ( RunConfig,
        rcAllowDirty,
        rcFromScratch,
        rcNonInteractive,
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
      rcWithDevContainer = Nothing
    }
