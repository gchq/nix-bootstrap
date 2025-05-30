-- | Copyright : (c) Crown Copyright GCHQ
--   Description : Defines the most recent config version, from which
--   many of the config types are generated.
module Bootstrap.Data.Config.Internal.CurrentVersion (currentVersionNumber, versionUniverse) where

-- | The most recent version of the config
currentVersionNumber :: Int
currentVersionNumber = 9

versionUniverse :: [Int]
versionUniverse = [1 .. currentVersionNumber]
