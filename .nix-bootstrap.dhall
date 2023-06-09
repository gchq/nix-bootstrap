-- This file was generated by nix-bootstrap.
-- It should be checked into version control.
-- It is used to aid migration between nix-bootstrap versions and preserve idempotence.

let JavaOptions =
      { installMinishift : Bool
      , installLombok : Bool
      , setUpJavaBuild : < SetUpJavaBuild : Text | NoJavaBuild >
      }

let ProjectType =
      < Minimal
      | Node : < NPM | PNPm | Yarn >
      | Go : Bool
      | Java : JavaOptions
      | Python
      >

in  { projectName = "nix-bootstrap"
    , projectType = ProjectType.Minimal
    , setUpPreCommitHooks = True
    , setUpContinuousIntegration = True
    , setUpVSCodeDevContainer = True
    , useNixFlakes = True
    }
