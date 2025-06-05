{-# LANGUAGE TypeApplications #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Readme
  ( Readme
      ( Readme,
        readmeProjectName,
        readmeProjectType,
        readmeHasDevContainer,
        readmeMBuildPlan
      ),
  )
where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason))
import Bootstrap.Data.BuildPlan (BuildPlan, toReasonTree)
import Bootstrap.Data.DevContainer (DevContainerConfig, unDevContainerConfig)
import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeBare, ElmModeNode),
    ElmOptions (ElmOptions, elmOptionElmMode, elmOptionProvideElmReview),
    HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly, HaskellProjectTypeServer),
    ProjectType (Elm, Go, Haskell, Python),
    SetUpGoBuild (SetUpGoBuild),
    SetUpHaskellBuild (SetUpHaskellBuild),
  )
import Bootstrap.Nix.Command
  ( NixCommand (NixCommand),
    NixCommandVariant (NCVBuild),
    writeNixCommand,
  )
import Data.Tree (drawTree)

data Readme = Readme
  { readmeProjectName :: ProjectName,
    readmeProjectType :: ProjectType,
    readmeHasDevContainer :: DevContainerConfig,
    readmeMBuildPlan :: Maybe BuildPlan
  }

instance Bootstrappable Readme where
  bootstrapName = const "README.md"
  bootstrapReason = const "This helpfully explains to you what each file (including itself) does!"
  bootstrapContent Readme {..} = do
    let buildFileName = "nix/build.nix"
        buildingForProduction =
          [ "",
            "## Building for Production",
            "",
            "To produce a production build as defined in `"
              <> buildFileName
              <> "`, run `"
              <> writeNixCommand (NixCommand NCVBuild)
              <> "`.",
            "",
            "This will produce a `result` directory with built artefacts."
          ]
    pure . Right . unlines $
      intersperse
        ""
        [ "# " <> unProjectName readmeProjectName,
          "[![Made with nix-bootstrap](https://img.shields.io/badge/Made%20with-nix--bootstrap-rgb(58%2C%2095%2C%20168)?style=flat-square&logo=nixos&logoColor=white&link=https://github.com/gchq/nix-bootstrap)](https://github.com/gchq/nix-bootstrap)",
          "## Overview",
          "**__TODO:__** Write an overview of " <> unProjectName readmeProjectName
        ]
        <> ( [ "",
               "## Development Environment",
               "",
               "<details>",
               "<summary>Expand development environment setup instructions</summary>",
               "<p>",
               "",
               "You can work on "
                 <> unProjectName readmeProjectName
                 <> " in "
                 <> show @_ @Int (if unDevContainerConfig readmeHasDevContainer then 3 else 2)
                 <> " ways:",
               "",
               "  * [In GitPod](#use-in-gitpod)",
               "  * [In a VM](#set-up-in-a-vm)"
             ]
               <> ["  * [In a VSCode DevContainer](#set-up-in-a-devcontainer)" | unDevContainerConfig readmeHasDevContainer]
           )
        <> [ "",
             "### Use In GitPod",
             "",
             "To develop with this project in GitPod, follow the instructions below:",
             "",
             "1. Open this repository in GitLab",
             "2. Click the dropdown near the \"Clone\" button which says \"Web IDE\". Change it to \"GitPod\".",
             "3. Click GitPod button on the same dropdown.",
             "",
             "### Set Up In A VM",
             "",
             "**Note:** This guide assumes you already have a working VM. If you don't know how to set one up, consider following **Setup via Dev Container** below instead.",
             "",
             "1. [Install Nix](https://nixos.org) into your VM by running the following command:",
             "",
             "   ```sh",
             "   sh <(curl -L https://nixos.org/nix/install) --daemon",
             "   ```",
             "",
             "2. [Install direnv **>=2.23.0** by following the instructions for your system](https://direnv.net/docs/installation.html)",
             "",
             "    - You can check your current version by running `direnv version`",
             "    - On the latest Ubuntu, this is available using `apt-get`",
             "    - If you can't install it through your OS's package manager, download a release from the [GitHub releases page](https://github.com/direnv/direnv/releases) and put it somewhere on your `$PATH`.",
             "",
             "3. [Follow the instructions to hook direnv into your shell](https://direnv.net/docs/hook.html)",
             "4. Run `direnv allow` in the project root directory",
             "",
             "_After following the steps above, you will be able to use any of the project's tools mentioned below when in the project folders; direnv will automatically make the tools available when you `cd` into the project._"
           ]
        <> ( if unDevContainerConfig readmeHasDevContainer
               then
                 [ "",
                   "### Set Up In A DevContainer",
                   "",
                   "1. From the AC store, install Docker and Visual Studio Code",
                   "2. If it's not already running, start Docker",
                   "3. Clone the repo",
                   "4. Open Visual Studio Code",
                   "5. Install the \"Remote - Containers\" extension for Visual Studio Code",
                   "6. Open the repo in Visual Studio Code",
                   "7. Select \"Reopen in Container\" in the popup dialog (or press Cmd+Shift+P and select `Remote-Containers: Reopen in Container`)",
                   "",
                   "  At this point, the container will be built and developer tooling should be installed. If you receive a message in a terminal window saying you need to run `direnv allow`, do so.",
                   "",
                   "_After following the steps above, you will be able to use any of the project's tools mentioned below when in the integrated terminal of Visual Studio Code._"
                 ]
               else []
           )
        <> [ "",
             "</p>",
             "</details>"
           ]
        <> ( case readmeProjectType of
               Elm ElmOptions {..} ->
                 [""]
                   <> ( case elmOptionElmMode of
                          ElmModeNode _ ->
                            [ "## Running a Development Server",
                              "",
                              "To run a development server, use the `dev` script defined in `package.json`.",
                              "",
                              "### Troubleshooting: Unexpected token...",
                              "",
                              "Caching issues sometimes cause build failures with unhelpful error messages.",
                              "Try `rm -rf elm-stuff .parcel-cache` if this occurs.",
                              ""
                            ]
                          _ -> []
                      )
                   <> ( if elmOptionProvideElmReview
                          then
                            [ "## Linting Your Elm Files",
                              "",
                              "To lint your Elm files, run `elm-review --fix`.",
                              "",
                              "You may need to run some additional setup commands related to versions when",
                              "you first do this. The tool will advise on what needs doing.",
                              ""
                            ]
                          else []
                      )
                   <> [ "## Building for Production",
                        ""
                      ]
                   <> ( case elmOptionElmMode of
                          ElmModeBare ->
                            ["To produce a production build, run `elm make src/Main.elm`. This will produce `index.html`."]
                          ElmModeNode _ ->
                            [ "To produce a production build, use the `build` script defined in `package.json`.",
                              "",
                              "This will produce a `dist` directory with a built web app."
                            ]
                      )
               Haskell (HaskellOptions _ haskellProjectType) ->
                 let cabalInstructions withBuild =
                       [ "",
                         "## Using your project",
                         "",
                         "1. Generate a cabal file by running `hpack`",
                         "2. `cabal build` will build your application",
                         "3. `cabal run app` will run your application. **Note:** this will initially fail until you replace the body of the `lib` function in `src/Lib.hs`."
                       ]
                         <> if withBuild then buildingForProduction else []
                  in case haskellProjectType of
                       HaskellProjectTypeReplOnly ->
                         [ "",
                           "## Using the Repl",
                           "",
                           "You can use the provided Haskell repl by running `cabal repl` in the dev shell."
                         ]
                       HaskellProjectTypeBasic (SetUpHaskellBuild withBuild) -> cabalInstructions withBuild
                       HaskellProjectTypeServer (SetUpHaskellBuild withBuild) -> cabalInstructions withBuild
               Go (SetUpGoBuild True) -> buildingForProduction
               Python _ ->
                 [ "",
                   "## Adding Python Dependencies",
                   "",
                   "To add python dependencies to the project add them to the 'requirements.txt', in the same format seen in Pip projects.",
                   "",
                   "Then run `direnv reload` to reload the shell and install the dependencies in `requirements.txt`.",
                   "",
                   "For example:",
                   "",
                   "   ```python",
                   "   numpy",
                   "   pandas==1.4.2",
                   "   ```",
                   "",
                   "Note that the `requirements.txt` file has to be tracked by git in order to add dependencies. This can be done by running `git add -N requirements.txt` in this projects root directory"
                 ]
               _ -> []
           )
        <> ( case readmeMBuildPlan of
               Just buildPlan ->
                 [ "",
                   "## Project Structure",
                   "",
                   "<details>",
                   "<summary>Expand project structure tree</summary>",
                   "<p>",
                   "",
                   "The following diagram explains how nix-bootstrap has laid out your infrastructure.",
                   "Feel free to remove this once you're familiar with how it works, or amend it as you grow your project.",
                   "",
                   "```plaintext"
                 ]
                   <> lines (toText . drawTree $ toReasonTree buildPlan)
                   <> [ "```",
                        "",
                        "</p>",
                        "</details>"
                      ]
               Nothing -> []
           )
        <> [ "",
             "## nix-bootstrap",
             "",
             "This project was bootstrapped by [nix-bootstrap](https://github.com/gchq/nix-bootstrap/).",
             "",
             "If you'd like to learn more about how the generated infrastructure works, or have any feedback, please direct it to [our issue tracker](https://github.com/gchq/nix-bootstrap/issues)."
           ]
