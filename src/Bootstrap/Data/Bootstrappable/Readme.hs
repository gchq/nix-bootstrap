{-# LANGUAGE TypeApplications #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Readme
  ( Readme
      ( Readme,
        readmeProjectName,
        readmeProjectType,
        readmeHasDevContainer,
        readmeMBuildPlan,
        readmeUseFlakes
      ),
  )
where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason))
import Bootstrap.Data.BuildPlan (BuildPlan, toReasonTree)
import Bootstrap.Data.DevContainer (DevContainerConfig, unDevContainerConfig)
import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Data.ProjectType (ProjectType (Go, Python), SetUpGoBuild (SetUpGoBuild))
import Bootstrap.Nix.Command
  ( NixCommand (NixCommand),
    NixCommandStyle (NCSNew, NCSOld),
    NixCommandVariant (NCVBuild),
    writeNixCommand,
  )
import Data.Tree (drawTree)

data Readme = Readme
  { readmeProjectName :: ProjectName,
    readmeProjectType :: ProjectType,
    readmeHasDevContainer :: DevContainerConfig,
    readmeMBuildPlan :: Maybe BuildPlan,
    readmeUseFlakes :: Bool
  }

instance Bootstrappable Readme where
  bootstrapName = const "README.md"
  bootstrapReason = const "This helpfully explains to you what each file (including itself) does!"
  bootstrapContent Readme {..} = do
    let commandStyle = if readmeUseFlakes then NCSNew else NCSOld
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
             ""
           ]
        <> ( if readmeUseFlakes
               then
                 [ "2. [Install direnv **>=2.23.0** by following the instructions for your system](https://direnv.net/docs/installation.html)",
                   "",
                   "    - You can check your current version by running `direnv version`",
                   "    - On the latest Ubuntu, this is available using `apt-get`",
                   "    - If you can't install it through your OS's package manager, download a release from the [GitHub releases page](https://github.com/direnv/direnv/releases) and put it somewhere on your `$PATH`.",
                   ""
                 ]
               else ["2. [Install direnv, following the instructions for your system](https://direnv.net/docs/installation.html)"]
           )
        <> [ "3. [Follow the instructions to hook direnv into your shell](https://direnv.net/docs/hook.html)",
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
        <> ( case readmeProjectType of
               Go (SetUpGoBuild True) ->
                 [ "",
                   "## Building for Production",
                   "",
                   "To produce a production build as defined in `default.nix`, run `"
                     <> writeNixCommand (NixCommand commandStyle NCVBuild)
                     <> "`.",
                   "",
                   "This will produce a `result` directory with built artefacts."
                 ]
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
                   "   ```"
                 ]
                   <> ( if readmeUseFlakes
                          then
                            [ "",
                              "Note that the `requirements.txt` file has to be tracked by git in order to add dependencies. This can be done by running `git add -N requirements.txt` in this projects root directory"
                            ]
                          else []
                      )
               _ -> []
           )
        <> ( case readmeMBuildPlan of
               Just buildPlan ->
                 [ "",
                   "## Project Structure",
                   "",
                   "The following diagram explains how nix-bootstrap has laid out your infrastructure.",
                   "Feel free to remove this once you're familiar with how it works, or amend it as you grow your project.",
                   "",
                   "```plaintext"
                 ]
                   <> lines (toText . drawTree $ toReasonTree buildPlan)
                   <> ["```"]
               Nothing -> []
           )
        <> [ "",
             "## nix-bootstrap",
             "",
             "This project was bootstrapped by [nix-bootstrap](https://github.com/gchq/nix-bootstrap/).",
             "",
             "If you'd like to learn more about how the generated infrastructure works, or have any feedback, please direct it to [our issue tracker](https://github.com/gchq/nix-bootstrap/issues)."
           ]
