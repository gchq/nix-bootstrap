-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.DevContainer
  ( DevContainerDockerfile,
    DevContainerJson,
    DevContainerDockerCompose,
    devContainerDockerfileFor,
    devContainerJsonFor,
    devContainerDockerComposeFor,
  )
where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason), bootstrapContentPrettyJson, bootstrapContentYaml)
import Bootstrap.Data.DevContainer (DevContainerConfig (DevContainerConfig))
import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import qualified Bootstrap.Data.ProjectName as ProjectName
import Bootstrap.Data.ProjectType (ProjectType)
import Bootstrap.Data.VSCodeExtension (VSCodeExtension, vsCodeExtensionsFor)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Text as T

data DevContainerDockerfile = DevContainerDockerfile

instance Bootstrappable DevContainerDockerfile where
  bootstrapName = const ".devcontainer/Dockerfile"
  bootstrapReason = const "The dockerfile from which the VSCode DevContainer is built."
  bootstrapContent =
    const
      . pure
      . Right
      . unlines
      $ [ "FROM ubuntu:22.04 as base",
          "",
          "# Set shell and check for pipe fails",
          "SHELL [\"/bin/bash\", \"-o\", \"pipefail\", \"-c\"]",
          "",
          "# Install deps required by Nix installer",
          "RUN apt-get update && apt-get upgrade -y && apt-get install -y --no-install-recommends \\",
          "    ca-certificates \\",
          "    curl \\",
          "    sudo \\",
          "    xz-utils",
          "",
          "# Create user",
          "RUN groupadd -g 1001 vscode && \\",
          "    useradd -u 1001 -g 1001 -G sudo -m vscode -s /bin/bash",
          "",
          "# Configure sudo and Nix",
          "RUN sed -i 's/%sudo.*ALL/%sudo   ALL=(ALL:ALL) NOPASSWD:ALL/' /etc/sudoers && \\",
          "    echo \"sandbox = false\" > /etc/nix.conf && \\",
          "    echo \"experimental-features = nix-command flakes\" >> /etc/nix.conf",
          "",
          "# Install Nix and enable flakes",
          "USER vscode",
          "ENV USER=vscode",
          "ENV NIX_PATH=/home/vscode/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels",
          "ENV NIX_CONF_DIR /etc",
          "RUN curl -L https://nixos.org/nix/install | NIX_INSTALLER_NO_MODIFY_PROFILE=1 sh",
          "",
          "FROM ubuntu:22.04",
          "",
          "ENV NIX_CONF_DIR /etc",
          "",
          "## Install vscode deps",
          "RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get upgrade -y && apt-get install -y --no-install-recommends \\",
          "    ca-certificates \\",
          "    git \\",
          "    gnupg \\",
          "    gnupg2 \\",
          "    locales \\",
          "    ssh \\",
          "    sudo && \\",
          "    rm -rf /var/lib/apt/lists/*",
          "",
          "# Create user",
          "RUN groupadd -g 1001 vscode && \\",
          "    useradd -u 1001 -g 1001 -G sudo -m vscode -s /bin/bash",
          "COPY --from=base --chown=vscode:vscode /home/vscode /home/vscode",
          "",
          "# Configure en_US.UTF-8 locale",
          "RUN echo \"en_US.UTF-8 UTF-8\" >> /etc/locale.gen && \\",
          "    locale-gen",
          "",
          "# Configure sudo",
          "RUN sed -i 's/%sudo.*ALL/%sudo   ALL=(ALL:ALL) NOPASSWD:ALL/' /etc/sudoers",
          "",
          "# Setup Nix environment",
          "RUN echo \"source /home/vscode/.nix-profile/etc/profile.d/nix.sh\" >> /etc/bash.bashrc && \\",
          "    echo \"source /home/vscode/.nix-profile/etc/profile.d/nix.sh\" >> /etc/zshrc",
          "",
          "# Copy nix and configs",
          "COPY --from=base /nix /nix",
          "COPY --from=base /etc/nix.conf /etc/nix.conf",
          "",
          "USER vscode",
          "",
          "# Setup vscode",
          "RUN mkdir -p /home/vscode/.vscode-server/extensions && \\",
          "    mkdir -p /home/vscode/.vscode-server-insiders/extensions",
          "",
          "RUN export PATH=$PATH:/home/vscode/.nix-profile/bin \\",
          "    && nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs && nix-channel --update \\",
          "    && nix-env -iA nixpkgs.bashInteractive nixpkgs.direnv nixpkgs.git \\",
          "    && echo 'eval \"$(direnv hook bash)\"' >> ~/.bashrc",
          "",
          "RUN mkdir -p ~/.config/direnv && touch ~/.config/direnv/direnvrc",
          "",
          "ENV SHELL=\"/home/vscode/.nix-profile/bin/bash\"",
          "ENV PATH=\"${PATH}:/home/vscode/.nix-profile/bin\""
        ]

devContainerDockerfileFor :: DevContainerConfig -> Maybe DevContainerDockerfile
devContainerDockerfileFor (DevContainerConfig True) = Just DevContainerDockerfile
devContainerDockerfileFor (DevContainerConfig False) = Nothing

data DevContainerJson = DevContainerJson
  { projectName :: ProjectName,
    extensions :: [VSCodeExtension]
  }

instance Bootstrappable DevContainerJson where
  bootstrapName = const ".devcontainer/devcontainer.json"
  bootstrapReason = const "The config for the VSCode DevContainer."
  bootstrapContent = bootstrapContentPrettyJson ["name"]

instance ToJSON DevContainerJson where
  toJSON DevContainerJson {..} = do
    let serialisableProjectName = ProjectName.replaceSpacesWithDashes projectName
        serviceName = serviceNameFor projectName
        workspaceFolderName = "/workspaces/" <> unProjectName serialisableProjectName
    Aeson.object
      [ "name" .= Aeson.String (unProjectName projectName <> " DevContainer"),
        "dockerComposeFile" .= Aeson.String "docker-compose.yaml",
        "extensions" .= toJSON extensions,
        "postCreateCommand"
          .= Aeson.String
            ( "git config --global core.editor 'code --wait' && "
                <> "git config --global --add safe.directory "
                <> workspaceFolderName
            ),
        "postStartCommand" .= Aeson.String "direnv allow && nix-shell --run \"echo 'Setup complete!'\"",
        "service" .= Aeson.String serviceName,
        "workspaceFolder" .= Aeson.String workspaceFolderName
      ]

devContainerJsonFor :: DevContainerConfig -> ProjectName -> ProjectType -> Maybe DevContainerJson
devContainerJsonFor (DevContainerConfig False) _ _ = Nothing
devContainerJsonFor (DevContainerConfig True) projectName projectType =
  Just
    DevContainerJson
      { projectName = projectName,
        extensions = vsCodeExtensionsFor projectType
      }

newtype DevContainerDockerCompose = DevContainerDockerCompose
  {devContainerDockerComposeProjectName :: ProjectName}

instance Bootstrappable DevContainerDockerCompose where
  bootstrapName = const ".devcontainer/docker-compose.yaml"
  bootstrapReason = const "The docker compose file from which the VSCode DevContainer's service is configured."
  bootstrapContent =
    pure . Right
      . unlines
      . foldr buildDockerCompose []
      . dropWhileEnd (T.all isSpace)
      . lines
      . bootstrapContentYaml
    where
      buildDockerCompose :: Text -> [Text] -> [Text]
      buildDockerCompose next acc =
        if "    command" `T.isPrefixOf` next
          then "    # Override the default command so things don't shut down after the process ends." : next : acc
          else next : acc

instance ToJSON DevContainerDockerCompose where
  toJSON DevContainerDockerCompose {..} = do
    let serialisableProjectName = ProjectName.replaceSpacesWithDashes devContainerDockerComposeProjectName
        serviceName = serviceNameFor devContainerDockerComposeProjectName
    Aeson.object
      [ "version" .= Aeson.String "3",
        "services"
          .= Aeson.object
            [ fromString (toString serviceName)
                .= Aeson.object
                  [ "container_name" .= Aeson.String serviceName,
                    "build"
                      .= Aeson.object
                        [ "context" .= Aeson.String ".",
                          "dockerfile" .= Aeson.String "Dockerfile"
                        ],
                    "volumes"
                      .= Aeson.Array
                        ( fromList
                            [ Aeson.String $ "../:/workspaces/" <> unProjectName serialisableProjectName <> ":cached",
                              Aeson.String "${HOME}/.ssh:/home/vscode/.ssh"
                            ]
                        ),
                    "command" .= Aeson.String "/bin/sh -c 'while sleep 1000; do :; done'"
                  ]
            ]
      ]

devContainerDockerComposeFor :: DevContainerConfig -> ProjectName -> Maybe DevContainerDockerCompose
devContainerDockerComposeFor (DevContainerConfig True) n = Just $ DevContainerDockerCompose n
devContainerDockerComposeFor (DevContainerConfig False) _ = Nothing

-- | Gets the name of the dev container service from the project name
serviceNameFor :: ProjectName -> Text
serviceNameFor = (<> "-dev-container") . T.toLower . unProjectName . ProjectName.replaceSpacesWithDashes
