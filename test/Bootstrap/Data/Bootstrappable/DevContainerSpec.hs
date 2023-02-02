-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.DevContainerSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.DevContainer
  ( devContainerDockerComposeFor,
    devContainerDockerfileFor,
    devContainerJsonFor,
  )
import Bootstrap.Data.DevContainer (DevContainerConfig (DevContainerConfig))
import Bootstrap.Data.ProjectName (mkProjectName)
import Bootstrap.Data.ProjectType (NodePackageManager (Yarn), ProjectType (Node))
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = describe "devcontainer rendering" do
  let projectName = Unsafe.fromJust $ mkProjectName "my Project"
      devContainerConfig = DevContainerConfig True
  describe "Dockerfile rendering" do
    it "renders the dockerfile correctly" do
      bootstrapContent (devContainerDockerfileFor devContainerConfig)
        >>= ( `shouldBe`
                Right
                  ( unlines
                      [ "FROM ubuntu:22.04 as base",
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
                  )
            )
  describe "devcontainer.json rendering" do
    it "renders the json correctly" do
      bootstrapContent (devContainerJsonFor devContainerConfig projectName (Node Yarn))
        >>= ( `shouldBe`
                Right
                  ( unlines
                      [ "{",
                        "    \"name\": \"my Project DevContainer\",",
                        "    \"dockerComposeFile\": \"docker-compose.yaml\",",
                        "    \"extensions\": [",
                        "        \"arrterian.nix-env-selector\",",
                        "        \"jnoortheen.nix-ide\"",
                        "    ],",
                        "    \"postCreateCommand\": \"git config --global core.editor 'code --wait' && git config --global --add safe.directory /workspaces/my-Project\",",
                        "    \"postStartCommand\": \"direnv allow && nix-shell --run \\\"echo 'Setup complete!'\\\"\",",
                        "    \"service\": \"my-project-dev-container\",",
                        "    \"workspaceFolder\": \"/workspaces/my-Project\"",
                        "}"
                      ]
                  )
            )
  describe "docker-compose.yaml rendering" do
    it "renders the yaml correctly" do
      bootstrapContent (devContainerDockerComposeFor devContainerConfig projectName)
        >>= ( `shouldBe`
                Right
                  ( unlines
                      [ "services:",
                        "  my-project-dev-container:",
                        "    build:",
                        "      context: .",
                        "      dockerfile: Dockerfile",
                        "    # Override the default command so things don't shut down after the process ends.",
                        "    command: /bin/sh -c 'while sleep 1000; do :; done'",
                        "    container_name: my-project-dev-container",
                        "    volumes:",
                        "    - ../:/workspaces/my-Project:cached",
                        "    - ${HOME}/.ssh:/home/vscode/.ssh",
                        "version: '3'"
                      ]
                  )
            )
