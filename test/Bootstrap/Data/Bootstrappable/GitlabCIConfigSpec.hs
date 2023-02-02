-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.GitlabCIConfigSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.GitlabCIConfig (gitlabCIConfigFor)
import Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig (ContinuousIntegrationConfig),
  )
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (PreCommitHooksConfig))
import Bootstrap.Data.ProjectType
  ( ProjectType (Go),
    SetUpGoBuild (SetUpGoBuild),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Util.CanDieOnError ()
import Test.Util.RunConfig (rcDefault, rcWithFlakes)

spec :: Spec
spec = describe "gitlab-ci.yml rendering" do
  let ciConfig = ContinuousIntegrationConfig True
  it "renders a Go gitlab-ci with a flake build and without pre-commit checks correctly" do
    let preCommitHooksConfig = PreCommitHooksConfig False
    bootstrapContent (gitlabCIConfigFor ciConfig rcWithFlakes (Go $ SetUpGoBuild True) preCommitHooksConfig)
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa",
                      "",
                      "default:",
                      "  before_script:",
                      "    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs",
                      "    - nix-channel --update",
                      "    - nix-env -iA nixpkgs.bash nixpkgs.openssh",
                      "    - echo \"experimental-features = nix-command flakes\" >> /etc/nix/nix.conf",
                      "",
                      "check-dev-environment:",
                      "  stage: build",
                      "  script: \"nix develop -c echo ok\"",
                      "",
                      "build:",
                      "  stage: build",
                      "  script: \"nix build\""
                    ]
                )
          )
  it "renders a Go gitlab-ci with a flake build and with pre-commit checks correctly" do
    let preCommitHooksConfig = PreCommitHooksConfig True
    bootstrapContent (gitlabCIConfigFor ciConfig rcWithFlakes (Go $ SetUpGoBuild True) preCommitHooksConfig)
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa",
                      "",
                      "default:",
                      "  before_script:",
                      "    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs",
                      "    - nix-channel --update",
                      "    - nix-env -iA nixpkgs.bash nixpkgs.openssh",
                      "    - echo \"experimental-features = nix-command flakes\" >> /etc/nix/nix.conf",
                      "",
                      "check-dev-environment:",
                      "  stage: build",
                      "  script: \"nix develop -c echo ok\"",
                      "",
                      "pre-commit-check:",
                      "  stage: build",
                      "  script: \"nix build '.#runChecks'\"",
                      "",
                      "build:",
                      "  stage: build",
                      "  script: \"nix build\""
                    ]
                )
          )
  it "renders a Go gitlab-ci with a build and without pre-commit checks correctly" do
    let preCommitHooksConfig = PreCommitHooksConfig False
    bootstrapContent (gitlabCIConfigFor ciConfig rcDefault (Go $ SetUpGoBuild True) preCommitHooksConfig)
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa",
                      "",
                      "default:",
                      "  before_script:",
                      "    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs",
                      "    - nix-channel --update",
                      "    - nix-env -iA nixpkgs.bash nixpkgs.openssh",
                      "    - echo \"experimental-features = nix-command flakes\" >> /etc/nix/nix.conf",
                      "",
                      "check-dev-environment:",
                      "  stage: build",
                      "  script: \"nix-shell --run 'echo ok'\"",
                      "",
                      "build:",
                      "  stage: build",
                      "  script: \"nix-build\""
                    ]
                )
          )
  it "renders a Go gitlab-ci with a build and with pre-commit checks correctly" do
    let preCommitHooksConfig = PreCommitHooksConfig True
    bootstrapContent (gitlabCIConfigFor ciConfig rcDefault (Go $ SetUpGoBuild True) preCommitHooksConfig)
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa",
                      "",
                      "default:",
                      "  before_script:",
                      "    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs",
                      "    - nix-channel --update",
                      "    - nix-env -iA nixpkgs.bash nixpkgs.openssh",
                      "    - echo \"experimental-features = nix-command flakes\" >> /etc/nix/nix.conf",
                      "",
                      "check-dev-environment:",
                      "  stage: build",
                      "  script: \"nix-shell --run 'echo ok'\"",
                      "",
                      "pre-commit-check:",
                      "  stage: build",
                      "  script: \"nix-build nix/pre-commit-hooks.nix -A hooks --arg pre-commit-hooks-lib 'import (import nix/sources.nix {}).pre-commit-hooks'\"",
                      "",
                      "build:",
                      "  stage: build",
                      "  script: \"nix-build\""
                    ]
                )
          )
  it "renders a gitlab-ci without a build and with pre-commit checks correctly" do
    let preCommitHooksConfig = PreCommitHooksConfig True
    bootstrapContent (gitlabCIConfigFor ciConfig rcDefault (Go $ SetUpGoBuild False) preCommitHooksConfig)
      >>= ( `shouldBe`
              Right
                ( unlines
                    [ "image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa",
                      "",
                      "default:",
                      "  before_script:",
                      "    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs",
                      "    - nix-channel --update",
                      "    - nix-env -iA nixpkgs.bash nixpkgs.openssh",
                      "    - echo \"experimental-features = nix-command flakes\" >> /etc/nix/nix.conf",
                      "",
                      "check-dev-environment:",
                      "  stage: build",
                      "  script: \"nix-shell --run 'echo ok'\"",
                      "",
                      "pre-commit-check:",
                      "  stage: build",
                      "  script: \"nix-build nix/pre-commit-hooks.nix -A hooks --arg pre-commit-hooks-lib 'import (import nix/sources.nix {}).pre-commit-hooks'\""
                    ]
                )
          )
