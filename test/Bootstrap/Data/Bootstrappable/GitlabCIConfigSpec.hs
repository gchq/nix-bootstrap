{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.GitlabCIConfigSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.GitlabCIConfig (gitlabCIConfigFor)
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig
  ( nixPreCommitHookConfigFor,
  )
import Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig (ContinuousIntegrationConfig),
  )
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeBare, ElmModeNode),
    ElmOptions (ElmOptions),
    NodePackageManager (PNPm),
    ProjectType (Elm, Go),
    SetUpGoBuild (SetUpGoBuild),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Util.CanDieOnError ()
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "gitlab-ci.yml rendering" do
  let ciConfig = ContinuousIntegrationConfig True
  it "renders an Elm/Parcel gitlab-ci config without pre-commit checks correctly" do
    bootstrapContent (gitlabCIConfigFor ciConfig (Elm $ ElmOptions (ElmModeNode PNPm) False) Nothing)
      >>= ( `shouldBe`
              Right
                [r|image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa

default:
  before_script:
    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
    - nix-channel --update
    - nix-env -iA nixpkgs.bash nixpkgs.openssh
    - echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

check-dev-environment:
  stage: build
  script:
    - nix develop -c echo ok

build-site:
  stage: build
  script:
    - nix develop -c pnpm install --frozen-lockfile
    - nix develop -c pnpm run build
|]
          )
  it "renders an Elm/Parcel gitlab-ci config without pre-commit or flakes checks correctly" do
    bootstrapContent (gitlabCIConfigFor ciConfig (Elm $ ElmOptions (ElmModeNode PNPm) False) Nothing)
      >>= ( `shouldBe`
              Right
                [r|image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa

default:
  before_script:
    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
    - nix-channel --update
    - nix-env -iA nixpkgs.bash nixpkgs.openssh
    - echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

check-dev-environment:
  stage: build
  script:
    - nix develop -c echo ok

build-site:
  stage: build
  script:
    - nix develop -c pnpm install --frozen-lockfile
    - nix develop -c pnpm run build
|]
          )
  it "renders a bare Elm gitlab-ci config with pre-commit checks correctly" do
    let projectType = Elm $ ElmOptions ElmModeBare True
        nixPreCommitHookConfig = Just $ nixPreCommitHookConfigFor projectType
    bootstrapContent (gitlabCIConfigFor ciConfig projectType nixPreCommitHookConfig)
      >>= ( `shouldBe`
              Right
                [r|image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa

default:
  before_script:
    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
    - nix-channel --update
    - nix-env -iA nixpkgs.bash nixpkgs.openssh
    - echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

check-dev-environment-and-run-impure-hooks:
  stage: build
  script:
    - nix develop -c elm-review

pre-commit-check:
  stage: build
  script:
    - nix flake check

build-site:
  stage: build
  script:
    - nix develop -c elm make src/Main.elm
|]
          )
  it "renders a bare Elm gitlab-ci config with pre-commit checks and flakes correctly" do
    let projectType = Elm $ ElmOptions ElmModeBare True
        nixPreCommitHookConfig = Just $ nixPreCommitHookConfigFor projectType
    bootstrapContent (gitlabCIConfigFor ciConfig projectType nixPreCommitHookConfig)
      >>= ( `shouldBe`
              Right
                [r|image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa

default:
  before_script:
    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
    - nix-channel --update
    - nix-env -iA nixpkgs.bash nixpkgs.openssh
    - echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

check-dev-environment-and-run-impure-hooks:
  stage: build
  script:
    - nix develop -c elm-review

pre-commit-check:
  stage: build
  script:
    - nix flake check

build-site:
  stage: build
  script:
    - nix develop -c elm make src/Main.elm
|]
          )
  it "renders a Go gitlab-ci with a flake build and without pre-commit checks correctly" do
    bootstrapContent (gitlabCIConfigFor ciConfig (Go $ SetUpGoBuild True) Nothing)
      >>= ( `shouldBe`
              Right
                [r|image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa

default:
  before_script:
    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
    - nix-channel --update
    - nix-env -iA nixpkgs.bash nixpkgs.openssh
    - echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

check-dev-environment:
  stage: build
  script:
    - nix develop -c echo ok

build:
  stage: build
  script:
    - nix build
|]
          )
  it "renders a Go gitlab-ci with a flake build and with pre-commit checks correctly" do
    let projectType = Go $ SetUpGoBuild True
        nixPreCommitHookConfig = Just $ nixPreCommitHookConfigFor projectType
    bootstrapContent (gitlabCIConfigFor ciConfig projectType nixPreCommitHookConfig)
      >>= ( `shouldBe`
              Right
                [r|image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa

default:
  before_script:
    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
    - nix-channel --update
    - nix-env -iA nixpkgs.bash nixpkgs.openssh
    - echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

check-dev-environment:
  stage: build
  script:
    - nix develop -c echo ok

pre-commit-check:
  stage: build
  script:
    - nix flake check

build:
  stage: build
  script:
    - nix build
|]
          )
  it "renders a Go gitlab-ci with a build and without pre-commit checks correctly" do
    bootstrapContent (gitlabCIConfigFor ciConfig (Go $ SetUpGoBuild True) Nothing)
      >>= ( `shouldBe`
              Right
                [r|image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa

default:
  before_script:
    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
    - nix-channel --update
    - nix-env -iA nixpkgs.bash nixpkgs.openssh
    - echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

check-dev-environment:
  stage: build
  script:
    - nix develop -c echo ok

build:
  stage: build
  script:
    - nix build
|]
          )
  it "renders a Go gitlab-ci with a build and with pre-commit checks correctly" do
    let projectType = Go $ SetUpGoBuild True
        nixPreCommitHookConfig = Just $ nixPreCommitHookConfigFor projectType
    bootstrapContent (gitlabCIConfigFor ciConfig projectType nixPreCommitHookConfig)
      >>= ( `shouldBe`
              Right
                [r|image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa

default:
  before_script:
    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
    - nix-channel --update
    - nix-env -iA nixpkgs.bash nixpkgs.openssh
    - echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

check-dev-environment:
  stage: build
  script:
    - nix develop -c echo ok

pre-commit-check:
  stage: build
  script:
    - nix flake check

build:
  stage: build
  script:
    - nix build
|]
          )
  it "renders a gitlab-ci without a build and with pre-commit checks correctly" do
    let projectType = Go $ SetUpGoBuild False
        nixPreCommitHookConfig = Just $ nixPreCommitHookConfigFor projectType
    bootstrapContent (gitlabCIConfigFor ciConfig projectType nixPreCommitHookConfig)
      >>= ( `shouldBe`
              Right
                [r|image: nixos/nix@sha256:473a2b527958665554806aea24d0131bacec46d23af09fef4598eeab331850fa

default:
  before_script:
    - nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
    - nix-channel --update
    - nix-env -iA nixpkgs.bash nixpkgs.openssh
    - echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

check-dev-environment:
  stage: build
  script:
    - nix develop -c echo ok

pre-commit-check:
  stage: build
  script:
    - nix flake check
|]
          )
