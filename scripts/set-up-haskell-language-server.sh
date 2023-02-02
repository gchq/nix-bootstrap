# shellcheck shell=bash
#
#
#
# © Crown Copyright GCHQ
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#
#
# This script should be run in a dev shell with project dependencies
# either automatically in a devcontainer or otherwise after cloning
# the project. It should be run using the devShell-provided
# "setUpHaskellLanguageServer" command.

function run() {
  # shellcheck disable=SC2154
  # (sed is defined in flake.nix)
  local sedCmd=$sed

  # Run first build (should fail after downloading niv)
  $sedCmd -i 's/\s*&& exit 1//' cabal.project
  printf '\033[1m\033[34mRunning first build (this one should fail)\033[0m\n'
  set +e
  cabal build -O0
  haskell-language-server-wrapper
  cabal build -O0
  haskell-language-server-wrapper

  $sedCmd -i 's/hpack$/hpack \&\& exit 1/' cabal.project
  set -e
  # Update cabal db
  printf '\033[1m\033[34mUpdating cabal DB\033[0m\n'
  cabal update

  # Run second build (should succeed)
  printf '\033[1m\033[34mRunning second build (this one should succeed)\033[0m\n'
  cabal build -O0
  set +e
  haskell-language-server-wrapper

  printf "\n\033[1m\033[32m%s\033[0m\n" "Language server set up successfully (errors above should be ignorable)."
}

# Delete any existing build files
(cd "$(git rev-parse --show-toplevel)" && rm -rf dist-newstyle)

(cd "$(git rev-parse --show-toplevel)" && run)
