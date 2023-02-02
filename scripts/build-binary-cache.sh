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

function runBuildBinaryCache() {
  # shellcheck disable=SC2154
  # (sed is defined in ../flake.nix)
  local sedCmd=$sed
  # shellcheck disable=SC2154
  # (tar is defined in ../flake.nix)
  local tarCmd=$tar

  set -e
  nix build
  # shellcheck disable=SC2046
  # (We want word splitting in the output from nix-store -qR)
  nix-store --export $(nix-store -qR "$(readlink result)") > nix-bootstrap-binary-cache
  $tarCmd cvzf "$(readlink result | $sedCmd 's:.*/::').tgz" nix-bootstrap-binary-cache
  mkdir -p releases
  mv ./*nix-bootstrap*.tgz releases
  rm -r nix-bootstrap-binary-cache
}

runBuildBinaryCache

