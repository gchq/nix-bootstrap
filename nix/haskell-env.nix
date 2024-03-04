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
{nixpkgs}: let
  # haskell package set
  baseHaskellPackages = nixpkgs.haskell.packages.ghc948;

  # dev tools
  haskellEnv = baseHaskellPackages.ghcWithPackages (
    haskellPackages:
      with haskellPackages; [
        cabal-install
        haskell-language-server
      ]
  );
in {
  inherit baseHaskellPackages;
  haskellDevTools = [
    haskellEnv
    # cabal uses wget but doesn't package it. This ensures a compatible version is used,
    # as devcontainers otherwise package the busybox version by default, which accepts
    # different arguments.
    nixpkgs.wget
  ];
}
