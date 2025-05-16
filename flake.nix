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
{
  description = "Development infrastructure for nix-bootstrap";
  inputs = {
    nixpkgs-src.url = "nixpkgs/b62d2a95c72fb068aecd374a7262b37ed92df82b";
    # Needed to get non-broken vulnix
    nixpkgs-src-previous.url = "github:NixOS/nixpkgs?rev=89172919243df199fe237ba0f776c3e3e3d72367";
    pre-commit-hooks-lib = {
      inputs.nixpkgs.follows = "nixpkgs-src";
      url = "github:cachix/pre-commit-hooks.nix";
    };
  };

  outputs = {
    self,
    nixpkgs-src,
    nixpkgs-src-previous,
    pre-commit-hooks-lib,
    ...
  }: let
    systemsHelpers = import nix/systems.nix;
    allSystems = nixpkgs-src.lib.platforms.all;
    supportedSystems = with systemsHelpers.system allSystems; [x86_64-linux aarch64-linux];
  in
    systemsHelpers.forEachSystem supportedSystems (
      system: let
        nixpkgs = nixpkgs-src.legacyPackages.${system};
        inherit
          (import nix/haskell-env.nix {inherit nixpkgs;})
          baseHaskellPackages
          haskellDevTools
          ;
        inherit (import nix/release.nix {inherit baseHaskellPackages nixpkgs;}) nix-bootstrap;
        buildBinaryCache = nixpkgs.writeShellScriptBin "buildBinaryCache" ''
          sed=${nixpkgs.gnused}/bin/sed
          tar=${nixpkgs.gnutar}/bin/tar
          ${builtins.readFile scripts/build-binary-cache.sh}
        '';
        pre-commit-hooks = import nix/pre-commit-hooks.nix {
          inherit nixpkgs pre-commit-hooks-lib system;
          inherit (nixpkgs) alejandra;
          src = ./.;
          vulnix = nixpkgs-src-previous.legacyPackages.${system}.vulnix;
        };
        extraDevShellArgs = {
          inherit (pre-commit-hooks.allHooks) shellHook;
        };
      in {
        checks = {pre-commit-check = pre-commit-hooks.pureHooks;};
        devShells = {
          default = nixpkgs.mkShell ({
              buildInputs =
                [buildBinaryCache]
                ++ haskellDevTools
                ++ pre-commit-hooks.tools
                ++ (
                  with nixpkgs; [
                    coreutils
                    dhall
                    glab
                    gnused
                    gnutar
                    which
                  ]
                );
            }
            // extraDevShellArgs);
        };
        packages = {
          default = nix-bootstrap;
          inherit nix-bootstrap;
          # To be used as tools in CI
          ciPackages_buildBinaryCache = buildBinaryCache;
          ciPackages_vulnix = nixpkgs.vulnix;
        };
      }
    );
}
