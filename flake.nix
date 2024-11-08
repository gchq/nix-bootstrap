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
    flake-compat = {
      flake = false;
      url = github:edolstra/flake-compat;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs-src.url = "github:NixOS/nixpkgs/nixos-24.05";
    pre-commit-hooks-lib = {
      inputs.flake-utils.follows = "flake-utils";
      url = "github:cachix/pre-commit-hooks.nix";
    };
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs-src,
    pre-commit-hooks-lib,
    ...
  }:
    flake-utils.lib.eachSystem (with flake-utils.lib.system; [x86_64-linux aarch64-linux]) (
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
        };
        extraDevShellArgs = {
          inherit (pre-commit-hooks.allHooks) shellHook;
        };
      in {
        checks = {pre-commit-check = pre-commit-hooks.pureHooks;};
        defaultPackage = self.packages.${system}.default;
        devShell = self.devShells.${system}.default;
        devShells = {
          default = nixpkgs.mkShell ({
              buildInputs =
                [buildBinaryCache]
                ++ haskellDevTools
                ++ pre-commit-hooks.tools
                ++ (
                  with nixpkgs; [
                    coreutils
                    glab
                    gnused
                    gnutar
                    niv
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
          ciPackages = {
            inherit buildBinaryCache;
            inherit (nixpkgs) vulnix;
          };
          # runChecks is a hack required to allow checks to run on a single system
          # when using Import from Deviation (https://discourse.nixos.org/t/nix-flake-check-for-current-system-only/18366)
          # Building it is the single-system equivalent of running "nix flake check".
          runChecks = nixpkgs.runCommand "run-checks" {
            currentSystemChecks = builtins.attrValues self.checks.${system};
          } "echo $currentSystemChecks; touch $out";
        };
      }
    );
}
