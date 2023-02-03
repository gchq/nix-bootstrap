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
  alejandra,
  nixpkgs,
  pre-commit-hooks-lib,
  src,
  system,
}: let
  # Function to make a set of pre-commit hooks
  makeHooks = hooks:
    pre-commit-hooks-lib.lib.${system}.run {
      inherit hooks src;
    };
  # Hooks which don't depend on running in a dev environment
  pureHooks = {
    alejandra = {
      enable = true;
      entry = nixpkgs.lib.mkOverride 0 "${alejandra}/bin/alejandra";
    };
    hlint = {
      enable = true;
      entry = nixpkgs.lib.mkOverride 0 "${pre-commit-hooks-lib.packages.${system}.hlint}/bin/hlint -XNoCPP";
    };
    hpack = {
      enable = true;
      entry = nixpkgs.lib.mkOverride 0 "${
        nixpkgs.writeShellScriptBin
        "hpack-override"
        "(${pre-commit-hooks-lib.packages.${system}.hpack}/bin/hpack)"
      }/bin/hpack-override";
    };
    nix-linter.enable = true;
    ormolu.enable = true;
    prettier = {
      enable = true;
      excludes = [".pnp.*" ".pre-commit-config.yaml"];
    };
    shellcheck = {
      enable = true;
      entry = nixpkgs.lib.mkOverride 0 "${
        pre-commit-hooks-lib.packages."${system}".shellcheck
      }/bin/shellcheck -x";
      types_or = ["shell"];
    };
  };
  # Hooks which can run on pre-commit but not in CI
  impureHooks = {
    hpack-with-version-bump = {
      enable = true;
      entry = "${
        nixpkgs.writeShellScriptBin "hpack-with-version-bump.sh" ''
          set -e
          (${nixpkgs.git}/bin/git fetch --all && \
            ${nixpkgs.git}/bin/git diff "$(git describe --tags --abbrev=0)" -- package.yaml | ${nixpkgs.gnugrep}/bin/grep version) \
            || (echo "You must bump the nix-bootstrap version number in package.yaml!" && set -e && exit 1)
          (${pre-commit-hooks-lib.packages.${system}.hpack}/bin/hpack)
        ''
      }/bin/hpack-with-version-bump.sh";
      files = ".*";
      name = "Update config and version";
      pass_filenames = false;
    };
    # Prefixed with z_ to make it run last
    z_build_nix_bootstrap = {
      enable = true;
      entry = "nix build";
      files = "\\.(cabal|hs|nix|yaml)$";
      name = "build";
      pass_filenames = false;
    };
    z_vulnerabilities = {
      enable = true;
      entry = "${
        nixpkgs.writeShellScriptBin
        "check-for-vulnerabilities"
        "${nixpkgs.vulnix}/bin/vulnix -w vulnerability-whitelist.toml result"
      }/bin/check-for-vulnerabilities";
      files = "nix-bootstrap";
      name = "check-for-vulnerabilities";
      pass_filenames = false;
    };
  };
in {
  allHooks = makeHooks (builtins.removeAttrs (pureHooks // impureHooks) ["hpack"]);
  pureHooks = makeHooks pureHooks;
  tools =
    [alejandra nixpkgs.vulnix]
    ++ (with pre-commit-hooks-lib.packages.${system}; [
      hlint
      hpack
      hpack-dir
      nix-linter
      ormolu
      prettier
      shellcheck
    ]);
}
