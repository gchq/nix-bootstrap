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
  baseHaskellPackages,
  nixpkgs,
}: let
  src = ../.;
  nix-bootstrap-unstripped = nixpkgs.haskell.lib.overrideCabal (baseHaskellPackages.callCabal2nix "nix-bootstrap" src {}) (
    _: {
      configureFlags = ["-fprod"];
      preBuild = ''
        sed -i 's:proc "alejandra":proc "${nixpkgs.alejandra.outPath}/bin/alejandra":g' src/Bootstrap/Unix.hs
        sed -i 's:runCommand "which":runCommand "${nixpkgs.which.outPath}/bin/which":g' src/Bootstrap/Unix.hs
      '';
    }
  );
in {
  nix-bootstrap = nixpkgs.stdenv.mkDerivation {
    inherit src;
    inherit (nix-bootstrap-unstripped) name version;
    buildInputs = with nixpkgs; [
      alejandra
      glibc
      gmp
      libffi
      ncurses
      which
      zlib
    ];
    installPhase = ''
      mkdir -p $out/bin
      cp ${(nixpkgs.haskell.lib.enableSeparateBinOutput nix-bootstrap-unstripped).bin}/bin/app $out/bin/nix-bootstrap
    '';
  };
}
