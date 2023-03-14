#!/bin/sh
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
# This script checks prerequisites and runs nix-bootstrap.
#
# On Ubuntu it can also install any missing prerequisites.

#########################################################################
# FORMATTING AND INPUT HELPERS                                          #
#########################################################################

beginFormatting() {
  printf "\033[%im" "$1"
}

beginBold() { beginFormatting 1; }
beginUnderline() { beginFormatting 4; }
beginRed() { beginFormatting 31; }
beginGreen() { beginFormatting 32; }
beginYellow() { beginFormatting 33; }
beginCyan() { beginFormatting 36; }

resetFormatting() { beginFormatting 0; }

printStatus() {
  resetFormatting
  "begin$1"
  beginBold
  printf "%s\n" "$2"
  resetFormatting
}

printError() { printStatus "Red" "$1"; }
printSuccess() { printStatus "Green" "$1"; }
printWarning() { printStatus "Yellow" "$1"; }

# Prompts the user for a yes/no answer.
# The question will be repeated until a valid answer is given.
#
# The question will appear in blue and formatting will be reset afterwards.
# Error messages appear in red.
#
# Arg 1 is the prompt. A space will be inserted after the prompt text.
askYesNo() {
  while true; do
    beginCyan
    printf "%s [yes|no]: " "$1"
    resetFormatting
    read -r yn
    case $yn in
        y|Y|yes|Yes ) return 0;;
        n|N|no|No ) return 1;;
        * ) printError "Please answer yes or no.";;
    esac
  done
}

#########################################################################
# INDIVIDUAL PROCESSING FUNCTIONS                                       #
#########################################################################

warnDirectoryUnclean() {
  printError "This script should be run in a new, clean directory where you want to create a project."
  printf "Alternatively, it can be run in an existing directory but may overwrite some of its contents - make"
  printf " sure you're happy for this to happen if you proceed anyway.\n"
  printf "If you wish to proceed anyway, you can do so by typing PROCEED (anything else will cause this script to exit): "
  read -r proceed
  case $proceed in
      PROCEED ) return 0;;
      * ) exit 1;;
  esac
}

isUbuntu() { uname -v | grep -iq ubuntu; }

packageListUpdated=1
installWithApt() {
  if [ $packageListUpdated -eq 1 ]; then
    sudo apt-get update
    packageListUpdated=0
  fi
  if ! sudo apt-get install "$1"; then
      printError "Could not install $1; exiting."
      exit 1
  fi
}

installUbuntuPrerequisite() {
  if isUbuntu; then
    printWarning "You need to have $1 installed to run nix-bootstrap."
    if askYesNo "Would you like to install $1?"; then
      installWithApt "$1"
    else
      printError "$1 is a prerequisite of nix-bootstrap; please install it and try again."
      exit 1
    fi
  else
    printError "$1 is a prerequisite of nix-bootstrap; please install it and try again."
    exit 1
  fi
}

installNix() {
  resetFormatting
  printWarning "You need to have nix installed to run nix-bootstrap."
  if askYesNo "Would you like to install nix?"; then
    tmp="$(mktemp)"
    curl -L https://nixos.org/nix/install > "$tmp"
    if sh "$tmp" --daemon; then
      printSuccess "Nix has been installed successfully."
      printWarning "PLEASE CLOSE THIS TERMINAL, OPEN A NEW ONE AND RUN THE SAME COMMAND AGAIN TO PROCEED."
      exit 0
    else
      printError "The installation of nix seems to have failed."
      printError "Please try installing nix using the instructions on nixos.org then run this script again."
      exit 1
    fi
  else
    printError "Nix is a prerequisite of nix-bootstrap; exiting."
    exit 1
  fi
  resetFormatting
}

installDirenv() {
  printWarning "nix-bootstrap works best when direnv is installed."
  if askYesNo "Would you like to install it and hook it into your .bashrc?"; then
    installWithApt direnv
    # shellcheck disable=SC2016
    # We don't want expressions to expand here
    printf '%s\n' 'eval "$(direnv hook bash)"' >> ~/.bashrc
    printSuccess "direnv has been installed successfully."
    printWarning "PLEASE CLOSE THIS TERMINAL, OPEN A NEW ONE AND RUN THE SAME COMMAND AGAIN TO PROCEED."
    exit 0
  fi
}

recommendDirenv() {
  printWarning "WARNING: Direnv is not installed. We recommend you install it to use nix-bootstrap:"
  printf "         "
  beginUnderline
  printWarning "https://direnv.net/docs/installation.html"
  resetFormatting
}

installOrRecommendDirenv() {
  if isUbuntu; then installDirenv; else recommendDirenv; fi
}

installNixBootstrap() {
  # shellcheck disable=SC2016
  # We don't want expressions to expand here
  installNixBootstrapCmd="$(printf '%s && %s && %s && %s && %s && %s && %s && %s' \
    'rm -rf /tmp/nix-bootstrap-binary-cache' \
    'NIX_BOOTSTRAP_RELEASE_ID="$(curl -L https://api.github.com/repos/gchq/nix-bootstrap/releases/latest | jq ".id")"' \
    'NIX_BOOTSTRAP_RELEASE_INFO="$(curl -L "https://api.github.com/repos/gchq/nix-bootstrap/releases/$NIX_BOOTSTRAP_RELEASE_ID/assets" | jq ".[0]")"' \
    'curl -L "$(printf "%s" "$NIX_BOOTSTRAP_RELEASE_INFO" | jq -r ".browser_download_url")"  | tar xz -C /tmp' \
    'releaseFilename="$(printf "%s" "$NIX_BOOTSTRAP_RELEASE_INFO" | jq -r ".name")"' \
    'NIX_BOOTSTRAP_STORE_PATH="$(echo "/nix/store/${releaseFilename/\.tgz/}")"' \
    'sudo su -c "$(printf '"'"'%s --import < /tmp/nix-bootstrap-binary-cache'"'"' "$(which nix-store)")"' \
    '$NIX_BOOTSTRAP_STORE_PATH/bin/nix-bootstrap --allow-dirty')"
  nix-shell -p bash curl gnutar jq --command "$installNixBootstrapCmd $*"
}

#########################################################################
# MAIN FLOW                                                             #
#########################################################################

# Check we're in a clean directory
command ls | grep -vq '.git' && warnDirectoryUnclean
# Check curl is installed (or install it if possible)
[ "$(which curl 2>/dev/null)" ] || installUbuntuPrerequisite "curl"
# Check git is installed (or install it if possible)
[ "$(which git 2>/dev/null)" ] || installUbuntuPrerequisite "git"
# Check nix is installed (or install it)
[ "$(which nix-store 2>/dev/null)" ] || installNix
# Check direnv is installed (or install/recommend it)
[ "$(which direnv 2>/dev/null)" ] || installOrRecommendDirenv
printSuccess "All prerequisites installed. Installing nix-bootstrap..."
installNixBootstrap "$@"
