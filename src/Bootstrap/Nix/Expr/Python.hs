{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr.Python (machNixLegacyNixBinding, machNixFlakeInput, pythonPackagesBinding) where

import Bootstrap.Nix.Expr
  ( Binding,
    Expr (EGrouping, ELit, ESet),
    Literal (LString),
    nix,
    nixbinding,
    nixproperty,
    (|*),
    (|.),
    (|=),
  )

machNixVersion :: Text
machNixVersion = "3.5.0"

-- | A binding which pulls mach-nix using builtins.fetchGit
machNixLegacyNixBinding :: Binding
machNixLegacyNixBinding =
  [nixproperty|mach-nix|] |= [nix|import|]
    |* EGrouping
      ( [nix|builtins.fetchGit|]
          |* ESet
            False
            [ [nixbinding|url = "https://github.com/DavHau/mach-nix";|],
              [nixproperty|ref|] |= ELit (LString $ "refs/tags/" <> machNixVersion)
            ]
      )
    |* [nix|{}|]

-- | A binding to be used as an input in a flake to pull mach-nix
machNixFlakeInput :: Binding
machNixFlakeInput =
  [nixproperty|mach-nix.url|]
    |= ELit (LString $ "github:DavHau/mach-nix?ref=" <> machNixVersion)

-- | A binding which provides python packages based on requirements.txt.
--
-- It expects mach-nix to be in scope.
--
-- If the flake parameter is True, it also expects system to be in scope.
pythonPackagesBinding ::
  -- | Whether working with the mach-nix flake (as opposed to legacy builtins.fetchGit)
  Bool ->
  Binding
pythonPackagesBinding isFlake =
  [nixproperty|pythonPackages|]
    |= ( (if isFlake then [nix|mach-nix.lib.${system}|] else [nix|mach-nix|])
           |. [nixproperty|mkPython|]
       )
    |* [nix|rec {
          requirements = builtins.readFile ./requirements.txt;
          python = "python39";
        }|]
