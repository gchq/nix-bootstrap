{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr.ReproducibleBuild.Haskell (reproducibleHaskellBuild) where

import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Nix.Expr
  ( Expr (EIdent, ELetIn, ELit, ESet),
    Literal (LMultilineString, LString),
    nix,
    nixbinding,
    nixproperty,
    (|*),
    (|.),
    (|=),
  )
import Bootstrap.Nix.Expr.ReproducibleBuild
  ( ReproducibleBuildExpr (ReproducibleBuildExpr),
    ReproducibleBuildRequirement (RBRHaskellPackages, RBRNixpkgs),
    reproducibleBuildRequirementIdentifier,
  )
import Text.RawString.QQ (r)

reproducibleHaskellBuild ::
  ProjectName ->
  -- | src path
  Expr ->
  ReproducibleBuildExpr
reproducibleHaskellBuild projectName srcDir =
  ReproducibleBuildExpr
    ( ELetIn
        ( [nixbinding|# This is the core build of your program, but its closure includes all build inputs|]
            :| [ [nixproperty|unstripped|]
                   |= ( (EIdent (reproducibleBuildRequirementIdentifier RBRHaskellPackages) |. [nixproperty|callCabal2nix|])
                          |* ELit (LString $ unProjectName projectName)
                          |* srcDir
                          |* ESet False []
                      )
               ]
        )
        ( [nix|nixpkgs.stdenv.mkDerivation|]
            |* ESet
              False
              [ [nixbinding|# This derivation strips out the build inputs from `unstripped` above to leave just your program|],
                [nixbinding|inherit (unstripped) name src version;|],
                [nixbinding|buildInputs = with nixpkgs; [
  # This is an assumed set of system libraries needed; you can add to this as necessary.
  # You can find out what your package needs by reading the output of `ldd` on your built binary.
  glibc
  gmp
  libffi
  # ncurses and zlib are not needed for the bootstrapped program, but are needed by lots of common Haskell libraries
  ncurses
  zlib
];|],
                [nixproperty|installPhase|]
                  |= ( ELit . LMultilineString $
                         [r|
  mkdir -p $out/bin
  cp ${(nixpkgs.haskell.lib.enableSeparateBinOutput unstripped).bin}/bin/app $out/bin/|]
                           <> unProjectName projectName
                     )
              ]
        )
    )
    (RBRHaskellPackages :| [RBRNixpkgs])
