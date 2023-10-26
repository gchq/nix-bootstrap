{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr.MkShell (BuildInputSpec (..), mkShell) where

import Bootstrap.Data.PreCommitHook
  ( PreCommitHooksConfig (PreCommitHooksConfig),
  )
import Bootstrap.Data.ProjectType
  ( HasProjectSuperType (projectSuperType),
    ProjectSuperType (PSTJava, PSTRust),
  )
import Bootstrap.Nix.Expr
  ( Binding,
    Expr (ELit, ESet),
    Literal (LMultilineString),
    nix,
    nixbinding,
    nixproperty,
    (|*),
    (|=),
  )
import Bootstrap.Nix.Expr.BuildInputs
  ( BuildInputSpec (BuildInputSpec, bisNixpkgsPackages, bisPreCommitHooksConfig, bisProjectType),
    buildInputsBindings,
  )

-- | A nixpkgs.mkShell expression. Expects `nixpkgs` to be in scope.
mkShell :: HasProjectSuperType t => BuildInputSpec t -> Expr
mkShell buildInputSpec@BuildInputSpec {bisPreCommitHooksConfig, bisProjectType} =
  [nix|nixpkgs.mkShell|]
    |* ESet
      False
      ( buildInputsBindings buildInputSpec
          <> toList (shellHookBinding <$> shellHookFor bisPreCommitHooksConfig bisProjectType)
      )

data ShellHook
  = ShellHookFromPreCommit
  | ShellHookJava
  | ShellHookRust
  | ShellHookCombined (NonEmpty ShellHook)

shellHookFor :: HasProjectSuperType t => PreCommitHooksConfig -> t -> Maybe ShellHook
shellHookFor pchc pt = case (pchc, projectSuperType pt) of
  (PreCommitHooksConfig True, PSTJava) -> Just $ ShellHookCombined (ShellHookJava :| [ShellHookFromPreCommit])
  (PreCommitHooksConfig False, PSTJava) -> Just ShellHookJava
  (PreCommitHooksConfig True, PSTRust) -> Just $ ShellHookCombined (ShellHookRust :| [ShellHookFromPreCommit])
  (PreCommitHooksConfig False, PSTRust) -> Just ShellHookRust
  (PreCommitHooksConfig True, _) -> Just ShellHookFromPreCommit
  _ -> Nothing

shellHookBinding :: ShellHook -> Binding
shellHookBinding = \case
  ShellHookFromPreCommit -> [nixbinding|inherit (preCommitHooks.allHooks) shellHook;|]
  x -> withComponents $ shellHookComponentBinding x
  where
    withComponents :: [Text] -> Binding
    withComponents xs =
      [nixproperty|shellHook|]
        |= ELit
          (LMultilineString $ mconcat (("\n    " <>) <$> xs) <> "\n  ")
    shellHookComponentBinding :: ShellHook -> [Text]
    shellHookComponentBinding = \case
      ShellHookFromPreCommit -> ["${preCommitHooks.allHooks.shellHook}"]
      ShellHookJava -> ["export JAVA_HOME=\"${nixpkgs.jdk}\""]
      ShellHookRust -> ["export RUST_SRC_PATH=${nixpkgs.rustPlatform.rustLibSrc}"]
      ShellHookCombined xs -> sconcat $ shellHookComponentBinding <$> xs
