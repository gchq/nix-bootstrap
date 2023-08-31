{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr.BuildInputs (BuildInputSpec (..), buildInputsBinding) where

import Bootstrap.Data.PreCommitHook
  ( PreCommitHooksConfig (unPreCommitHooksConfig),
  )
import Bootstrap.Data.ProjectType
  ( HasProjectSuperType (projectSuperType),
    ProjectSuperType (PSTPython),
  )
import Bootstrap.Nix.Expr
  ( Binding,
    Expr (EGrouping, EList, EWith),
    IsNixExpr (toNixExpr),
    nix,
    nixproperty,
    (|++),
    (|=),
  )

data BuildInputGroup = BIGNixpkgs [Expr] | BIGOther [Expr] | BIGPreCommitHooks | BIGPythonPackages

instance IsNixExpr BuildInputGroup where
  toNixExpr = \case
    BIGNixpkgs buildInputs -> EWith [nix|nixpkgs|] $ EList buildInputs
    BIGOther otherPackages -> EList otherPackages
    BIGPreCommitHooks -> [nix|preCommitHooks.tools|]
    BIGPythonPackages -> [nix|[pythonPackages]|]

-- | Whether this group must be wrapped with brackets to be concatenated
requiresGrouping :: BuildInputGroup -> Bool
requiresGrouping = \case
  BIGNixpkgs _ -> True
  BIGOther _ -> False
  BIGPreCommitHooks -> False
  BIGPythonPackages -> False

data BuildInputSpec projectType = BuildInputSpec
  { bisNixpkgsPackages :: [Expr],
    bisOtherPackages :: [Expr],
    bisPreCommitHooksConfig :: PreCommitHooksConfig,
    bisProjectType :: projectType
  }

buildInputsBinding :: HasProjectSuperType t => BuildInputSpec t -> Binding
buildInputsBinding spec = [nixproperty|buildInputs|] |= foldr (|++) buildInputGroupExpr1 otherBuildInputGroupExprs
  where
    buildInputGroups :: NonEmpty BuildInputGroup
    buildInputGroups = buildInputGroupsFor spec
    groupPackageSets :: Bool
    groupPackageSets = length buildInputGroups > 1
    groupToExpr :: BuildInputGroup -> Expr
    groupToExpr g = (if groupPackageSets && requiresGrouping g then EGrouping else id) (toNixExpr g)
    buildInputGroupExpr1 :: Expr
    otherBuildInputGroupExprs :: [Expr]
    (buildInputGroupExpr1 :| otherBuildInputGroupExprs) = groupToExpr <$> buildInputGroups

buildInputGroupsFor :: HasProjectSuperType t => BuildInputSpec t -> NonEmpty BuildInputGroup
buildInputGroupsFor BuildInputSpec {..} =
  BIGNixpkgs bisNixpkgsPackages
    :| catMaybes
      [ if unPreCommitHooksConfig bisPreCommitHooksConfig then Just BIGPreCommitHooks else Nothing,
        if projectSuperType bisProjectType == PSTPython then Just BIGPythonPackages else Nothing,
        case bisOtherPackages of
          [] -> Nothing
          otherPackages -> Just $ BIGOther otherPackages
      ]
