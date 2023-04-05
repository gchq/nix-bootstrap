{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.DefaultNix
  ( DefaultNix (defaultNixInBlockExpr),
    defaultNixFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentNix,
  )
import Bootstrap.Data.ProjectName (ProjectName)
import Bootstrap.Data.ProjectType (JavaOptions (JavaOptions), ProjectType (Go, Java), SetUpGoBuild (SetUpGoBuild), SetUpJavaBuild (SetUpJavaBuild))
import Bootstrap.Nix.Expr
  ( Expr (ELetIn),
    IsNixExpr (toNixExpr),
    nixproperty,
    (|=),
  )
import Bootstrap.Nix.Expr.Nixpkgs (nixpkgsFromNiv)
import Bootstrap.Nix.Expr.ReproducibleBuild.Go (reproducibleGoBuild)
import Bootstrap.Nix.Expr.ReproducibleBuild.Java (reproducibleJavaBuild)

newtype DefaultNix = DefaultNix {defaultNixInBlockExpr :: Expr}

instance Bootstrappable DefaultNix where
  bootstrapName = const "default.nix"
  bootstrapReason = const "This configures your reproducible project builds."
  bootstrapContent = bootstrapContentNix

instance IsNixExpr DefaultNix where
  toNixExpr DefaultNix {..} =
    ELetIn (one $ [nixproperty|nixpkgs|] |= nixpkgsFromNiv) defaultNixInBlockExpr

defaultNixFor :: ProjectName -> ProjectType -> Maybe DefaultNix
defaultNixFor projectName = \case
  Go (SetUpGoBuild True) -> Just . DefaultNix $ reproducibleGoBuild projectName
  Java (JavaOptions _ _ (SetUpJavaBuild artefactId)) -> Just . DefaultNix $ reproducibleJavaBuild projectName artefactId
  _ -> Nothing
