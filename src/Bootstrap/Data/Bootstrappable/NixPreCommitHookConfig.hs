{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig
  ( NixPreCommitHookConfig (nixPreCommitHookConfigRequiresNixpkgs),
    PreCommitHook,
    nixPreCommitHookConfigFor,
  )
where

import Bootstrap.Cli (RunConfig (RunConfig, rcUseFlakes))
import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
    bootstrapContentNix,
  )
import Bootstrap.Data.ProjectType
  ( HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic, HaskellProjectTypeReplOnly),
    ProjectType
      ( Elm,
        Go,
        Haskell,
        Java,
        Minimal,
        Node,
        Python,
        Rust
      ),
  )
import Bootstrap.Nix.Expr
  ( Binding (BNameValue),
    Expr (EGrouping, EIdent, EList, ELit, ESet, EWith),
    FunctionArgs (FASet),
    Identifier (Identifier),
    IsNixExpr (toNixExpr),
    Literal (LBool, LString),
    Property (PCons, PIdent),
    nix,
    nixbinding,
    nixident,
    nixproperty,
    (|*),
    (|++),
    (|:),
    (|=),
  )

data NixPreCommitHookConfig = NixPreCommitHookConfig
  { nixPreCommitHookConfigHooks :: [PreCommitHook],
    nixPreCommitHookConfigRequiresNixpkgs :: Bool,
    nixPreCommitHookConfigUsingFlakeLib :: Bool
  }

instance Bootstrappable NixPreCommitHookConfig where
  bootstrapName = const "nix/pre-commit-hooks.nix"
  bootstrapReason = const "This configures which pre-commit hooks are used."
  bootstrapContent = bootstrapContentNix

instance IsNixExpr NixPreCommitHookConfig where
  toNixExpr NixPreCommitHookConfig {..} =
    FASet
      ( [nixident|pre-commit-hooks-lib|]
          :| [[nixident|nixpkgs|] | nixPreCommitHookConfigRequiresNixpkgs]
          <> [[nixident|system|] | nixPreCommitHookConfigUsingFlakeLib]
      )
      |: ESet
        False
        [ [nixproperty|hooks|] |= run |* ESet False runArgs,
          [nixproperty|tools|]
            |= if nixPreCommitHookConfigRequiresNixpkgs
              then defaultToolsExpr |++ nixpkgsToolsExpr
              else defaultToolsExpr
        ]
    where
      run :: Expr
      run =
        if nixPreCommitHookConfigUsingFlakeLib
          then [nix|pre-commit-hooks-lib.lib.${system}.run|]
          else [nix|pre-commit-hooks-lib.run|]
      runArgs :: [Binding]
      runArgs =
        [ [nixbinding|src = ../.;|],
          [nixproperty|hooks|] |= ESet False (catMaybes (toBinding <$> sort nixPreCommitHookConfigHooks))
        ]
      mkToolsExpr :: Expr -> (PreCommitHookTool -> Bool) -> Expr
      mkToolsExpr parentSet condition =
        EGrouping . EWith parentSet $
          EList $
            catMaybes
              ( fmap (EIdent . Identifier) . preCommitHookToolSubAttrName
                  <$> sortNub (filter condition (tool <$> nixPreCommitHookConfigHooks))
              )
      libPackageSet :: Expr
      libPackageSet =
        if nixPreCommitHookConfigUsingFlakeLib
          then [nix|pre-commit-hooks-lib.packages.${system}|]
          else [nix|pre-commit-hooks-lib|]
      defaultToolsExpr = mkToolsExpr libPackageSet preCommitHookToolIsFromHooksLib
      nixpkgsToolsExpr = mkToolsExpr [nix|nixpkgs|] preCommitHookToolIsFromNixpkgs

nixPreCommitHookConfigFor :: RunConfig -> ProjectType -> NixPreCommitHookConfig
nixPreCommitHookConfigFor RunConfig {rcUseFlakes} projectType =
  let nixPreCommitHookConfigHooks =
        alejandra : case projectType of
          Minimal -> []
          Elm _ -> [elmFormat, elmReview, prettier]
          Haskell (HaskellOptions _ HaskellProjectTypeReplOnly) -> []
          Haskell (HaskellOptions _ HaskellProjectTypeBasic) -> [hlint, hpack, ormolu]
          Node _ -> [prettier]
          Go _ -> [goFmt, goTest]
          Java {} -> [googleJavaFormat]
          Python _ -> []
          Rust -> [cargoCheck, clippy, rustfmt]
      nixPreCommitHookConfigRequiresNixpkgs =
        any preCommitHookToolIsFromNixpkgs (tool <$> nixPreCommitHookConfigHooks)
          || hpack `elem` nixPreCommitHookConfigHooks
   in NixPreCommitHookConfig {nixPreCommitHookConfigUsingFlakeLib = rcUseFlakes, ..}

data PreCommitHook = PreCommitHook
  { attrName :: Text,
    enable :: Bool,
    entry :: Maybe Text,
    excludes :: Maybe [Text],
    files :: Maybe Expr,
    name :: Maybe Text,
    pass_filenames :: Maybe Bool,
    types_or :: Maybe [Text],
    tool :: PreCommitHookTool
  }
  deriving stock (Eq)

instance Ord PreCommitHook where
  p1 <= p2 = attrName p1 <= attrName p2

-- | Represent a child attribute of the set nixpkgs
newtype NixpkgsSubAttrName = NixpkgsSubAttrName Text deriving stock (Eq)

data PreCommitHookTool
  = DefaultPreCommitHookTool Text
  | NixpkgsPreCommitHookTool NixpkgsSubAttrName
  | -- | Don't pass a tool from the pre-commit library back up to the shell, use the shell's
    -- own instead
    UseShellPreCommitHookTool
  deriving stock (Eq)

instance Ord PreCommitHookTool where
  t1 <= t2 = textValueOf t1 <= textValueOf t2
    where
      textValueOf = \case
        DefaultPreCommitHookTool t -> t
        NixpkgsPreCommitHookTool (NixpkgsSubAttrName t) -> t
        UseShellPreCommitHookTool -> ""

preCommitHookToolIsFromHooksLib :: PreCommitHookTool -> Bool
preCommitHookToolIsFromHooksLib (DefaultPreCommitHookTool _) = True
preCommitHookToolIsFromHooksLib _ = False

preCommitHookToolIsFromNixpkgs :: PreCommitHookTool -> Bool
preCommitHookToolIsFromNixpkgs (NixpkgsPreCommitHookTool _) = True
preCommitHookToolIsFromNixpkgs _ = False

-- | Gets the attribute name within its containing set (not including the containing set)
preCommitHookToolSubAttrName :: PreCommitHookTool -> Maybe Text
preCommitHookToolSubAttrName (DefaultPreCommitHookTool attrName) = Just attrName
preCommitHookToolSubAttrName (NixpkgsPreCommitHookTool (NixpkgsSubAttrName attrName)) = Just attrName
preCommitHookToolSubAttrName UseShellPreCommitHookTool = Nothing

toBinding :: PreCommitHook -> Maybe Binding
toBinding PreCommitHook {..} =
  if not enable
    then Nothing
    else
      Just
        if all isNothing [entry, name] && all isNothing [excludes, types_or] && isNothing files && isNothing pass_filenames
          then (PIdent (Identifier attrName) `PCons` [nixproperty|.enable|]) |= [nix|true|]
          else
            PIdent (Identifier attrName)
              |= ESet
                False
                ( uncurry BNameValue
                    <$> catMaybesInSecond
                      [ ([nixproperty|enable|], Just [nix|true|]),
                        ([nixproperty|entry|], ELit . LString <$> entry),
                        ([nixproperty|excludes|], EList . fmap (ELit . LString) <$> excludes),
                        ([nixproperty|files|], files),
                        ([nixproperty|name|], ELit . LString <$> name),
                        ([nixproperty|pass_filenames|], ELit . LBool <$> pass_filenames),
                        ([nixproperty|types_or|], EList . fmap (ELit . LString) <$> types_or)
                      ]
                )
  where
    catMaybesInSecond :: [(Property, Maybe Expr)] -> [(Property, Expr)]
    catMaybesInSecond [] = []
    catMaybesInSecond (x : xs) =
      let rs = catMaybesInSecond xs
       in case snd x of
            Just r -> (fst x, r) : rs
            Nothing -> rs

nixpkgsPreCommitHookToolDefaultEntry :: NixpkgsSubAttrName -> Text
nixpkgsPreCommitHookToolDefaultEntry (NixpkgsSubAttrName attrName) = "${nixpkgs." <> attrName <> "}/bin/" <> attrName

defaultPreCommitHook :: Text -> PreCommitHookTool -> PreCommitHook
defaultPreCommitHook name tool =
  PreCommitHook
    { attrName = name,
      enable = True,
      entry = Nothing,
      excludes = Nothing,
      files = Nothing,
      name = Nothing,
      pass_filenames = Nothing,
      types_or = Nothing,
      tool = tool
    }

alejandra :: PreCommitHook
alejandra = defaultPreCommitHook "alejandra" $ DefaultPreCommitHookTool "alejandra"

cargoCheck :: PreCommitHook
cargoCheck = defaultPreCommitHook "cargo-check" UseShellPreCommitHookTool

clippy :: PreCommitHook
clippy = defaultPreCommitHook "clippy" $ DefaultPreCommitHookTool "clippy"

elmFormat :: PreCommitHook
elmFormat = defaultPreCommitHook "elm-format" $ DefaultPreCommitHookTool "elm-format"

elmReview :: PreCommitHook
elmReview = defaultPreCommitHook "elm-review" $ DefaultPreCommitHookTool "elm-review"

hlint :: PreCommitHook
hlint = defaultPreCommitHook "hlint" $ DefaultPreCommitHookTool "hlint"

hpack :: PreCommitHook
hpack =
  (defaultPreCommitHook "hpack" $ DefaultPreCommitHookTool "hpack")
    { files = Just [nix|nixpkgs.lib.mkOverride 0 "(\\.l?hs$)|cabal\\.project|([^/]+\\.cabal$)|(package\\.yaml$)"|]
    }

ormolu :: PreCommitHook
ormolu = defaultPreCommitHook "ormolu" $ DefaultPreCommitHookTool "ormolu"

prettier :: PreCommitHook
prettier = defaultPreCommitHook "prettier" $ DefaultPreCommitHookTool "prettier"

rustfmt :: PreCommitHook
rustfmt = defaultPreCommitHook "rustfmt" $ DefaultPreCommitHookTool "rustfmt"

googleJavaFormat :: PreCommitHook
googleJavaFormat =
  let toolAttrName = NixpkgsSubAttrName "google-java-format"
   in (defaultPreCommitHook "google-java-format" $ NixpkgsPreCommitHookTool toolAttrName)
        { entry = Just $ nixpkgsPreCommitHookToolDefaultEntry toolAttrName <> " -i",
          files = Just [nix|"\\\\.java$"|],
          pass_filenames = Just True
        }

goFmt :: PreCommitHook
goFmt =
  let toolAttrName = NixpkgsSubAttrName "go"
   in (defaultPreCommitHook "go-fmt" $ NixpkgsPreCommitHookTool toolAttrName)
        { entry = Just $ nixpkgsPreCommitHookToolDefaultEntry toolAttrName <> " fmt",
          files = Just [nix|"\\\\.go$"|],
          pass_filenames = Just False
        }

goTest :: PreCommitHook
goTest =
  let toolAttrName = NixpkgsSubAttrName "go"
   in (defaultPreCommitHook "go-test" $ NixpkgsPreCommitHookTool toolAttrName)
        { entry = Just $ nixpkgsPreCommitHookToolDefaultEntry toolAttrName <> " test",
          files = Just [nix|"\\\\.(go|mod)$"|],
          pass_filenames = Just False
        }
