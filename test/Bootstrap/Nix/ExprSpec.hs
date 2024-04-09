{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.ExprSpec (spec) where

import Bootstrap.Nix.Expr
  ( Binding (BInherit, BInheritFrom, BLineComment, BNameValue),
    CommentsPolicy (ShowComments),
    Expr
      ( EApplication,
        EFunc,
        EGrouping,
        EIdent,
        EImport,
        ELetIn,
        EList,
        EListConcatOperator,
        ELit,
        EPathConcatOperator,
        EPropertyAccess,
        ESet,
        EWith
      ),
    FunctionArgs (FAOne, FASet),
    Identifier (Identifier),
    Literal (LBool, LInteger, LMultilineString, LNull, LPath, LString),
    Property (PAntiquote, PCons, PIdent),
    isMostlyCorrectlyScoped,
    nix,
    nixargs,
    nixbinding,
    nixident,
    nixproperty,
    parseExpr,
    unsafeSimplifyBindings,
    writeExpr,
    writeExprForTerminal,
    (|*),
    (|++),
    (|.),
    (|:),
    (|=),
  )
import Data.Char (isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isPrint, isSpace)
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (Expectation, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    applyArbitrary2,
    arbitraryPrintableChar,
    choose,
    frequency,
    oneof,
    sized,
    suchThat,
    vectorOf,
    withMaxSuccess,
  )

instance Arbitrary Expr where
  arbitrary = sized arbitrary'
    where
      arbitrary' :: Int -> Gen Expr
      arbitrary' n
        | n <= 0 = genSimple
        | n >= 256 = arbitrary' 128
        | otherwise = frequency [(4, genSimple), (3, genComplex)]
        where
          genSimple =
            oneof
              [ EIdent <$> arbitrary,
                pure EImport,
                ELit <$> arbitrary
              ]
          genComplex =
            oneof
              [ frequency
                  [ ( 6,
                      EApplication
                        <$> (subexpr `suchThat` \lhs -> isAtomic lhs && couldBeAFunction lhs)
                        <*> (subexpr `suchThat` isNotApplication)
                    ),
                    ( 1,
                      EApplication
                        <$> ( EApplication <$> subexprWhichCouldBeAList
                                <*> oneof [pure EListConcatOperator, pure EPathConcatOperator]
                            )
                        <*> subexprWhichCouldBeAList
                    )
                  ],
                EFunc <$> oneof [FAOne <$> arbitrary, FASet <$> listOf1 arbitrary] <*> subexpr,
                EGrouping <$> subexpr,
                ELetIn <$> listOf1 arbitrary <*> subexpr `suchThat` isNotLetIn,
                EList <$> listOf atomicSubexpr,
                EPropertyAccess
                  <$> (atomicSubexpr `suchThat` \lhs -> isNotPathLiteral lhs && isNotPropertyAccess lhs)
                  <*> subproperty True,
                ESet <$> arbitrary <*> listOf arbitrary,
                EWith <$> subexpr <*> subexpr
              ]
          subexpr = arbitrary' (n `div` 2)
          atomicSubexpr = subexpr `suchThat` isAtomic
          subexprWhichCouldBeAList =
            subexpr `suchThat` \case
              EIdent _ -> True
              EList _ -> False
              EPropertyAccess _ _ -> True
              _ -> False
          subproperty allowCons =
            oneof $
              [PIdent <$> arbitrary, PAntiquote <$> subexpr]
                <> [PCons <$> subproperty True <*> subproperty False | allowCons]
          isNotLetIn = \case
            ELetIn _ _ -> False
            _ -> True

instance Arbitrary Binding where
  arbitrary =
    oneof
      [ BInherit <$> listOf1 arbitrary,
        BInheritFrom <$> arbitrary <*> listOf1 arbitrary,
        BLineComment . toText . toList
          <$> listOf1
            (arbitrary `suchThat` \c -> isAscii c && isPrint c && not (isSpace c) && c /= '\\'),
        BNameValue <$> (PIdent <$> arbitrary) <*> arbitrary
      ]

instance Arbitrary Identifier where
  arbitrary = Identifier . toText . unValidIdentString <$> arbitrary

instance Arbitrary Literal where
  arbitrary =
    oneof
      [ LBool <$> arbitrary,
        LInteger <$> arbitrary,
        LMultilineString . toText . unValidMultilineLiteralString <$> arbitrary,
        pure LNull,
        LPath . toText . unValidPathLiteralString <$> arbitrary,
        LString . toText . unValidLiteralString <$> arbitrary
      ]

spec :: Spec
spec = do
  describe "Expr" do
    prop "roundtrips for all expressions" $ withMaxSuccess 30000 roundtrips
    it "correctly parses the import keyword" do
      parseExpr "import" `shouldBe` Right EImport
    it "backtracks if an identifier begins with \"import\"" do
      parseExpr "imports" `shouldBe` Right (EIdent $ Identifier "imports")
    it "merges nested let-in expressions" do
      parseExpr (writeExpr ShowComments [nix|let x = 5; in let y = 4; in ""|])
        `shouldBe` Right [nix|let x = 5; y = 4; in ""|]
    describe "EApplication" do
      it "roundtrips with multiple arguments" do
        let expr = EApplication (EApplication EImport (EIdent $ Identifier "nixpkgs")) (ESet False [])
        parseExpr (writeExpr ShowComments expr) `shouldBe` Right expr
    describe "EPropertyAccess" do
      it "correctly parses a property access expression" do
        parseExpr "(let a = \"hello\"; b = \"world\"; in a).b"
          `shouldBe` Right
            ( EPropertyAccess
                ( EGrouping
                    ( ELetIn
                        ( (BNameValue (PIdent $ Identifier "a") . ELit $ LString "hello")
                            :| one (BNameValue (PIdent $ Identifier "b") . ELit $ LString "world")
                        )
                        (EIdent $ Identifier "a")
                    )
                )
                (PIdent $ Identifier "b")
            )
      it "correctly associates left on an ambiguous pairing with property access" do
        parseExpr "let a = \"hello\"; b = \"world\"; in a.b"
          `shouldBe` Right
            ( ELetIn
                ( (BNameValue (PIdent $ Identifier "a") . ELit $ LString "hello")
                    :| one (BNameValue (PIdent $ Identifier "b") . ELit $ LString "world")
                )
                (EPropertyAccess (EIdent $ Identifier "a") (PIdent $ Identifier "b"))
            )
      it "rejects property access if there is a space on either side of the dot" do
        parseExpr "a .b" `shouldSatisfy` isLeft
        parseExpr "a. b" `shouldSatisfy` isLeft
    describe "(|*)" do
      it "correctly applies arguments" do
        ([nix|import|] |* [nix|nixpkgs|] |* [nix|{}|])
          `shouldBe` EApplication
            ( EApplication
                EImport
                (EIdent $ Identifier "nixpkgs")
            )
            (ESet False [])
    describe "(|.)" do
      it "correctly applies arguments" do
        ([nix|a|] |. [nixproperty|.b|] |. [nixproperty|.c|])
          `shouldBe` EPropertyAccess
            ( EPropertyAccess
                (EIdent $ Identifier "a")
                (PIdent $ Identifier "b")
            )
            (PIdent $ Identifier "c")
    describe "(|=)" do
      it "correctly applies arguments" do
        ([nixproperty|a|] |= [nix|b|])
          `shouldBe` BNameValue (PIdent $ Identifier "a") (EIdent $ Identifier "b")
    describe "(|:)" do
      it "correctly applies arguments" do
        ([nixargs|x:|] |: [nix|x|])
          `shouldBe` [nix|x: x|]
    describe "(|++)" do
      it "correctly applies arguments" do
        ([nix|[a b]|] |++ [nix|[c d]|])
          `shouldBe` [nix|[a b] ++ [c d]|]
    describe "operator precedence" do
      it "(|.) has higher precedece than (|*)" do
        ( [nixargs|{ a, b, c, d, e }:|]
            |: [nix|a|] |* [nix|b|] |. [nixproperty|.c|] |* [nix|d|] |. [nixproperty|.e|]
          )
          `shouldBe` EFunc
            (FASet ([nixident|a|] :| (Identifier <$> ["b", "c", "d", "e"])))
            ( EApplication
                ( EApplication
                    (EIdent (Identifier "a"))
                    ( EPropertyAccess
                        (EIdent (Identifier "b"))
                        (PIdent (Identifier "c"))
                    )
                )
                ( EPropertyAccess
                    (EIdent (Identifier "d"))
                    (PIdent (Identifier "e"))
                )
            )
      it "(|=) has higher precedece than (|*) and (|.)" do
        [nixproperty|a|] |= [nix|b|] |* [nix|c|] |. [nixproperty|.d|]
          `shouldBe` [nixbinding|a = b c.d;|]
    describe "Identifier" do
      it "correctly parses the special \"...\" identifier" do
        [nixident|...|] `shouldBe` Identifier "..."
    describe "Literal" do
      describe "LMultilineString" do
        it "roundtrips when there are escape sequences in the literal" do
          roundtrips (ELit $ LMultilineString "hello '''' ''$ world")
      describe "LString" do
        it "roundtrips when there are escaped quotes in the literal" do
          roundtrips (ELit $ LString "hello \" world")
  describe "writeExprForTerminal" do
    it "correctly strips out line comments" do
      writeExprForTerminal
        [nix|let x = 5;
        # this is a comment
        y = 6;
        in x.${y}.z|]
        `shouldBe` "let x = 5; y = 6; in x.\\${y}.z"
  describe "isMostlyCorrectlyScoped" do
    describe "when given correctly-scoped expressions" do
      it "returns True for [nix|\"Hello, world!\"|]" do
        isMostlyCorrectlyScoped [nix|"Hello, world!"|] `shouldBe` Right ()
      it "returns Right () for [nix|let x = 5; in x|]" do
        isMostlyCorrectlyScoped [nix|let x = 5; in x|] `shouldBe` Right ()
      it "returns Right () for [nix|let x = 5; y = x; in y|]" do
        isMostlyCorrectlyScoped [nix|let x = 5; y = x; in y|] `shouldBe` Right ()
      it "returns Right () for [nix|let x = { a = 1; b = 2; }; inherit (x) a; in x a|]" do
        isMostlyCorrectlyScoped [nix|let x = { a = 1; b = 2; }; inherit (x) a; in x a|] `shouldBe` Right ()
      it "returns Right () for [nix|{ a = 1; b = a; }|]" do
        isMostlyCorrectlyScoped [nix|{ a = 1; b = a; }|] `shouldBe` Right ()
      it "returns Right () for [nix|rec { a = b; b = 1; }|]" do
        isMostlyCorrectlyScoped [nix|rec { a = b; b = 1; }|] `shouldBe` Right ()
      it "returns Right () for [nix|builtins.readFile ./test.txt|]" do
        isMostlyCorrectlyScoped [nix|builtins.readFile ./test.txt|] `shouldBe` Right ()
    describe "when given ambiguous scope" do
      it "returns Right () for [nix|let lib = import <nixpkgs/lib> {}; in lib.mkShellScriptBin|]" do
        isMostlyCorrectlyScoped [nix|let lib = import <nixpkgs/lib> {}; in lib.mkShellScriptBin|] `shouldBe` Right ()
      it "returns Right () for [nix|with import ./someOtherFile.nix; a|]" do
        isMostlyCorrectlyScoped [nix|with import ./someOtherFile.nix; a|] `shouldBe` Right ()
    describe "when given decidably badly-scoped expressions" do
      it "returns Left for [nix|x|]" do
        isMostlyCorrectlyScoped [nix|x|] `shouldBe` Left (one [nixident|x|])
      it "returns Left for [nix|b a|]" do
        isMostlyCorrectlyScoped [nix|b a|] `shouldBe` Left ([nixident|b|] :| [[nixident|a|]])
      it "returns Left for [nix|let x = 5; in y|]" do
        isMostlyCorrectlyScoped [nix|let x = 5; in y|] `shouldBe` Left (one [nixident|y|])
      it "returns Left for [nix|let y = x; x = 5; in y|]" do
        isMostlyCorrectlyScoped [nix|let y = x; x = 5; in y|] `shouldBe` Left (one [nixident|x|])
      it "returns Left for [nix|{ a = b; b = 1; }|]" do
        isMostlyCorrectlyScoped [nix|{ a = b; b = 1; }|] `shouldBe` Left (one [nixident|b|])
  describe "unsafeSimplifyBindings" do
    describe "when given bindings including comments" do
      it "does nothing" do
        let bindings = case [nix|{
        a = "b";
        b = 5;
        # We take d from somewhere in scope
        inherit d;
}|] of
              ESet False bs -> fromList bs
              _ -> error "unsafeSimplifyBindings test: initial set was not correct"
        unsafeSimplifyBindings bindings `shouldBe` bindings
  describe "when given non-comment bindings" do
    it "simplifies and sorts them" do
      let bindings = case [nix|{
        b = "bbbbb";
        inherit (thing1) thing6;
        a = "aaaaa";
        inherit (thing1) thing3 thing4;
        inherit l m n o p;
        inherit (thing5) y x;
        inherit d;
}|] of
            ESet False bs -> fromList bs
            _ -> error "unsafeSimplifyBindings test: initial set was not correct"
      ESet False (toList $ unsafeSimplifyBindings bindings)
        `shouldBe` [nix|{
        inherit d l m n o p;
        inherit (thing1) thing3 thing4 thing6;
        inherit (thing5) x y;
        a = "aaaaa";
        b = "bbbbb";
}|]

-- | An expectation that a Nix expression is the same when written out and parsed again.
roundtrips :: Expr -> Expectation
roundtrips e = parseExpr (writeExpr ShowComments e) `shouldBe` Right e

-- Arbitrary Helpers

-- | Like `Test.QuickCheck.Gen.listOf` but with a maximum length of 5
listOf :: Gen a -> Gen [a]
listOf gen = do
  k <- choose (0, 5)
  vectorOf k gen

-- | Like `Test.QuickCheck.Gen.listOf1` but with a maximum length of 5
listOf1 :: Gen a -> Gen (NonEmpty a)
listOf1 gen = do
  k <- choose (1, 5)
  Unsafe.fromJust . nonEmpty <$> vectorOf k gen

-- | A wrapper around String whose Arbitrary instance only allows valid identifiers
newtype ValidIdentString = ValidIdentString {unValidIdentString :: String}

-- | A wrapper around Char whose Arbitrary instance only allows valid first characters of identifiers
newtype IdentFirstChar = IdentFirstChar Char

instance Arbitrary IdentFirstChar where
  arbitrary = IdentFirstChar <$> arbitrary `suchThat` \c -> c == '_' || isAsciiLower c || isAsciiUpper c

-- | A wrapper around String whose Arbitrary instance only allows valid characters of identifiers
newtype IdentRest = IdentRest String

instance Arbitrary IdentRest where
  arbitrary = IdentRest <$> listOf (arbitrary `suchThat` \c -> c == '_' || c == '-' || isAlphaNum c && isAscii c)

instance Arbitrary ValidIdentString where
  arbitrary = ValidIdentString <$> applyArbitrary2 \(IdentFirstChar c) (IdentRest r) -> c : r

-- | A wrapper around String whose Arbitrary instance only allows printable
-- characters which are not backslash
newtype ValidLiteralString = ValidLiteralString {unValidLiteralString :: String}

instance Arbitrary ValidLiteralString where
  arbitrary =
    ValidLiteralString
      <$> listOf
        (arbitraryPrintableChar `suchThat` \c -> c /= '\\')

-- | A wrapper around String whose Arbitrary instance only allows printable
-- characters which are not backslash or single quotes
newtype ValidMultilineLiteralString = ValidMultilineLiteralString {unValidMultilineLiteralString :: String}

instance Arbitrary ValidMultilineLiteralString where
  arbitrary =
    ValidMultilineLiteralString
      <$> listOf
        (arbitraryPrintableChar `suchThat` \c -> c /= '\\' && c /= '\'')

-- | A wrapper around String whose Arbitrary instance only allows characters allowable
-- in a path literal in nix
newtype ValidPathLiteralString = ValidPathLiteralString {unValidPathLiteralString :: String}

instance Arbitrary ValidPathLiteralString where
  arbitrary = do
    numCharsBeforeFirstSlash <- choose (0, 5)
    charsBeforeFirstSlash <- vectorOf numCharsBeforeFirstSlash $ c `suchThat` (/= '$')
    numSegments <- choose (0, 3)
    segments <- vectorOf numSegments segment
    addFinalSlash <- if numSegments == 0 then pure True else (== 0) <$> choose @Int (0, 1)
    let p = charsBeforeFirstSlash <> concat segments <> if addFinalSlash then "/" else ""
    pure . ValidPathLiteralString $ if p == "/" then "/." else p
    where
      c :: Gen Char
      c =
        arbitraryPrintableChar `suchThat` \c' ->
          not (isSpace c')
            && notElem
              c'
              ['/', ' ', '\0', ';', '\'', '"', '(', ')', '[', ']', '{', '}', '\\', '=', '+']
      segment :: Gen String
      segment = do
        k <- choose (1, 7)
        ('/' :) <$> vectorOf k c

isNotApplication :: Expr -> Bool
isNotApplication = \case
  EApplication _ _ -> False
  _ -> True

isNotPathLiteral :: Expr -> Bool
isNotPathLiteral = \case
  ELit (LPath _) -> False
  _ -> True

isNotPropertyAccess :: Expr -> Bool
isNotPropertyAccess = \case
  EPropertyAccess _ _ -> False
  _ -> True

-- | Expressions that could not be parsed as applications (this also excludes constructs such
-- as let-in expressions which should be grouped in whitespace-sensitive contexts such as
-- lists)
isAtomic :: Expr -> Bool
isAtomic = \case
  EApplication _ _ -> False
  EFunc _ _ -> False
  EGrouping _ -> True
  EIdent _ -> True
  EImport -> True
  ELetIn _ _ -> False
  EList _ -> True
  EListConcatOperator -> True
  EPathConcatOperator -> True
  ELit l -> case l of
    LInteger i -> i >= 0
    _ -> True
  EPropertyAccess _ _ -> True
  ESet isRec _ -> not isRec
  EWith _ _ -> False

-- | Returns `True` if the given expression could plausibly be a function in nix code
couldBeAFunction :: Expr -> Bool
couldBeAFunction = \case
  -- An application could be, but for simplicity we'll say it couldn't
  -- and test multiple-argument applications another way
  EApplication _ _ -> False
  EFunc _ _ -> True
  EGrouping e -> couldBeAFunction e
  EIdent _ -> True
  EImport -> True
  ELetIn _ e -> couldBeAFunction e
  EList _ -> False
  EListConcatOperator -> True
  EPathConcatOperator -> True
  ELit _ -> False
  EPropertyAccess _ _ -> True
  ESet _ _ -> False
  EWith _ _ -> True
