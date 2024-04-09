{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Defines a type to represent Nix Expressions along with
--   its parser, formatter, and quasiquoter.
-- Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr
  ( -- * IsNixExpr Typeclass

    -- | The `IsNixExpr` typeclass allows representation of other datatypes
    -- as nix expressions
    IsNixExpr (..),

    -- * Expression Types

    -- | The provided types represent the
    -- possible structures of a Nix expression.
    Expr (..),
    FunctionArgs (..),
    Binding (..),
    Identifier (..),
    Literal (..),
    Property (..),

    -- * Operators

    -- | Application
    (|*),
    -- | Binding (BNameValue)
    (|=),
    -- | Function arguments
    (|:),
    -- | List concatenation
    (|++),
    -- | Property Access
    (|.),

    -- * Parsing

    -- | `parseExpr` parses Nix expressions as `Expr`s.
    parseExpr,
    -- | Various nix constructs can be parsed at compile time through
    -- quasiquoters.
    nix,
    nixargs,
    nixbinding,
    nixident,
    nixproperty,

    -- * Formatting

    -- | `writeExpr` writes out an `Expr` without formatting it.
    writeExpr,
    -- | `writeExprFormatted` writes out an `Expr`, formatting it with alejandra.
    writeExprFormatted,
    -- | `writeExprForTerminal` writes out an `Expr` for use on a command line.
    writeExprForTerminal,
    -- | `writeBinding` writes out a `Binding` without formatting it.
    writeBinding,
    CommentsPolicy (..),

    -- * Validation

    -- | `isMostlyCorrectlyScoped` returns `Right ()` if all of the identifiers used are in scope,
    -- although will give some false positives in cases of sets which may have unknown attributes.
    isMostlyCorrectlyScoped,

    -- * Other helpers

    -- | Deduplicates and organises a set of bindings; note it is the caller's responsibility to
    -- check that these bindings don't reference each other as the reordering could make this fail.
    --
    -- The unsafe prefix simply highlights this responsibility.
    unsafeSimplifyBindings,
  )
where

import Bootstrap.Unix (alejandra)
import Control.Applicative.Combinators (option)
import Control.Exception (IOException)
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Data.Data (Data, cast)
import Data.List.NonEmpty ((<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
  ( QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType),
  )
import Language.Haskell.TH.Syntax as THS
  ( Lift (lift),
    Q,
    dataToExpQ,
    dataToPatQ,
  )
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec
  ( ErrorItem (Label, Tokens),
    MonadParsec
      ( eof,
        label,
        lookAhead,
        notFollowedBy,
        try
      ),
    Parsec,
    between,
    choice,
    errorBundlePretty,
    failure,
    manyTill,
    parse,
    someTill,
    (<?>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | Class representing things with canonical representations in Nix
class IsNixExpr a where
  toNixExpr :: a -> Expr

instance IsNixExpr Expr where
  toNixExpr = id

instance IsNixExpr Bool where
  toNixExpr = ELit . LBool

instance IsNixExpr Int where
  toNixExpr = ELit . LInteger

-- | A representation of a Nix expression
data Expr
  = -- | Function application
    EApplication Expr Expr
  | -- | Function
    EFunc FunctionArgs Expr
  | -- | A grouping (within parentheses) to make an otherwise non-atomic expression atomic
    EGrouping Expr
  | -- | Identifier
    EIdent Identifier
  | -- | Import - a special case of identifier
    EImport
  | -- | Let-In expression
    ELetIn (NonEmpty Binding) Expr
  | -- | List
    EList [Expr]
  | -- | List concatenation operator (++)
    EListConcatOperator
  | -- | Path concatenation operator (+)
    EPathConcatOperator
  | -- | Literal
    ELit Literal
  | -- | Property access (.)
    EPropertyAccess Expr Property
  | -- | Set
    ESet Bool [Binding]
  | -- | With expression
    EWith Expr Expr
  deriving stock (Data, Eq, Show)

infixl 5 |:

-- | Operator form of separation of function args from function body (`EFunc`)
(|:) :: FunctionArgs -> Expr -> Expr
(|:) = EFunc

infixl 6 |*

-- | Operator form of expression application (`EApplication`)
(|*) :: Expr -> Expr -> Expr
(|*) = EApplication

infixl 8 |.

-- | Operator form of property access (`EPropertyAccess`)
(|.) :: Expr -> Property -> Expr
(|.) = EPropertyAccess

infixl 7 |++

-- | Operator to simplify list concatenation
(|++) :: Expr -> Expr -> Expr
(|++) e1 = (e1 |* EListConcatOperator |*)

-- | Whether to include comments when formatting
data CommentsPolicy = ShowComments | HideComments

-- | Writes out an `Expr` as Nix code
writeExpr :: CommentsPolicy -> Expr -> Text
writeExpr cp =
  \case
    EApplication e1 e2 -> writeExpr cp e1 <> " " <> writeExpr cp e2
    EFunc args e -> writeFunctionArgs args <> writeExpr cp e
    EGrouping e -> "(" <> writeExpr cp e <> ")"
    EIdent (Identifier i) -> i
    EImport -> "import"
    ELetIn bindings e -> "let " <> sconcat (writeBinding cp <$> bindings) <> "in " <> writeExpr cp e
    EList exprs ->
      "["
        <> (if length exprs > 2 then "\n" else "")
        <> T.concat (intersperse " " (writeExpr cp <$> exprs))
        <> "]"
    EListConcatOperator -> "++"
    EPathConcatOperator -> "+"
    ELit l -> writeLiteral l
    EPropertyAccess e1 p -> writeExpr cp e1 <> "." <> writeProperty cp p
    ESet isRec bindings ->
      (if isRec then "rec " else "")
        <> "{"
        <> (if length bindings > 2 then "\n" else " ")
        <> mconcat (writeBinding cp <$> bindings)
        <> "}"
    EWith additionalScope e -> "with " <> writeExpr cp additionalScope <> "; " <> writeExpr cp e
    . mergeNestedLetExprs

-- | Recursively merges nested let expressions into a single let in block
mergeNestedLetExprs :: Expr -> Expr
mergeNestedLetExprs = \case
  EApplication e1 e2 -> EApplication (mergeNestedLetExprs e1) (mergeNestedLetExprs e2)
  EFunc args e -> EFunc args (mergeNestedLetExprs e)
  EGrouping e -> EGrouping (mergeNestedLetExprs e)
  EIdent i -> EIdent i
  EImport -> EImport
  ELetIn bindings e -> case e of
    ELetIn bindings' e' -> ELetIn (mergeNestedLetExprsB <$> (bindings <> bindings')) (mergeNestedLetExprs e')
    _ -> ELetIn bindings (mergeNestedLetExprs e)
  EList exprs -> EList (mergeNestedLetExprs <$> exprs)
  EListConcatOperator -> EListConcatOperator
  EPathConcatOperator -> EPathConcatOperator
  ELit l -> ELit l
  EPropertyAccess e1 p -> EPropertyAccess (mergeNestedLetExprs e1) (mergeNestedLetExprsP p)
  ESet isRec bindings -> ESet isRec (mergeNestedLetExprsB <$> bindings)
  EWith additionalScope e -> EWith (mergeNestedLetExprs additionalScope) (mergeNestedLetExprs e)
  where
    mergeNestedLetExprsB :: Binding -> Binding
    mergeNestedLetExprsB = \case
      BInherit xs -> BInherit xs
      BInheritFrom from xs -> BInheritFrom (mergeNestedLetExprs from) xs
      BLineComment c -> BLineComment c
      BNameValue p v -> BNameValue (mergeNestedLetExprsP p) (mergeNestedLetExprs v)
    mergeNestedLetExprsP :: Property -> Property
    mergeNestedLetExprsP = \case
      PIdent i -> PIdent i
      PAntiquote e -> PAntiquote (mergeNestedLetExprs e)
      PCons p1 p2 -> PCons (mergeNestedLetExprsP p1) (mergeNestedLetExprsP p2)

-- | Writes out an `Expr` as Nix code, formatting it with alejandra
writeExprFormatted :: MonadIO m => CommentsPolicy -> Expr -> m (Either IOException Text)
writeExprFormatted cp = runExceptT . (fmap toText <$> alejandra . toString . writeExpr cp)

-- | Like writeExpr, but strips out line comments,
-- replaces whitespace groups with a single space,
-- and escapes dollar symbols.
writeExprForTerminal :: Expr -> Text
writeExprForTerminal e =
  unwords . words . T.replace "$" "\\$" $ writeExpr HideComments e

-- | Runs `parseE`, parsing a Nix expression
parseExpr :: Text -> Either Text Expr
parseExpr =
  first (toText . errorBundlePretty)
    . parse (between skipSpace eof $ parseE False) ""

-- | Parses an `Expr`
parseE ::
  -- | Whether to enforce that the parsed expression is atomic (see `parseAtom`)
  Bool ->
  Parser Expr
parseE atomic = makeExprParser (if atomic then parseAtom True else parseNonOperatorExpr) operators
  where
    operators =
      [[InfixL (EApplication <$ L.symbol skipSpace "")] | not atomic]

-- | Parses a non-operator `Expr`
parseNonOperatorExpr :: Parser Expr
parseNonOperatorExpr =
  choice
    [ uncurry EPropertyAccess <$> parsePropertyAccess,
      EGrouping <$> parseGrouping,
      ELit <$> parseLiteral False,
      uncurry EFunc <$> parseFunction,
      parseImport,
      uncurry ELetIn <$> parseLetIn,
      uncurry EWith <$> parseWith,
      uncurry ESet <$> parseSet True,
      EIdent <$> parseIdentifier,
      EList <$> parseList,
      EListConcatOperator <$ lexeme (symbol "++"),
      EPathConcatOperator <$ lexeme (symbol "+")
    ]

-- | Parses an expression which is considered atomic by nix - i.e. it is unambiguously a
-- single expression (as opposed to an application of multiple expressions, for example).
parseAtom ::
  -- | Whether to allow this to be property access or any other operator-like expression; stops infinite loops
  Bool ->
  Parser Expr
parseAtom allowPropertyAccess =
  choice $
    [uncurry EPropertyAccess <$> parsePropertyAccess | allowPropertyAccess]
      <> [EGrouping <$> parseGrouping]
      <> [EListConcatOperator <$ lexeme (symbol "++") | allowPropertyAccess]
      <> [EPathConcatOperator <$ lexeme (symbol "+") | allowPropertyAccess]
      <> [ ELit <$> parseLiteral True,
           parseImport,
           uncurry ESet <$> parseSet False,
           EIdent <$> parseIdentifier,
           EList <$> parseList
         ]

-- | A representation of the arguments to a Nix function
data FunctionArgs
  = -- | a:
    --
    -- Note: there can be nested functions to produce
    -- a: b: c:
    FAOne Identifier
  | -- | { a, b, c }:
    FASet (NonEmpty Identifier)
  deriving stock (Data, Eq, Show)

-- | Writes out `FunctionArgs` as Nix code
writeFunctionArgs :: FunctionArgs -> Text
writeFunctionArgs = \case
  FAOne (Identifier i) -> i <> ": "
  FASet is -> "{ " <> T.intercalate ", " (unIdentifier <$> toList is) <> "}:\n"

-- | Parses an `EFunc`
parseFunction :: Parser (FunctionArgs, Expr)
parseFunction = label "function" $ try ((,) <$> parseFunctionArgs <*> parseE False)

-- | Parses `FunctionArgs`
parseFunctionArgs :: Parser FunctionArgs
parseFunctionArgs = space *> choice [parseFAOne, parseFASet]
  where
    parseFAOne :: Parser FunctionArgs
    parseFAOne =
      FAOne <$> try do
        i <- parseIdentifier
        void . lexeme $ char ':'
        pure i
    parseFASet :: Parser FunctionArgs
    parseFASet =
      FASet . Unsafe.fromJust . nonEmpty
        <$> between
          (lexeme (char '{'))
          (lexeme (symbol "}:"))
          ( do
              argsExceptLast <- many $ try do
                i <- parseIdentifier
                void . lexeme $ char ','
                pure i
              lastArg <- lexeme parseIdentifier
              pure $ argsExceptLast <> [lastArg]
          )

-- | Parses an `EGrouping`
parseGrouping :: Parser Expr
parseGrouping =
  label "grouping"
    . between (char '(') (char ')')
    $ space *> lexeme (parseE False)

-- | Parses the import keyword
parseImport :: Parser Expr
parseImport = EImport <$ (label "import" . try $ symbol "import")

-- | Parses an `ELetIn`
parseLetIn :: Parser (NonEmpty Binding, Expr)
parseLetIn = label "let-in expression" $ try do
  void . lexeme $ symbol "let"
  bindings <- Unsafe.fromJust . nonEmpty <$> some (lexeme $ parseBinding True)
  void . lexeme $ symbol "in"
  expr <- parseE False
  pure (bindings, expr)

-- | A representation of a Nix value binding
data Binding
  = -- | inherit a b c;
    BInherit (NonEmpty Identifier)
  | -- | inherit (a) b c;
    BInheritFrom Expr (NonEmpty Identifier)
  | -- | # this is a comment
    BLineComment Text
  | -- | helloStr = "hello";
    BNameValue Property Expr
  deriving stock (Data, Eq, Show)

infixr 4 |=

-- | Operator form of = in a binding (`BNameValue`)
(|=) :: Property -> Expr -> Binding
(|=) = BNameValue

-- | Writes out a `Binding` as Nix code
writeBinding :: CommentsPolicy -> Binding -> Text
writeBinding cp = \case
  BInherit xs -> "inherit " <> writeIdentifiers " " xs <> ";\n"
  BInheritFrom from xs -> "inherit (" <> writeExpr cp from <> ") " <> writeIdentifiers " " xs <> ";\n"
  BLineComment c -> case cp of
    ShowComments -> "# " <> c <> "\n"
    HideComments -> ""
  BNameValue p v -> writeProperty cp p <> " = " <> writeExpr cp v <> ";\n"
  where
    writeIdentifiers :: Text -> NonEmpty Identifier -> Text
    writeIdentifiers sep = T.concat . intersperse sep . fmap unIdentifier . toList

-- | Parses a `Binding`
parseBinding ::
  -- | Whether to require a newline after a comment line
  Bool ->
  Parser Binding
parseBinding requireNewLineAfterCommentLine =
  label "binding"
    . lexeme
    $ try space
      *> choice
        [ try parseBLineComment,
          try parseBInherit,
          try parseBInheritFrom,
          try parseBNameValue
        ]
  where
    inheritKeywordFollowedBy :: Parser a -> Parser a
    inheritKeywordFollowedBy = ((void . lexeme $ symbol "inherit") *>)
    parseIdentifierList :: Parser (NonEmpty Identifier)
    parseIdentifierList = Unsafe.fromJust . nonEmpty <$> some (lexeme parseIdentifier)
    parseBInherit :: Parser Binding
    parseBInherit = do
      b <- inheritKeywordFollowedBy (BInherit <$> parseIdentifierList)
      void . lexeme $ symbol ";"
      pure b
    parseBInheritFrom :: Parser Binding
    parseBInheritFrom = do
      b <-
        inheritKeywordFollowedBy
          ( BInheritFrom
              <$> lexeme (between "(" ")" $ parseE False)
              <*> parseIdentifierList
          )
      void $ symbol ";"
      pure b
    parseBLineComment :: Parser Binding
    parseBLineComment = do
      space *> char '#' *> space
      BLineComment . toText
        <$> manyTill
          L.charLiteral
          (if requireNewLineAfterCommentLine then void (char '\n') else void (char '\n') <|> eof)
    parseBNameValue :: Parser Binding
    parseBNameValue = do
      n <- lexeme $ parseProperty False
      void . lexeme $ symbol "="
      v <- lexeme $ parseE False
      void $ symbol ";"
      pure $ BNameValue n v

-- | Organises a list of bindings, merging where possible and sorting.
--
-- Performs no action if any bindings are comments, as these cannot be meaningfully sorted.
--
-- Note: it is the caller's responsibility to check that these bindings don't reference
-- each other as the reordering could make this fail. The unsafe prefix simply highlights
-- this responsibility.
unsafeSimplifyBindings :: NonEmpty Binding -> NonEmpty Binding
unsafeSimplifyBindings initBs =
  if any isCommentBinding initBs
    then initBs
    else case initBs of
      (_ :| []) -> initBs
      _ -> sortBindings $ mergeBindings initBs
  where
    isCommentBinding :: Binding -> Bool
    isCommentBinding (BLineComment _) = True
    isCommentBinding _ = False
    mergeBindings :: NonEmpty Binding -> NonEmpty Binding
    mergeBindings (b :| bs) = foldr integrateBinding (b :| []) bs
    integrateBinding :: Binding -> NonEmpty Binding -> NonEmpty Binding
    integrateBinding b acc = case find (sameBindingType b) acc of
      Just bindingToMergeWith -> mergeTwoBindings b bindingToMergeWith :| NE.filter (/= bindingToMergeWith) acc
      Nothing -> internallySortBinding b `NE.cons` acc
    sameBindingType :: Binding -> Binding -> Bool
    sameBindingType (BInherit _) (BInherit _) = True
    sameBindingType (BInheritFrom a _) (BInheritFrom b _) = a == b
    sameBindingType _ _ = False
    mergeTwoBindings :: Binding -> Binding -> Binding
    mergeTwoBindings (BInherit xs) (BInherit ys) = BInherit $ combineBindingIdents xs ys
    mergeTwoBindings (BInheritFrom a xs) (BInheritFrom b ys) | a == b = BInheritFrom a $ combineBindingIdents xs ys
    mergeTwoBindings a b = error $ "Two bindings " <> show (a, b) <> " cannot be merged; this is a bug in nix-bootstrap"
    internallySortBinding :: Binding -> Binding
    internallySortBinding (BInherit xs) = BInherit . fromList . sortNub $ toList xs
    internallySortBinding (BInheritFrom a xs) = BInheritFrom a . fromList . sortNub $ toList xs
    internallySortBinding b = b
    combineBindingIdents :: NonEmpty Identifier -> NonEmpty Identifier -> NonEmpty Identifier
    combineBindingIdents xs = fromList . sortNub . toList . (xs <>)
    sortBindings :: NonEmpty Binding -> NonEmpty Binding
    sortBindings = NE.sortBy compareBindings
      where
        compareBindings (BLineComment _) _ = error "Line comment bindings cannot be sorted; this occurring means there is a bug in nix-bootstrap"
        compareBindings _ (BLineComment _) = error "Line comment bindings cannot be sorted; this occurring means there is a bug in nix-bootstrap"
        compareBindings (BInherit xs) (BInherit ys) = compare xs ys
        compareBindings (BInherit _) _ = LT
        compareBindings _ (BInherit _) = GT
        compareBindings (BInheritFrom e1 _) (BInheritFrom e2 _) = compare (writeExprForTerminal e1) (writeExprForTerminal e2)
        compareBindings (BInheritFrom _ _) _ = LT
        compareBindings _ (BInheritFrom _ _) = GT
        compareBindings (BNameValue p1 _) (BNameValue p2 _) = compareProperties p1 p2
        compareProperties (PIdent a) (PIdent b) = compare a b
        compareProperties (PIdent _) _ = LT
        compareProperties _ (PIdent _) = GT
        compareProperties (PCons p1 _) (PCons p2 _) = compareProperties p1 p2
        compareProperties (PCons _ _) _ = LT
        compareProperties _ (PCons _ _) = GT
        compareProperties (PAntiquote e1) (PAntiquote e2) = compare (writeExprForTerminal e1) (writeExprForTerminal e2)

-- | A representation of a Nix identifier
newtype Identifier = Identifier {unIdentifier :: Text}
  deriving stock (Data, Eq, Ord, Show)

-- | Parses an `Identifier`
parseIdentifier :: Parser Identifier
parseIdentifier =
  label "identifier" . fmap Identifier $
    choice
      [ string "...",
        do
          firstChar <- letterChar <|> char '_'
          rest <- many $ alphaNumChar <|> char '_' <|> char '-'
          pure . toText $ firstChar : rest
      ]

-- | Parses a list of `Expr`s
parseList :: Parser [Expr]
parseList =
  label "list" . between (space *> char '[') (char ']')
    . many
    . lexeme
    $ (space *> parseE True)

-- | A representation of a Nix literal
data Literal
  = -- | Boolean literal
    LBool Bool
  | -- | Integer literal
    LInteger Int
  | -- | Multi-line string literal
    LMultilineString Text
  | -- | Null literal
    LNull
  | -- | Path literal
    LPath Text
  | -- | String literal
    LString Text
  deriving stock (Data, Eq, Show)

-- | Writes out a `Literal` as Nix code
writeLiteral :: Literal -> Text
writeLiteral = \case
  LBool b -> T.toLower (show b)
  LInteger i -> show i
  LMultilineString s -> "''" <> s <> "''"
  LNull -> "null"
  LPath p -> p
  LString s -> "\"" <> T.replace "\"" "\\\"" s <> "\""

-- | parses a `literal`
parseLiteral ::
  -- | If `True`, non-atomic (see `parseAtom`) literals will fail to parse
  Bool ->
  Parser Literal
parseLiteral atomic =
  label "literal" $
    choice
      [ try parseBooleanLiteral,
        try (label "null literal" (LNull <$ string "null")),
        try parsePathLiteral,
        try parseIntegerLiteral,
        try parseStringLiteral,
        parseMultilineStringLiteral
      ]
  where
    parseBooleanLiteral :: Parser Literal
    parseBooleanLiteral =
      label
        "boolean literal"
        (LBool <$> choice [True <$ string "true", False <$ string "false"])
    parseIntegerLiteral :: Parser Literal
    parseIntegerLiteral =
      label "integer literal"
        . fmap LInteger
        $ if atomic
          then L.decimal
          else option id (lexeme (negate <$ char '-')) <*> L.decimal
    parseMultilineStringLiteral :: Parser Literal
    parseMultilineStringLiteral =
      label "multiline string literal"
        . fmap (LMultilineString . T.concat . fmap mslpToText)
        $ char '\'' *> char '\''
          *> manyTill
            ( choice
                [ try parseMultilineStringLiteralEscape <?> "escape sequence",
                  try (MSLPChar <$> L.charLiteral) <?> "non-terminating string character"
                ]
            )
            (try parseMultilineStringLiteralEnd <?> "multiline string end")
      where
        parseMultilineStringLiteralEscape :: Parser MultilineStringLiteralPart
        parseMultilineStringLiteralEscape =
          (symbol "''''" $> MSLPEscapeQuotes)
            <|> (symbol "''$" $> MSLPEscapeDollar)
        parseMultilineStringLiteralEnd :: Parser MultilineStringLiteralPart
        parseMultilineStringLiteralEnd = do
          (char '\'' *> char '\'' *> notFollowedBy (char '$' <|> char '\'')) $> MSLPEnd
    parsePathLiteral :: Parser Literal
    parsePathLiteral = label "path literal" do
      let disallowedChars = ['\0', ';', '\'', '"', '(', ')', '[', ']', '{', '}', '\\', '=']
      p <- someTill L.charLiteral . lookAhead . choice $ (eof : space1 : (void . char <$> disallowedChars))
      if '/' `elem` p
        then
          if any (`elem` p) disallowedChars
            then failure (Just (Tokens . Unsafe.fromJust $ nonEmpty p)) (one . Label $ 'p' :| "ath literal")
            else
              if '$' `elem` takeWhile (/= '/') p
                then failure (Just (Tokens $ '$' :| "")) (one . Tokens $ '/' :| "")
                else pure . LPath $ toText p
        else failure Nothing (one . Tokens $ '/' :| "")
    parseStringLiteral :: Parser Literal
    parseStringLiteral =
      label "string literal"
        . fmap (LString . toText)
        $ char '"' *> manyTill L.charLiteral (char '"')

-- | A property of a set, used in `EPropertyAccess`
data Property
  = -- | a.b
    PIdent Identifier
  | -- | a.${b}
    PAntiquote Expr
  | -- | a.${b}.c
    PCons Property Property
  deriving stock (Data, Eq, Show)

-- | Parses an `EPropertyAccess`
parsePropertyAccess :: Parser (Expr, Property)
parsePropertyAccess = try ((,) <$> parseAtom False <*> parseProperty True)

-- | Parses a `Property`
parseProperty ::
  -- | Whether to require a dot to precede the first property
  Bool ->
  Parser Property
parseProperty requireDot = do
  p1 <- parseOneProperty requireDot
  recursivelyGetNextProperty p1
  where
    parseOneProperty :: Bool -> Parser Property
    parseOneProperty requireDot' = do
      (if requireDot' then void else void . optional) $ char '.'
      choice
        [ PIdent <$> parseIdentifier,
          PAntiquote
            <$> between
              (string "${")
              (char '}')
              (parseE False)
        ]
    recursivelyGetNextProperty :: Property -> Parser Property
    recursivelyGetNextProperty p = do
      nextP <- optional (try $ parseOneProperty True)
      case nextP of
        Just nextP' -> recursivelyGetNextProperty (p `PCons` nextP')
        Nothing -> pure p

-- | Writes out a `Property` as Nix code
writeProperty :: CommentsPolicy -> Property -> Text
writeProperty cp = \case
  PIdent (Identifier i) -> i
  PAntiquote e -> "${" <> writeExpr cp e <> "}"
  PCons p1 p2 -> writeProperty cp p1 <> "." <> writeProperty cp p2

-- | Parses an `ESet`
parseSet ::
  -- | Whether to allow recursive sets
  Bool ->
  Parser (Bool, [Binding])
parseSet allowRecursive = label "set" $ try do
  isRec <-
    if allowRecursive
      then isJust <$> optional (lexeme $ string "rec")
      else pure False
  fmap (isRec,) <$> between "{" "}" $ (space *> many (try $ parseBinding True))

-- | Parses an `EWith`
parseWith :: Parser (Expr, Expr)
parseWith = label "with expression" $ try do
  void . lexeme $ symbol "with"
  additionalScope <- lexeme $ parseE False
  void . lexeme $ symbol ";"
  expr <- parseE False
  pure (additionalScope, expr)

-- Parsing Helpers

-- | Skips any amount of whitespace
skipSpace :: Parser ()
skipSpace = L.space space1 empty empty

-- | Consumes all following whitespace after the given parser succeeds
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

-- | Handles parsing of literal text
symbol :: Text -> Parser ()
symbol s = string s *> notFollowedBy (alphaNumChar <|> char '-' <|> char '_')

data MultilineStringLiteralPart = MSLPChar Char | MSLPEscapeQuotes | MSLPEscapeDollar | MSLPEnd
  deriving stock (Eq, Show)

mslpToText :: MultilineStringLiteralPart -> Text
mslpToText = \case
  MSLPChar c -> one c
  MSLPEscapeQuotes -> "''''"
  MSLPEscapeDollar -> "''$"
  MSLPEnd -> ""

-- | Runs the given parser, prettily formatting its errors
mkEitherParser :: Parser a -> Text -> Either Text a
mkEitherParser p = first (toText . errorBundlePretty) . parse (between skipSpace eof p) ""

-- Validation

-- | Returns `Right ()` if all of the identifiers used are in scope,
-- or `Left` with a list of out-of-scope identifiers otherwise.
--
-- Since it's impossible to know the attributes of a set such as
-- nixpkgs without evaluating it, scope checking will always succeed
-- in with expressions and when using property access.
--
-- The use of Mostly in the name serves to remind the caller that this
-- is a coarse check which may give false positives, but all negatives
-- will be genuine errors.
isMostlyCorrectlyScoped :: Expr -> Either (NonEmpty Identifier) ()
isMostlyCorrectlyScoped = isCorrectlyScoped' []

-- | Like `isMostlyCorrectlyScoped` but with some known existing scope
isCorrectlyScoped' :: [Identifier] -> Expr -> Either (NonEmpty Identifier) ()
isCorrectlyScoped' scope = \case
  EApplication e1 e2 -> mergeScopeResults (isCorrectlyScoped' scope e1 :| [isCorrectlyScoped' scope e2])
  EFunc args e ->
    let additionalScope = case args of FAOne i -> [i]; FASet is -> toList is
     in isCorrectlyScoped' (scope <> additionalScope) e
  EGrouping e -> isCorrectlyScoped' scope e
  EIdent i
    | i `elem` scope -> pass
    | i == Identifier "builtins" -> pass
    | i == Identifier "fetchTarball" -> pass
    | otherwise -> Left (one i)
  EImport -> pass
  ELetIn (toList -> bindings) e ->
    mergeScopeResults
      (bindingsAreCorrectlyScoped scope bindings :| [isCorrectlyScoped' (scope <> scopeFrom bindings) e])
  EList [] -> pass
  EList (e1 : eRest) -> mergeScopeResults (isCorrectlyScoped' scope <$> e1 :| eRest)
  EListConcatOperator -> pass
  EPathConcatOperator -> pass
  ELit _ -> pass
  EPropertyAccess e p -> mergeScopeResults (isCorrectlyScoped' scope e :| [propertyIsCorrectlyScoped scope p])
  ESet isRec bindings ->
    bindingsAreCorrectlyScoped
      ((if isRec then scopeFrom bindings else []) <> scope)
      bindings
  EWith additionalScope _ -> isCorrectlyScoped' scope additionalScope

-- | Used by isCorrectlyScoped' to ensure bindings are handled properly and in order
--
-- When inheriting from another set, bindings will always claim to be in scope.
bindingsAreCorrectlyScoped :: [Identifier] -> [Binding] -> Either (NonEmpty Identifier) ()
bindingsAreCorrectlyScoped _ [] = pass
bindingsAreCorrectlyScoped scope (b : bs) =
  mergeScopeResults
    ( bindingIsCorrectlyScoped b
        :| [bindingsAreCorrectlyScoped (scope <> scopeFrom (one b)) bs]
    )
  where
    bindingIsCorrectlyScoped :: Binding -> Either (NonEmpty Identifier) ()
    bindingIsCorrectlyScoped = \case
      BInherit is ->
        mergeScopeResults
          ( foldr
              ( \i acc ->
                  ( if i `elem` scope || i == Identifier "builtins"
                      then Right ()
                      else Left (one i)
                  )
                    <| acc
              )
              (one $ Right ())
              is
          )
      -- ((\i -> if i `elem` scope || i == Identifier "builtins" then pass else ) <$> is)
      BInheritFrom e _ -> isCorrectlyScoped' scope e
      BLineComment _ -> pass
      BNameValue p v -> mergeScopeResults (propertyIsCorrectlyScoped scope p :| [isCorrectlyScoped' scope v])

propertyIsCorrectlyScoped :: [Identifier] -> Property -> Either (NonEmpty Identifier) ()
propertyIsCorrectlyScoped scope = \case
  PIdent _ -> pass
  PAntiquote e -> isCorrectlyScoped' scope e
  p1 `PCons` p2 -> mergeScopeResults (propertyIsCorrectlyScoped scope p1 :| [propertyIsCorrectlyScoped scope p2])

-- | Merges scope results, ensuring all failures are kept in a `Left` value, or
-- the `Right` value is kept if there are none.
mergeScopeResults :: NonEmpty (Either (NonEmpty Identifier) ()) -> Either (NonEmpty Identifier) ()
mergeScopeResults (x :| []) = x
mergeScopeResults (Right () :| x : xs) = mergeScopeResults (x :| xs)
mergeScopeResults (Left is :| Right () : xs) = mergeScopeResults (Left is :| xs)
mergeScopeResults (Left is :| Left is2 : xs) =
  mergeScopeResults
    ( Left
        (Unsafe.fromJust . nonEmpty . ordNub . toList $ is <> is2)
        :| xs
    )

-- | Gets a list of identifiers brought into scope by the given bindings
scopeFrom :: [Binding] -> [Identifier]
scopeFrom = foldr f []
  where
    f :: Binding -> [Identifier] -> [Identifier]
    f b acc =
      acc <> case b of
        BInherit is -> toList is
        BInheritFrom _ is -> toList is
        BLineComment _ -> []
        BNameValue p _ -> scopeFromProperty p
    scopeFromProperty :: Property -> [Identifier]
    scopeFromProperty = \case
      PIdent i -> [i]
      -- In theory this could bring in scope but it's impossible to calculate it without
      -- evaluation and it's unlikely to be the only part of the property or the first part
      -- of the first `PCons`.
      PAntiquote _ -> []
      p `PCons` _ -> scopeFromProperty p

-- TH

-- | QuasiQuoter for Nix `Expr`s
nix :: QuasiQuoter
nix = mkParseQuoter "Expr" parseExpr

-- | QuasiQuoter for Nix `FunctionArgs`
nixargs :: QuasiQuoter
nixargs = mkParseQuoter "FunctionArgs" (mkEitherParser parseFunctionArgs)

-- | QuasiQuoter for Nix `Binding`s
nixbinding :: QuasiQuoter
nixbinding = mkParseQuoter "Binding" (mkEitherParser $ parseBinding False)

-- | QuasiQuoter for Nix `Identifier`s
nixident :: QuasiQuoter
nixident = mkParseQuoter "Identifier" (mkEitherParser parseIdentifier)

-- | QuasiQuoter for Nix `Property` values
nixproperty :: QuasiQuoter
nixproperty = mkParseQuoter "Property" (mkEitherParser $ parseProperty False)

mkParseQuoter :: forall a. Data a => String -> (Text -> Either Text a) -> QuasiQuoter
mkParseQuoter typeName parserFunction =
  QuasiQuoter
    { quoteExp = liftDataWithText <=< parseOrThrow,
      quotePat = dataToPatQ (const Nothing) <=< parseOrThrow,
      quoteType = const . fail $ "No " <> typeName <> " parser for type",
      quoteDec = const . fail $ "No " <> typeName <> " parser for declarations"
    }
  where
    parseOrThrow :: String -> Q a
    parseOrThrow = either (fail . toString) pure . parserFunction . toText
    -- A required workaround because the stock `liftData` expects
    -- `Data.Text.pack` to be defined in "Data.Text.Internal` instead,
    -- thus failing at compile-time.
    liftDataWithText :: a -> Q TH.Exp
    liftDataWithText = dataToExpQ (fmap liftText . cast)
    liftText :: Text -> Q TH.Exp
    liftText txt = TH.AppE (TH.VarE 'T.pack) <$> THS.lift (toString txt)
