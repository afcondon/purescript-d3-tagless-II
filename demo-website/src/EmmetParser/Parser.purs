module EmmetParser.Parser
  ( parse
  , parseEmmet
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List, many, some, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits as SCU
import Data.Enum (fromEnum)
import EmmetParser.Types (EmmetExpr(..), EmmetNode(..), ElementType(..), JoinType(..), Attribute(..), ParseError(..))
import Parsing (Parser, runParser, fail)
import Parsing (ParseError) as P
import Parsing.Combinators (between, sepBy1, optionMaybe, try)
import Parsing.String (char, satisfy, string, eof)
import Parsing.String.Basic (digit)

-- | Parse an Emmet expression from a string
parseEmmet :: String -> Either ParseError EmmetExpr
parseEmmet input =
  case runParser input emmetParser of
    Left err -> Left $ InvalidAttribute (show err) 0  -- Map parsing errors
    Right expr -> Right expr

-- | Alias for parseEmmet (legacy compatibility)
parse :: Array _ -> Either ParseError EmmetExpr
parse _ = Left $ InvalidAttribute "parse from tokens deprecated - use parseEmmet" 0

-- =============================================================================
-- Parser Combinators
-- =============================================================================

type P a = Parser String a

-- | Parse one of the given characters
oneOf :: Array Char -> P Char
oneOf chars = satisfy (\c -> Array.elem c chars)

-- | Skip whitespace (spaces, tabs, newlines, carriage returns)
skipSpaces :: P Unit
skipSpaces = void $ many $ satisfy (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')

emmetParser :: P EmmetExpr
emmetParser = do
  expr <- expression
  eof
  pure expr

-- | Expression ::= Term ('+' Term)*
expression :: P EmmetExpr
expression = do
  skipSpaces
  first <- term
  rest <- many (skipSpaces *> char '+' *> skipSpaces *> term)
  pure $ List.foldl (\acc t -> Sibling acc t) first rest

-- | Term ::= Factor ('>' Term)?  (right-associative)
term :: P EmmetExpr
term = do
  skipSpaces
  fact <- factor
  mChild <- optionMaybe (skipSpaces *> char '>' *> term)
  case mChild of
    Nothing -> pure fact
    Just child -> pure $ addChild fact child
  where
    addChild :: EmmetExpr -> EmmetExpr -> EmmetExpr
    addChild parent child = case parent of
      Single node children -> Single node (Array.snoc children child)
      _ -> parent  -- Shouldn't happen

-- | Factor ::= (Element | Join) Attributes? Multiplier? | '(' Expression ')' Multiplier?
factor :: P EmmetExpr
factor =
  try groupedExpr <|> try joinNode <|> elementNode
  where
    groupedExpr = do
      _ <- char '('
      expr <- expression
      _ <- char ')'
      mult <- optionMaybe multiplier
      case mult of
        Nothing -> pure expr
        Just n -> pure $ Repeat expr n

-- | Element node: ElementChar Attributes? Multiplier?
elementNode :: P EmmetExpr
elementNode = do
  elemChar <- oneOf ['g', 'c', 'r', 'p', 'l', 't']
  attrs <- optionMaybe attributes
  mult <- optionMaybe multiplier
  let elemType = charToElementType elemChar
  let node = ElemNode elemType (Array.fromFoldable $ fromMaybe [] attrs) Nothing
  let expr = Single node []
  case mult of
    Nothing -> pure expr
    Just n -> pure $ Repeat expr n

-- | Join node: JoinChar '(' Identifier ')' Attributes? Multiplier?
joinNode :: P EmmetExpr
joinNode = do
  joinChar <- oneOf ['j', 'n', 'u', 'x']
  typeName <- between (char '(') (char ')') identifier
  attrs <- optionMaybe attributes
  mult <- optionMaybe multiplier
  let joinType = charToJoinType joinChar
  let node = JoinNode joinType typeName (Array.fromFoldable $ fromMaybe [] attrs) Nothing
  let expr = Single node []
  case mult of
    Nothing -> pure expr
    Just n -> pure $ Repeat expr n

-- | Attributes ::= '[' Attribute (',' Attribute)* ']'
attributes :: P (Array Attribute)
attributes = between (char '[') (char ']') (Array.fromFoldable <$> sepBy1 attribute (skipSpaces *> char ',' <* skipSpaces))

-- | Attribute ::= Name '=' Value | Name ':' Field | Name '@' 'index'
attribute :: P Attribute
attribute = try staticAttr <|> try fieldAttr <|> indexAttr
  where
    staticAttr = do
      name <- attrName
      _ <- char '='
      value <- attrValue
      pure $ StaticAttr name value

    fieldAttr = do
      name <- attrName
      _ <- char ':'
      field <- identifier
      -- Check if field is an opaque token (COMPUTED, SCALE, CONDITIONAL)
      if isOpaqueToken field
        then pure $ OpaqueAttr name field
        else pure $ FieldAttr name field

    indexAttr = do
      name <- attrName
      _ <- char '@'
      _ <- string "index"
      pure $ IndexAttr name

-- | Attribute name (alphanumeric, can include hyphens)
attrName :: P String
attrName = SCU.fromCharArray <$> Array.fromFoldable <$> some (satisfy isAttrNameChar)
  where
    isAttrNameChar c = isAlphaNum c || c == '-'

-- | Attribute value (alphanumeric, can include dots for decimals)
attrValue :: P String
attrValue = SCU.fromCharArray <$> Array.fromFoldable <$> some (satisfy isValueChar)
  where
    isValueChar c = isAlphaNum c || c == '.' || c == '-'

-- | Identifier (alphanumeric starting with letter)
identifier :: P String
identifier = do
  first <- satisfy isAlpha
  rest <- many (satisfy isAlphaNum)
  pure $ SCU.fromCharArray ([first] <> Array.fromFoldable rest)

-- | Check if a field name is an opaque token
isOpaqueToken :: String -> Boolean
isOpaqueToken = case _ of
  "COMPUTED" -> true
  "SCALE" -> true
  "CONDITIONAL" -> true
  _ -> false

-- | Multiplier ::= '*' Number
multiplier :: P Int
multiplier = do
  _ <- char '*'
  digits <- some digit
  let numStr = SCU.fromCharArray (Array.fromFoldable digits)
  case parseInt numStr of
    Nothing -> fail "Invalid multiplier number"
    Just n -> pure n

-- =============================================================================
-- Helper Functions
-- =============================================================================

charToElementType :: Char -> ElementType
charToElementType = case _ of
  'g' -> EGroup
  'c' -> ECircle
  'r' -> ERect
  'p' -> EPath
  'l' -> ELine
  't' -> EText
  _ -> EGroup  -- Shouldn't happen

charToJoinType :: Char -> JoinType
charToJoinType = case _ of
  'j' -> SimpleJoin
  'n' -> NestedJoin
  'u' -> UpdateJoin
  'x' -> UpdateNestedJoin
  _ -> SimpleJoin  -- Shouldn't happen

isAlpha :: Char -> Boolean
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isAlphaNum :: Char -> Boolean
isAlphaNum c = isAlpha c || (c >= '0' && c <= '9')

parseInt :: String -> Maybe Int
parseInt s = case List.fromFoldable (SCU.toCharArray s) of
  List.Nil -> Nothing
  digits -> go 0 digits
  where
    go acc List.Nil = Just acc
    go acc (d : ds) =
      case charToDigit d of
        Nothing -> Nothing
        Just n -> go (acc * 10 + n) ds

    charToDigit c =
      if c >= '0' && c <= '9'
        then Just (fromEnum c - fromEnum '0')
        else Nothing
