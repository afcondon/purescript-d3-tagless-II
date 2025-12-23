module EmmetParser.Parser
  ( parse
  , parseEmmet
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import EmmetParser.Lexer (Token(..), ElementChar(..), JoinChar(..), tokenize)
import EmmetParser.Types (EmmetExpr(..), EmmetNode(..), ElementType(..), JoinType(..), Attribute(..), ParseError(..))

-- | Parser state
type ParserState =
  { tokens :: Array Token
  , pos :: Int
  }

-- | Parse an Emmet expression from a string
parseEmmet :: String -> Either ParseError EmmetExpr
parseEmmet input = do
  tokens <- tokenize input
  if Array.null tokens
    then Left EmptyExpression
    else parse tokens

-- | Parse tokens into an Emmet expression
parse :: Array Token -> Either ParseError EmmetExpr
parse tokens = do
  result <- parseExpression { tokens, pos: 0 }
  -- Check if we consumed all tokens
  if result.state.pos < Array.length tokens
    then Left $ UnexpectedChar '?' result.state.pos  -- TODO: improve error
    else Right result.expr

type ParseResult = { expr :: EmmetExpr, state :: ParserState }

-- | Parse: Expression ::= Term (('+' Term))*
parseExpression :: ParserState -> Either ParseError ParseResult
parseExpression state = do
  firstTerm <- parseTerm state
  parseRestSiblings firstTerm

  where
    parseRestSiblings :: ParseResult -> Either ParseError ParseResult
    parseRestSiblings result = do
      case peekToken result.state of
        Just TSibling -> do
          let nextState = advance result.state
          nextTerm <- parseTerm nextState
          -- Combine with Sibling
          let combined = { expr: Sibling result.expr nextTerm.expr, state: nextTerm.state }
          parseRestSiblings combined
        _ -> Right result

-- | Parse: Term ::= Factor (('>' Factor))*
parseTerm :: ParserState -> Either ParseError ParseResult
parseTerm state = do
  firstFactor <- parseFactor state
  parseRestChildren firstFactor

  where
    parseRestChildren :: ParseResult -> Either ParseError ParseResult
    parseRestChildren result = do
      case peekToken result.state of
        Just TChild -> do
          let nextState = advance result.state
          nextFactor <- parseFactor nextState
          -- Add child to current node
          let combined = addChild result.expr nextFactor.expr
          parseRestChildren { expr: combined, state: nextFactor.state }
        _ -> Right result

    addChild :: EmmetExpr -> EmmetExpr -> EmmetExpr
    addChild parent child = case parent of
      Single node children ->
        Single node (Array.snoc children child)
      _ ->
        -- Parent should always be Single at this point
        parent

-- | Parse: Factor ::= (Element | Join) Attributes? Name? Multiplier? | '(' Expression ')' Multiplier?
parseFactor :: ParserState -> Either ParseError ParseResult
parseFactor state = case peekToken state of
  Just TLParen -> parseGrouped state
  Just (TElement elemChar) -> parseElement elemChar state
  Just (TJoin joinChar) -> parseJoin joinChar state
  Just token -> Left $ UnexpectedChar '?' state.pos  -- TODO: better error
  Nothing -> Left $ UnexpectedEnd "element or join"

-- | Parse grouped expression: '(' Expression ')' Multiplier?
parseGrouped :: ParserState -> Either ParseError ParseResult
parseGrouped state = do
  -- Consume '('
  let state1 = advance state
  -- Parse inner expression
  result <- parseExpression state1
  -- Expect ')'
  case peekToken result.state of
    Just TRParen -> do
      let state2 = advance result.state
      -- Check for multiplier
      case peekToken state2 of
        Just (TMultiplier n) ->
          Right { expr: Repeat result.expr n, state: advance state2 }
        _ ->
          Right { expr: result.expr, state: state2 }
    _ ->
      Left $ UnbalancedParens state.pos

-- | Parse element: Element Attributes? Name? Multiplier?
parseElement :: ElementChar -> ParserState -> Either ParseError ParseResult
parseElement elemChar state = do
  let elemType = charToElementType elemChar
  let state1 = advance state  -- consume element char

  -- Parse optional attributes
  attrsResult <- parseOptionalAttributes state1

  -- Parse optional name
  nameResult <- parseOptionalName attrsResult.state

  -- Create node
  let node = ElemNode elemType attrsResult.attrs nameResult.name
  let expr = Single node []

  -- Parse optional multiplier
  case peekToken nameResult.state of
    Just (TMultiplier n) ->
      Right { expr: Repeat expr n, state: advance nameResult.state }
    _ ->
      Right { expr, state: nameResult.state }

-- | Parse join: Join '(' TypeName ')' Attributes? Name? Multiplier?
parseJoin :: JoinChar -> ParserState -> Either ParseError ParseResult
parseJoin joinChar state = do
  let joinType = charToJoinType joinChar
  let state1 = advance state  -- consume join char

  -- Expect '('
  case peekToken state1 of
    Just TLParen -> do
      let state2 = advance state1

      -- Parse type name
      case peekToken state2 of
        Just (TIdentifier typeName) -> do
          let state3 = advance state2

          -- Expect ')'
          case peekToken state3 of
            Just TRParen -> do
              let state4 = advance state3

              -- Parse optional attributes
              attrsResult <- parseOptionalAttributes state4

              -- Parse optional name
              nameResult <- parseOptionalName attrsResult.state

              -- Create node
              let node = JoinNode joinType typeName attrsResult.attrs nameResult.name
              let expr = Single node []

              -- Parse optional multiplier
              case peekToken nameResult.state of
                Just (TMultiplier n) ->
                  Right { expr: Repeat expr n, state: advance nameResult.state }
                _ ->
                  Right { expr, state: nameResult.state }

            _ -> Left $ UnbalancedParens state1.pos

        _ -> Left $ InvalidTypeName "" state2.pos

    _ -> Left $ UnexpectedChar '(' state.pos

-- | Parse optional attributes: '[' AttrList ']'?
parseOptionalAttributes :: ParserState -> Either ParseError { attrs :: Array Attribute, state :: ParserState }
parseOptionalAttributes state = case peekToken state of
  Just TLBracket -> do
    let state1 = advance state
    parseAttributeList state1 []
  _ -> Right { attrs: [], state }

  where
    parseAttributeList :: ParserState -> Array Attribute -> Either ParseError { attrs :: Array Attribute, state :: ParserState }
    parseAttributeList st acc = do
      attrResult <- parseAttribute st
      let newAcc = Array.snoc acc attrResult.attr

      case peekToken attrResult.state of
        Just TComma -> do
          -- More attributes
          let nextState = advance attrResult.state
          parseAttributeList nextState newAcc

        Just TRBracket -> do
          -- End of attributes
          Right { attrs: newAcc, state: advance attrResult.state }

        _ -> Left $ InvalidAttribute "Expected ',' or ']'" attrResult.state.pos

-- | Parse a single attribute: StaticAttr | FieldAttr | IndexAttr
parseAttribute :: ParserState -> Either ParseError { attr :: Attribute, state :: ParserState }
parseAttribute state = do
  -- Expect identifier (attribute name)
  case peekToken state of
    Just (TIdentifier attrName) -> do
      let state1 = advance state

      -- Check what follows
      case peekToken state1 of
        Just TEquals -> do
          -- StaticAttr: name=value
          let state2 = advance state1
          case peekToken state2 of
            Just (TIdentifier value) ->
              Right { attr: StaticAttr attrName value, state: advance state2 }
            Just (TNumber value) ->
              Right { attr: StaticAttr attrName value, state: advance state2 }
            _ -> Left $ InvalidAttribute "Expected value after '='" state2.pos

        Just TColon -> do
          -- FieldAttr: name:field
          let state2 = advance state1
          case peekToken state2 of
            Just (TIdentifier fieldName) ->
              Right { attr: FieldAttr attrName fieldName, state: advance state2 }
            _ -> Left $ InvalidAttribute "Expected field name after ':'" state2.pos

        Just TAt -> do
          -- IndexAttr: name@index
          let state2 = advance state1
          case peekToken state2 of
            Just (TIdentifier "index") ->
              Right { attr: IndexAttr attrName, state: advance state2 }
            _ ->
              -- Allow just @ without "index" keyword
              Right { attr: IndexAttr attrName, state: state2 }

        _ -> Left $ InvalidAttribute "Expected '=', ':', or '@' after attribute name" state1.pos

    _ -> Left $ InvalidAttribute "Expected attribute name" state.pos

-- | Parse optional name: '#' Identifier?
parseOptionalName :: ParserState -> Either ParseError { name :: Maybe String, state :: ParserState }
parseOptionalName state = case peekToken state of
  Just THash -> do
    let state1 = advance state
    case peekToken state1 of
      Just (TIdentifier name) ->
        Right { name: Just name, state: advance state1 }
      _ -> Left $ InvalidAttribute "Expected identifier after '#'" state1.pos
  _ -> Right { name: Nothing, state }

-- | Peek at current token without consuming
peekToken :: ParserState -> Maybe Token
peekToken state = Array.index state.tokens state.pos

-- | Advance parser position by 1
advance :: ParserState -> ParserState
advance state = state { pos = state.pos + 1 }

-- | Convert element character to element type
charToElementType :: ElementChar -> ElementType
charToElementType = case _ of
  G -> EGroup
  C -> ECircle
  R -> ERect
  P -> EPath
  L -> ELine
  T -> EText

-- | Convert join character to join type
charToJoinType :: JoinChar -> JoinType
charToJoinType = case _ of
  J -> SimpleJoin
  N -> NestedJoin
  U -> UpdateJoin
  X -> UpdateNestedJoin
