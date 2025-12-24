module EmmetParser.Lexer
  ( Token(..)
  , ElementChar(..)
  , JoinChar(..)
  , tokenize
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import EmmetParser.Types (ParseError(..))

-- | Tokens for Emmet expressions
data Token
  = TElement ElementChar        -- g, c, r, p, l, t
  | TJoin JoinChar             -- j, n, u, x
  | TLParen                    -- (
  | TRParen                    -- )
  | TLBracket                  -- [
  | TRBracket                  -- ]
  | TChild                     -- >
  | TSibling                   -- +
  | TMultiplier Int            -- *N
  | THash                      -- #
  | TEquals                    -- =
  | TColon                     -- :
  | TAt                        -- @
  | TComma                     -- ,
  | TIdentifier String         -- alphanumeric identifier
  | TNumber String             -- numeric literal

derive instance eqToken :: Eq Token

instance showToken :: Show Token where
  show = case _ of
    TElement c -> "TElement(" <> show c <> ")"
    TJoin c -> "TJoin(" <> show c <> ")"
    TLParen -> "("
    TRParen -> ")"
    TLBracket -> "["
    TRBracket -> "]"
    TChild -> ">"
    TSibling -> "+"
    TMultiplier n -> "*" <> show n
    THash -> "#"
    TEquals -> "="
    TColon -> ":"
    TAt -> "@"
    TComma -> ","
    TIdentifier s -> "Identifier(" <> s <> ")"
    TNumber s -> "Number(" <> s <> ")"

-- | Element characters
data ElementChar = G | C | R | P | L | T

derive instance eqElementChar :: Eq ElementChar

instance showElementChar :: Show ElementChar where
  show = case _ of
    G -> "g"
    C -> "c"
    R -> "r"
    P -> "p"
    L -> "l"
    T -> "t"

-- | Join characters
data JoinChar = J | N | U | X

derive instance eqJoinChar :: Eq JoinChar

instance showJoinChar :: Show JoinChar where
  show = case _ of
    J -> "j"
    N -> "n"
    U -> "u"
    X -> "x"

-- | Lexer state
type LexerState =
  { input :: String
  , pos :: Int
  , tokens :: Array Token
  , inBrackets :: Boolean  -- Track if we're inside attribute brackets
  }

-- | Tokenize an Emmet expression
tokenize :: String -> Either ParseError (Array Token)
tokenize input = go { input, pos: 0, tokens: [], inBrackets: false }
  where
    go :: LexerState -> Either ParseError (Array Token)
    go state
      | state.pos >= SCU.length state.input = Right state.tokens
      | otherwise = do
          case SCU.charAt state.pos state.input of
            Nothing -> Right state.tokens
            Just c -> do
              newState <- processChar c state
              go newState

    processChar :: Char -> LexerState -> Either ParseError LexerState
    processChar c state = case c of
      -- Skip whitespace
      ' ' -> Right $ state { pos = state.pos + 1 }
      '\t' -> Right $ state { pos = state.pos + 1 }
      '\n' -> Right $ state { pos = state.pos + 1 }
      '\r' -> Right $ state { pos = state.pos + 1 }

      -- Single character tokens
      '(' -> addToken TLParen state
      ')' -> addToken TRParen state
      '[' -> addTokenAndSetBrackets TLBracket true state
      ']' -> addTokenAndSetBrackets TRBracket false state
      '>' -> addToken TChild state
      '+' -> addToken TSibling state
      '#' -> addToken THash state
      '=' -> addToken TEquals state
      ':' -> addToken TColon state
      '@' -> addToken TAt state
      ',' -> addToken TComma state

      -- Multiplier *N
      '*' -> do
        let numberStart = state.pos + 1
        let numberStr = takeWhileFrom isDigit numberStart state.input
        if String.null numberStr
          then Left $ InvalidAttribute "Multiplier must be followed by a number" state.pos
          else case Int.fromString numberStr of
            Nothing -> Left $ InvalidAttribute "Invalid multiplier number" state.pos
            Just n ->
              Right $ state
                { tokens = Array.snoc state.tokens (TMultiplier n)
                , pos = state.pos + 1 + String.length numberStr
                }

      -- Element characters (only outside brackets)
      'g' | not state.inBrackets -> addToken (TElement G) state
      'c' | not state.inBrackets -> addToken (TElement C) state
      'r' | not state.inBrackets -> addToken (TElement R) state
      'p' | not state.inBrackets -> addToken (TElement P) state
      'l' | not state.inBrackets -> addToken (TElement L) state
      't' | not state.inBrackets -> addToken (TElement T) state

      -- Join characters (only outside brackets)
      'j' | not state.inBrackets -> addToken (TJoin J) state
      'n' | not state.inBrackets -> addToken (TJoin N) state
      'u' | not state.inBrackets -> addToken (TJoin U) state
      'x' | not state.inBrackets -> addToken (TJoin X) state

      -- Identifiers (alphanumeric starting with letter or digit)
      _ | isAlpha c || isDigit c -> do
          let identifier = takeWhileFrom isAlphaNum state.pos state.input
          -- Check for decimal numbers (e.g., 0.6, 123.45)
          if allDigits identifier
            then do
              let nextPos = state.pos + String.length identifier
              case SCU.charAt nextPos state.input of
                Just '.' -> do
                  -- Check if there are digits after the decimal point
                  let afterDecimal = takeWhileFrom isDigit (nextPos + 1) state.input
                  if String.null afterDecimal
                    then
                      -- No digits after '.', just return the integer part
                      Right $ state
                        { tokens = Array.snoc state.tokens (TNumber identifier)
                        , pos = nextPos
                        }
                    else
                      -- We have a decimal number
                      let fullNumber = identifier <> "." <> afterDecimal
                      in Right $ state
                        { tokens = Array.snoc state.tokens (TNumber fullNumber)
                        , pos = nextPos + 1 + String.length afterDecimal
                        }
                _ ->
                  -- No decimal point, just a regular number
                  Right $ state
                    { tokens = Array.snoc state.tokens (TNumber identifier)
                    , pos = nextPos
                    }
            else
              -- Not all digits, it's an identifier
              Right $ state
                { tokens = Array.snoc state.tokens (TIdentifier identifier)
                , pos = state.pos + String.length identifier
                }

      -- Unknown character
      _ -> Left $ UnexpectedChar c state.pos

    addToken :: Token -> LexerState -> Either ParseError LexerState
    addToken token state =
      Right $ state
        { tokens = Array.snoc state.tokens token
        , pos = state.pos + 1
        }

    addTokenAndSetBrackets :: Token -> Boolean -> LexerState -> Either ParseError LexerState
    addTokenAndSetBrackets token inBrackets state =
      Right $ state
        { tokens = Array.snoc state.tokens token
        , pos = state.pos + 1
        , inBrackets = inBrackets
        }

-- | Take characters while predicate holds, starting from position
takeWhileFrom :: (Char -> Boolean) -> Int -> String -> String
takeWhileFrom pred start str =
  let
    chars = SCU.toCharArray str
    taken = Array.drop start chars
      # Array.takeWhile pred
  in
    SCU.fromCharArray taken

-- | Check if character is alphabetic
isAlpha :: Char -> Boolean
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

-- | Check if character is a digit
isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

-- | Check if character is alphanumeric
isAlphaNum :: Char -> Boolean
isAlphaNum c = isAlpha c || isDigit c

-- | Check if all characters in string are digits
allDigits :: String -> Boolean
allDigits s = SCU.toCharArray s # Array.all isDigit
