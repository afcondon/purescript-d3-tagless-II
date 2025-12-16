-- | General Update Pattern Demo (Friendly API)
-- |
-- | The classic D3 "letters" example using the friendlier DSL.
-- | Compare this file with V3GUPDemo.purs to see the API improvements.
-- |
-- | Data changes trigger the full GUP cycle:
-- | - ENTER: New letters fade in from above
-- | - UPDATE: Existing letters slide to new positions
-- | - EXIT: Removed letters fade out and drop
module D3.Viz.TreeAPI.V3GUPDemoFriendly
  ( friendlyGUPDemo
  , updateWithLetters
  , initFriendlyGUP
  , LetterDatum
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console

-- Core infrastructure
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.Internal.Transition.Types (Easing(..), transitionWith)
import PSD3.AST (Tree)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- Friendly DSL - notice how much cleaner the imports are!
import PSD3.Expr.Friendly
  ( num
  , text
  , field
  , timesN
  , plusN
  , eval
  , viewBox
  , width
  , height
  , x
  , y
  , fill
  , opacity
  , fontSize
  , textAnchor
  , dominantBaseline
  , textContent
  , attr -- for "id", "class" which don't have sugar
  , class NumExpr
  , class DatumExpr
  , EvalD
  -- Colors and units
  , hex, pt
  )
import PSD3.Expr.Interpreter.CodeGen (CodeGen, runCodeGen)

-- =============================================================================
-- Data Type
-- =============================================================================

-- | Each letter has an index (for positioning) and the character
type LetterDatum =
  { letter :: String
  , index :: Number
  }

type LetterRow = (letter :: String, index :: Number)

-- =============================================================================
-- Field Accessors (define once per data type)
-- =============================================================================

-- | Access the index field
_index :: forall repr. DatumExpr repr LetterRow => repr Number
_index = field @"index"

-- =============================================================================
-- Expressions
-- =============================================================================

-- | X position: index * 40 + 50
-- |
-- | Compare to old API:
-- |   letterX = field (Proxy :: Proxy "index") *: 40.0 +: 50.0
letterX :: forall repr. NumExpr repr => DatumExpr repr LetterRow => repr Number
letterX = _index `timesN` 40.0 `plusN` 50.0

-- | Y position (constant)
-- letterY :: Number
letterY :: forall repr. NumExpr repr => repr Number
letterY = num 100.0

-- =============================================================================
-- GUP Tree with Full Enter/Update/Exit
-- =============================================================================

-- | Create the GUP tree for letters
-- |
-- | Notice how the new `attr` function handles both numbers and strings!
-- | No more choosing between `computed` and `computedStr`.
createLettersTree :: Array LetterDatum -> Tree LetterDatum
createLettersTree letters =
  T.sceneJoin "letters" "text" letters
    -- Template: FINAL state for each letter
    ( \d -> T.elem Text
        [ x letterX
        , y letterY
        , fontSize $ pt 32.0         -- CSS units!
        , textAnchor $ text "middle"
        , dominantBaseline $ text "middle"
        , fill $ hex "#2c3e50"       -- Type-safe colors!
        , opacity $ num 1.0
        , textContent $ text d.letter
        ]
    )
    -- Behaviors for enter/update/exit
    { keyFn: Just _.letter
    , enterBehavior: Just
        { initialAttrs:
            [ y $ num 20.0
            , opacity $ num 0.0
            , fill $ hex "#27ae60"   -- Green for entering
            ]
        , transition: Just $ transitionWith
            { duration: Milliseconds 750.0
            , delay: Nothing
            , staggerDelay: Just 50.0
            , easing: Just BounceOut
            }
        }
    , updateBehavior: Just
        { attrs:
            [ x letterX              -- Use expression directly!
            , y letterY
            , fill $ hex "#2c3e50"   -- Dark for stable
            , opacity $ num 1.0
            ]
        , transition: Just $ transitionWith
            { duration: Milliseconds 500.0
            , delay: Nothing
            , staggerDelay: Just 30.0
            , easing: Just CubicInOut
            }
        }
    , exitBehavior: Just
        { attrs:
            [ y $ num 180.0
            , opacity $ num 0.0
            , fill $ hex "#e74c3c"   -- Red for exiting
            ]
        , transition: Just $ transitionWith
            { duration: Milliseconds 500.0
            , delay: Nothing
            , staggerDelay: Just 40.0
            , easing: Just CubicIn
            }
        }
    }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Initialize the GUP demo
friendlyGUPDemo :: Effect Unit
friendlyGUPDemo = do
  Console.log "=== Friendly API General Update Pattern Demo ==="
  Console.log ""
  Console.log "Expression for X position:"
  Console.log $ "  letterX = _index `timesN` 40.0 `plusN` 50.0"
  Console.log $ "  Generated: " <> runCodeGen (letterX :: CodeGen Number)
  Console.log ""
  Console.log "Starting with: A B C D E F G"
  Console.log ""

  -- Create the SVG container
  runD3v2M do
    container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)
    let
      svgTree :: Tree Unit
      svgTree =
        T.named SVG "svg"
          [ width $ num 600.0
          , height $ num 200.0
          , viewBox 0.0 0.0 600.0 200.0
          , attr "id" $ text "friendly-gup-svg"
          , attr "class" $ text "friendly-gup-demo"
          ]
          `T.withChild`
            T.elem Group []
    _ <- renderTree container svgTree
    pure unit

  updateWithLetters "ABCDEFG"

-- | Update the visualization with new letters
updateWithLetters :: String -> Effect Unit
updateWithLetters letterString = runD3v2M do
  svg <- select "#friendly-gup-svg" :: _ (D3v2Selection_ SEmpty Element Unit)

  let
    letters = Array.mapWithIndex
      (\i c -> { letter: SCU.singleton c, index: toNumber i })
      (SCU.toCharArray letterString)

  liftEffect $ Console.log $ "Updating to: " <> letterString

  let
    tree :: Tree LetterDatum
    tree = createLettersTree letters

  _ <- renderTree svg tree

  liftEffect $ Console.log "GUP cycle complete!"

-- | Initialize in a given container, return update function
initFriendlyGUP :: String -> Effect (Array Char -> Effect Unit)
initFriendlyGUP containerSelector = do
  let
    svgId = "friendly-gup-svg-" <> filterAlphaNum containerSelector
    svgSelector = "#" <> svgId

  runD3v2M do
    container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)
    let
      svgTree :: Tree Unit
      svgTree =
        T.named SVG "svg"
          [ width $ num 800.0
          , height $ num 500.0
          , viewBox 0.0 (-50.0) 800.0 500.0
          , attr "id" $ text svgId
          , attr "class" $ text "friendly-gup-demo d3svg gup"
          ]
          `T.withChild`
            T.elem Group []
    _ <- renderTree container svgTree
    pure unit

  pure \letters -> runD3v2M do
    svg <- select svgSelector :: _ (D3v2Selection_ SEmpty Element Unit)

    let
      letterData = Array.mapWithIndex
        (\i c -> { letter: SCU.singleton c, index: toNumber i })
        letters

    let
      tree :: Tree LetterDatum
      tree = createLettersTree letterData

    _ <- renderTree svg tree
    pure unit

-- | Filter string to alphanumeric characters
filterAlphaNum :: String -> String
filterAlphaNum s = SCU.fromCharArray $ Array.filter isAlphaNum (SCU.toCharArray s)
  where
  isAlphaNum c =
    (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      ||
        (c >= '0' && c <= '9')
