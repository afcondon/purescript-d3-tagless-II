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
  ( num, text
  , field, timesN, plusN
  , computed, computedStr, staticStr, from, fromStr
  , class NumExpr, class DatumExpr, EvalD
  )
import PSD3.Expr.Interpreter.CodeGen (CodeGen, runCodeGen)
import PSD3.Expr.Interpreter.Eval (runEvalD)

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

-- | Access the letter field
_letter :: forall repr. DatumExpr repr LetterRow => repr String
_letter = field @"letter"

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
letterY :: Number
letterY = 100.0

-- | Evaluate expression with datum
evalExpr :: forall a. EvalD LetterDatum a -> LetterDatum -> a
evalExpr expr datum = runEvalD expr datum 0

-- =============================================================================
-- GUP Tree with Full Enter/Update/Exit
-- =============================================================================

-- | Create the GUP tree for letters
-- |
-- | Notice how attributes are now more readable:
-- | - `computed "x" letterX` instead of `v3Attr "x" letterX`
-- | - `staticStr "fill" "steelblue"` instead of `v3AttrStr "fill" (str "...")`
-- | - `from "x" (\d -> ...)` instead of `v3AttrFn "x" (\d -> ...)`
createLettersTree :: Array LetterDatum -> Tree LetterDatum
createLettersTree letters =
  T.sceneJoin "letters" "text" letters
    -- Template: FINAL state for each letter
    (\d -> T.elem Text
      [ computed "x" (num (evalExpr letterX d))
      , computed "y" (num letterY)
      , computed "font-size" (num 32.0)
      , computedStr "text-anchor" (text "middle")
      , computedStr "dominant-baseline" (text "middle")
      , computedStr "fill" (text "#2c3e50")
      , computed "opacity" (num 1.0)
      , computedStr "textContent" (text d.letter)
      ])
    -- Behaviors for enter/update/exit
    { keyFn: Just _.letter
    , enterBehavior: Just
        { initialAttrs:
            [ computed "y" (num 20.0)
            , computed "opacity" (num 0.0)
            , computedStr "fill" (text "#27ae60")  -- Green for entering
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
            [ from "x" (\d' -> evalExpr letterX d')
            , computed "y" (num letterY)
            , computedStr "fill" (text "#2c3e50")  -- Dark for stable
            , computed "opacity" (num 1.0)
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
            [ computed "y" (num 180.0)
            , computed "opacity" (num 0.0)
            , computedStr "fill" (text "#e74c3c")  -- Red for exiting
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
    let svgTree :: Tree Unit
        svgTree =
          T.named SVG "svg"
            [ computed "width" (num 600.0)
            , computed "height" (num 200.0)
            , computedStr "viewBox" (text "0 0 600 200")
            , computedStr "id" (text "friendly-gup-svg")
            , computedStr "class" (text "friendly-gup-demo")
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

  let letters = Array.mapWithIndex
        (\i c -> { letter: SCU.singleton c, index: toNumber i })
        (SCU.toCharArray letterString)

  liftEffect $ Console.log $ "Updating to: " <> letterString

  let tree :: Tree LetterDatum
      tree = createLettersTree letters

  _ <- renderTree svg tree

  liftEffect $ Console.log "GUP cycle complete!"

-- | Initialize in a given container, return update function
initFriendlyGUP :: String -> Effect (Array Char -> Effect Unit)
initFriendlyGUP containerSelector = do
  let svgId = "friendly-gup-svg-" <> filterAlphaNum containerSelector
      svgSelector = "#" <> svgId

  runD3v2M do
    container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)
    let svgTree :: Tree Unit
        svgTree =
          T.named SVG "svg"
            [ computed "width" (num 800.0)
            , computed "height" (num 500.0)
            , computedStr "viewBox" (text "0 -50 800 500")
            , computedStr "id" (text svgId)
            , computedStr "class" (text "friendly-gup-demo d3svg gup")
            ]
            `T.withChild`
              T.elem Group []
    _ <- renderTree container svgTree
    pure unit

  pure \letters -> runD3v2M do
    svg <- select svgSelector :: _ (D3v2Selection_ SEmpty Element Unit)

    let letterData = Array.mapWithIndex
          (\i c -> { letter: SCU.singleton c, index: toNumber i })
          letters

    let tree :: Tree LetterDatum
        tree = createLettersTree letterData

    _ <- renderTree svg tree
    pure unit

-- | Filter string to alphanumeric characters
filterAlphaNum :: String -> String
filterAlphaNum s = SCU.fromCharArray $ Array.filter isAlphaNum (SCU.toCharArray s)
  where
    isAlphaNum c =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9')
