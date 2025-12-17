-- | Expression DSL General Update Pattern Demo
-- |
-- | The classic D3 "letters" example reimagined with:
-- | - v3 polymorphic expressions for computed attributes
-- | - updateJoin with declarative enter/update/exit behaviors
-- | - Staggered transitions for visual appeal
-- |
-- | Data changes trigger the full GUP cycle:
-- | - ENTER: New letters fade in from above
-- | - UPDATE: Existing letters slide to new positions
-- | - EXIT: Removed letters fade out and drop
module D3.Viz.TreeAPI.V3GUPDemo
  ( v3GUPDemo
  , updateWithLetters
  , initV3GUP
  , LetterDatum
  ) where

import Prelude hiding (add)

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Type.Proxy (Proxy(..))

-- v2 infrastructure
import PSD3.Internal.Attribute (Attribute)
import PSD3.Expr.Friendly (num, text, attr, from, viewBox, width, height, x, y, fill, opacity, fontSize, textAnchor, dominantBaseline, textContent)
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.Internal.Transition.Types (Easing(..), transitionWith)
import PSD3.AST (Tree)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- v3 DSL
import PSD3.Expr.Expr (class NumExpr)
import PSD3.Expr.Datum (class DatumExpr, field)
import PSD3.Expr.Sugar ((*:), (+:))
import PSD3.Expr.Interpreter.CodeGen (CodeGen, runCodeGen)
import PSD3.Expr.Interpreter.Eval (EvalD, runEvalD)

-- =============================================================================
-- Data Type
-- =============================================================================

-- | Each letter has an index (for positioning) and the character
type LetterDatum =
  { letter :: String
  , index :: Number  -- Number for v3 expression compatibility
  }

type LetterRow = (letter :: String, index :: Number)

-- =============================================================================
-- v3 Expressions (Polymorphic!)
-- =============================================================================

-- | X position based on index: index * 40 + 50
letterX :: forall repr. NumExpr repr => DatumExpr repr LetterRow => repr Number
letterX = indexField *: 40.0 +: 50.0
  where
    indexField = field (Proxy :: Proxy "index")

-- | Y position (constant for update state)
letterY :: Number
letterY = 100.0

-- =============================================================================
-- v3 -> v2 Integration
-- =============================================================================

-- | Evaluate v3 expression with datum
evalExpr :: forall a. EvalD LetterDatum a -> LetterDatum -> a
evalExpr expr datum = runEvalD expr datum 0

-- =============================================================================
-- GUP Tree with Full Enter/Update/Exit
-- =============================================================================

-- | Create the GUP tree for letters
-- |
-- | ENTER: Letters start above (y=20), invisible, then fade in and drop to y=100
-- | UPDATE: Letters slide horizontally to new positions
-- | EXIT: Letters fade out and drop below (y=180)
createLettersTree :: Array LetterDatum -> Tree LetterDatum
createLettersTree letters =
  T.updateJoin "letters" "text" letters
    -- Template: FINAL state for each letter (where they end up after enter/update)
    (\d -> T.elem Text
      [ x $ num (evalExpr letterX d)   -- v3: computed X position
      , y $ num letterY                -- Final Y position
      , fontSize $ num 32.0
      , textAnchor $ text "middle"
      , dominantBaseline $ text "middle"
      , fill $ text "#2c3e50"
      , opacity $ num 1.0
      -- The text content is set via a special TextContent attribute
      , textContent $ text d.letter
      ])
    -- Behaviors for enter/update/exit
    { keyFn: Just _.letter       -- Identity by letter, not by index!
    , enterBehavior: Just
        { initialAttrs:
            [ y $ num 20.0              -- Start above
            , opacity $ num 0.0         -- Start invisible
            , fill $ text "#27ae60"      -- Green for entering
            ]
        , transition: Just $ transitionWith
            { duration: Milliseconds 750.0
            , delay: Nothing
            , staggerDelay: Just 50.0  -- Stagger enter
            , easing: Just BounceOut   -- Bouncy entrance!
            }
        }
    , updateBehavior: Just
        { attrs:
            -- Update attributes (slide to new position)
            [ from "x" (\d' -> evalExpr letterX d')
            , y $ num letterY
            , fill $ text "#2c3e50"  -- Dark for stable
            , opacity $ num 1.0
            ]
        , transition: Just $ transitionWith
            { duration: Milliseconds 500.0
            , delay: Nothing
            , staggerDelay: Just 30.0  -- Stagger update
            , easing: Just CubicInOut
            }
        }
    , exitBehavior: Just
        { attrs:
            [ y $ num 180.0             -- Drop below
            , opacity $ num 0.0         -- Fade out
            , fill $ text "#e74c3c"      -- Red for exiting
            ]
        , transition: Just $ transitionWith
            { duration: Milliseconds 500.0
            , delay: Nothing
            , staggerDelay: Just 40.0  -- Stagger exit
            , easing: Just CubicIn
            }
        }
    }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Initialize the GUP demo - creates SVG container and renders initial letters
v3GUPDemo :: Effect Unit
v3GUPDemo = do
  Console.log "=== Expression DSL General Update Pattern Demo ==="
  Console.log ""
  Console.log "v3 Expression for X position:"
  Console.log $ "  letterX = " <> runCodeGen (letterX :: CodeGen Number)
  Console.log ""
  Console.log "Starting with: A B C D E F G"
  Console.log ""

  -- Create the SVG container first
  runD3v2M do
    container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)
    let svgTree :: Tree Unit
        svgTree =
          T.named SVG "svg"
            [ width $ num 600.0
            , height $ num 200.0
            , viewBox 0.0 0.0 600.0 200.0
            , attr "id" $ text "v3-gup-svg"
            , attr "class" $ text "v3-gup-demo"
            ]
            `T.withChild`
              T.elem Group []  -- Empty group placeholder
    _ <- renderTree container svgTree
    pure unit

  -- Initial render with ABCDEFG
  updateWithLetters "ABCDEFG"

-- | Update the visualization with a new set of letters
-- |
-- | This triggers the full GUP cycle:
-- | - Letters not in old set: ENTER (fade in from above)
-- | - Letters in both sets: UPDATE (slide to new position)
-- | - Letters not in new set: EXIT (fade out and drop)
updateWithLetters :: String -> Effect Unit
updateWithLetters letterString = runD3v2M do
  -- Select into the existing SVG (not #viz)
  svg <- select "#v3-gup-svg" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Convert string to array of LetterDatum with indices
  let letters = Array.mapWithIndex
        (\i c -> { letter: SCU.singleton c, index: toNumber i })
        (SCU.toCharArray letterString)

  liftEffect $ Console.log $ "Updating to: " <> letterString <> " (" <> show (Array.length letters) <> " letters)"

  -- Just the letters tree - renders INTO the existing SVG
  let tree :: Tree LetterDatum
      tree = createLettersTree letters

  -- Render - updateJoin handles enter/update/exit automatically!
  _ <- renderTree svg tree

  liftEffect do
    Console.log "GUP cycle complete!"
    Console.log "  - New letters entered (green, bounced in from above)"
    Console.log "  - Existing letters updated (slid to new positions)"
    Console.log "  - Removed letters exited (red, faded and dropped)"

-- | Initialize the V3 GUP demo in a given container
-- | Returns an update function that accepts an array of characters
-- | Compatible with the GUPv2 API for drop-in replacement
initV3GUP :: String -> Effect (Array Char -> Effect Unit)
initV3GUP containerSelector = do
  -- Create unique SVG ID based on selector
  let svgId = "v3-gup-svg-" <> filterAlphaNum containerSelector
      svgSelector = "#" <> svgId

  -- Create the SVG container
  runD3v2M do
    container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)
    let svgTree :: Tree Unit
        svgTree =
          T.named SVG "svg"
            [ width $ num 800.0
            , height $ num 500.0
            , viewBox 0.0 (-50.0) 800.0 500.0
            , attr "id" $ text svgId
            , attr "class" $ text "v3-gup-demo d3svg gup"
            ]
            `T.withChild`
              T.elem Group []  -- Empty group placeholder
    _ <- renderTree container svgTree
    pure unit

  -- Return update function
  pure \letters -> runD3v2M do
    svg <- select svgSelector :: _ (D3v2Selection_ SEmpty Element Unit)

    -- Convert Char array to LetterDatum array with indices
    let letterData = Array.mapWithIndex
          (\i c -> { letter: SCU.singleton c, index: toNumber i })
          letters

    -- Render the letters tree
    let tree :: Tree LetterDatum
        tree = createLettersTree letterData

    _ <- renderTree svg tree
    pure unit

-- | Filter string to alphanumeric characters only (for ID generation)
filterAlphaNum :: String -> String
filterAlphaNum s = SCU.fromCharArray $ Array.filter isAlphaNum (SCU.toCharArray s)
  where
    isAlphaNum c =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9')
