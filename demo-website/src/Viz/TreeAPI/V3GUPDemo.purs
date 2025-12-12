-- | PSD3v3 General Update Pattern Demo
-- |
-- | The classic D3 "letters" example reimagined with:
-- | - v3 polymorphic expressions for computed attributes
-- | - sceneJoin with declarative enter/update/exit behaviors
-- | - Staggered transitions for visual appeal
-- |
-- | Data changes trigger the full GUP cycle:
-- | - ENTER: New letters fade in from above
-- | - UPDATE: Existing letters slide to new positions
-- | - EXIT: Removed letters fade out and drop
module D3.Viz.TreeAPI.V3GUPDemo
  ( v3GUPDemo
  , updateWithLetters
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
import PSD3v2.Attribute.Types (Attribute(..), AttributeName(..), AttributeValue(..), width, height, viewBox, id_, class_, fill, stroke, strokeWidth, x, y, fontSize, textAnchor, dominantBaseline, opacity)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.Transition.Types (Easing(..), staggeredTransition, transitionWith)
import PSD3v2.VizTree.Tree (Tree)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- v3 DSL
import PSD3v3.Expr (class NumExpr)
import PSD3v3.Datum (class DatumExpr, field)
import PSD3v3.Sugar ((*:), (+:))
import PSD3v3.Interpreter.CodeGen (CodeGen, runCodeGen)
import PSD3v3.Interpreter.Eval (EvalD, runEvalD)

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
  T.sceneJoin "letters" "text" letters
    -- Template: FINAL state for each letter (where they end up after enter/update)
    (\d -> T.elem Text
      [ x (evalExpr letterX d)   -- v3: computed X position
      , y letterY                -- Final Y position
      , fontSize 32.0
      , textAnchor "middle"
      , dominantBaseline "middle"
      , fill "#2c3e50"
      , opacity 1.0
      -- The text content is set via a special TextContent attribute
      , textContent d.letter
      ])
    -- Behaviors for enter/update/exit
    { keyFn: Just _.letter       -- Identity by letter, not by index!
    , enterBehavior: Just
        { initialAttrs:
            [ y 20.0              -- Start above
            , opacity 0.0         -- Start invisible
            , fill "#27ae60"      -- Green for entering
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
            [ DataAttr (AttributeName "x") (\d' -> NumberValue (evalExpr letterX d'))
            , StaticAttr (AttributeName "y") (NumberValue letterY)
            , StaticAttr (AttributeName "fill") (StringValue "#2c3e50")  -- Dark for stable
            , StaticAttr (AttributeName "opacity") (NumberValue 1.0)
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
            [ y 180.0             -- Drop below
            , opacity 0.0         -- Fade out
            , fill "#e74c3c"      -- Red for exiting
            ]
        , transition: Just $ transitionWith
            { duration: Milliseconds 500.0
            , delay: Nothing
            , staggerDelay: Just 40.0  -- Stagger exit
            , easing: Just CubicIn
            }
        }
    }

-- | Helper: text content attribute (special handling for text elements)
textContent :: String -> Attribute LetterDatum
textContent content = StaticAttr (AttributeName "textContent") (StringValue content)

-- =============================================================================
-- Public API
-- =============================================================================

-- | Initialize the GUP demo - creates SVG container and renders initial letters
v3GUPDemo :: Effect Unit
v3GUPDemo = do
  Console.log "=== PSD3v3 General Update Pattern Demo ==="
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
            [ width 600.0
            , height 200.0
            , viewBox "0 0 600 200"
            , id_ "v3-gup-svg"
            , class_ "v3-gup-demo"
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

  -- Render - sceneJoin handles enter/update/exit automatically!
  _ <- renderTree svg tree

  liftEffect do
    Console.log "GUP cycle complete!"
    Console.log "  - New letters entered (green, bounced in from above)"
    Console.log "  - Existing letters updated (slid to new positions)"
    Console.log "  - Removed letters exited (red, faded and dropped)"
