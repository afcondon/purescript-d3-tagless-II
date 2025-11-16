module D3.Viz.GUPV2 where

-- | General Update Pattern (GUP) using PSD3v2
-- |
-- | Demonstrates the enter-update-exit pattern with transitions:
-- | - Enter: New letters slide in from top (green)
-- | - Update: Existing letters move to new positions (gray)
-- | - Exit: Removed letters slide down and fade out (brown)
-- |
-- | Based on Mike Bostock's classic GUP example

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray, singleton)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import PSD3v2.Attribute.Types (Attribute, fill, x, y, fontSize, textContent, class_, id_, width, height, viewBox)
import PSD3v2.Capabilities.Selection (class SelectionM, select, appendChild, joinData, append, setAttrs, setAttrsExit, remove, merge)
import PSD3v2.Capabilities.Transition (class TransitionM, withTransition, withTransitionExit)
import PSD3v2.Interpreter.D3v2 (runD3v2M)
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..))
import PSD3v2.Transition.Types (transitionWith)

-- | Update the visualization with new text
-- |
-- | This is the core GUP function that handles:
-- | 1. Join new data with existing elements
-- | 2. Enter: Create and animate new elements
-- | 3. Update: Transition existing elements to new positions
-- | 4. Exit: Remove old elements with animation
updateText :: forall m sel. SelectionM sel m => TransitionM sel m => MonadEffect m => String -> String -> m Unit
updateText selector text = do
  -- Convert string to array of characters and sort alphabetically
  -- Sorting is essential for the GUP pattern - it allows update letters
  -- to slide into their correct positions between enter letters
  let letters = Array.sort $ toCharArray text

  -- DEBUG: Log the new data
  liftEffect $ log $ "GUP: New letters = " <> show letters

  -- Select the SVG group
  svgGroup <- select selector

  -- Data join
  JoinResult { enter, update, exit } <- joinData letters "text" svgGroup

  -- DEBUG: Log join results with positions
  liftEffect $ forWithIndex_ letters \index letter -> do
    log $ "  letters[" <> show index <> "] = '" <> singleton letter <> "' -> x=" <> show (50.0 + toNumber index * 48.0)

  -- ============================================================================
  -- ENTER: New letters
  -- ============================================================================
  -- Create new text elements
  enterEls <- append Text
    [ class_ "enter"
    , fill "green"
    , x xFromIndex
    , y 0.0  -- Start above viewport
    , fontSize 60.0
    , textContent charToString
    ]
    enter

  -- Transition enter elements down to their final position
  let enterTransition = transitionWith
        { duration: Milliseconds 750.0
        , delay: Nothing
        , easing: Nothing
        }

  withTransition enterTransition enterEls
    [ y 200.0  -- Slide down to middle
    ]

  -- ============================================================================
  -- UPDATE: Existing letters that remain
  -- ============================================================================
  -- Update styling
  _ <- setAttrs
    [ class_ "update"
    , fill "gray"
    , y 200.0
    , fontSize 60.0
    ]
    update

  -- Transition to new horizontal positions
  let updateTransition = transitionWith
        { duration: Milliseconds 750.0
        , delay: Nothing
        , easing: Nothing
        }

  withTransition updateTransition update
    [ x xFromIndex
    ]

  -- ============================================================================
  -- EXIT: Letters being removed
  -- ============================================================================
  -- Update exit styling
  _ <- setAttrsExit
    [ class_ "exit"
    , fill "brown"
    ]
    exit

  -- Transition exit elements down and then remove
  let exitTransition = transitionWith
        { duration: Milliseconds 750.0
        , delay: Nothing
        , easing: Nothing
        }

  withTransitionExit exitTransition exit
    [ y 400.0  -- Slide down off viewport
    ]
  -- Note: Elements are automatically removed after transition completes
  -- via transitionRemove_ in the interpreter

  pure unit

-- | Calculate X position from index
-- | Spaces letters 48 pixels apart, starting at x=50
xFromIndex :: Char -> Int -> Number
xFromIndex _ i = 50.0 + (toNumber i * 48.0)

-- | Convert Char to String for text content
charToString :: Char -> String
charToString = singleton

-- | Initialize the SVG structure
-- |
-- | Creates the SVG container and returns the selector for the group
initializeGUP :: forall m sel. SelectionM sel m => String -> m String
initializeGUP containerSelector = do
  container <- select containerSelector

  svg <- appendChild SVG
    [ id_ "gup-v2-svg"
    , width 800.0
    , height 400.0
    , viewBox "0 0 800 400"
    , class_ "gup-v2"
    ]
    container

  _ <- appendChild Group
    [ id_ "letter-group"
    ]
    svg

  pure "#letter-group"

-- | Run the GUP example with the D3v2 interpreter (for standalone testing)
main :: Effect Unit
main = runD3v2M do
  groupSelector <- initializeGUP "#example-viz"
  updateText groupSelector "HELLO"
