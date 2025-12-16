-- | General Update Pattern - V2 Implementation
-- |
-- | The classic D3 enter/update/exit pattern using PSD3v2 primitives.
-- | Letters appear (green, fall in), update (gray, slide), exit (brown, fall out).
module D3.Viz.GUPv2 where

import Prelude hiding (append)

import Data.Int (toNumber)
import Data.String.CodeUnits (singleton)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import PSD3.Internal.Attribute (class_, fill, fontSize, textContent, viewBox, x, y)
import PSD3.Internal.Capabilities.Selection (append, appendChild, openSelection, select, setAttrs, setAttrsExit, updateJoin)
import PSD3.Internal.Capabilities.Transition (withTransition, withTransitionExit)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), JoinResult(..), SEmpty)
import PSD3.Internal.Transition.Types (TransitionConfig, transition)
import Web.DOM.Element (Element)

-- | Key function for letters - each letter is its own key
charToKey :: Char -> String
charToKey = singleton

-- | X position based on alphabetical index
xFromIndex :: forall d. d -> Int -> Number
xFromIndex _ i = 50.0 + (toNumber i * 48.0)

-- | Transition config for animations
transitionConfig :: TransitionConfig
transitionConfig = transition (Milliseconds 2000.0)

-- | Initialize the GUP visualization
-- | Returns an update function that can be called with new letter data
initGUP :: String -> Effect (Array Char -> Effect Unit)
initGUP selector = runD3v2M do
  -- Create SVG structure
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)
  svg <- appendChild SVG
    [ viewBox "0 -50 800 500"
    , class_ "d3svg gup"
    ] container
  letterGroup <- appendChild Group [] svg

  -- Return update function
  pure \letters -> runD3v2M do
    -- Open selection on existing text elements
    openSel <- openSelection letterGroup "text"

    -- Join data - get enter/update/exit selections
    JoinResult { enter, update, exit } <- updateJoin openSel Text letters charToKey "text"

    -- EXIT: Brown, fall down, then remove
    _ <- setAttrsExit [ class_ "exit", fill "brown" ] exit
    withTransitionExit transitionConfig exit [ y 400.0 ]

    -- UPDATE: Gray, slide to new position
    _ <- setAttrs [ class_ "update", fill "gray", y 200.0 ] update
    withTransition transitionConfig update [ x (xFromIndex :: Char -> Int -> Number) ]

    -- ENTER: Create elements, start green at top, fall to middle
    entered <- append Text
      [ class_ "enter"
      , fill "green"
      , x (xFromIndex :: Char -> Int -> Number)
      , y 0.0
      , textContent (singleton :: Char -> String)
      , fontSize 60.0
      ] enter
    withTransition transitionConfig entered [ y 200.0 ]

    pure unit
