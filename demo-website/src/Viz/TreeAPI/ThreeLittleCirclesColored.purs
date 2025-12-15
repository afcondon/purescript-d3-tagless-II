module D3.Viz.TreeAPI.ThreeLittleCirclesColored where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3v3.Integration (v3Attr, v3AttrStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST (Tree, joinData)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- | Three Colored Circles - Attributes from data
-- |
-- | Data: ['red', 'green', 'blue'] - color names
-- | Visual: Three circles, each colored by its datum
-- |
-- | Key concept: Attributes can be functions of the data

type ColorDatum = { color :: String, index :: Number }

colorData :: Array ColorDatum
colorData =
  [ { color: "red", index: 0.0 }
  , { color: "green", index: 1.0 }
  , { color: "blue", index: 2.0 }
  ]

threeLittleCirclesColored :: String -> Effect Unit
threeLittleCirclesColored selector = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Define the tree with a DATA JOIN
  -- Now the fill color comes from the datum itself!
  -- Very compact size for foundational example
  let tree :: Tree ColorDatum
      tree =
        T.named SVG "svg"
          [ v3Attr "width" (lit 400.0)
          , v3Attr "height" (lit 100.0)
          , v3AttrStr "viewBox" (str "0 0 400 100")
          , v3AttrStr "id" (str "three-circles-colored-svg")
          , v3AttrStr "class" (str "tree-api-example")
          ]
          `T.withChild`
            -- joinData creates one circle per datum
            -- The key difference: fill uses d.color from the datum
            (joinData "circles" "circle" colorData $ \d ->
              T.elem Circle
                [ v3Attr "cx" (lit (100.0 + d.index * 100.0))    -- Space circles 100px apart
                , v3Attr "cy" (lit 50.0)                          -- All at same vertical position
                , v3Attr "r" (lit 25.0)                      -- All same size
                , v3AttrStr "fill" (str d.color)                     -- Color FROM THE DATUM!
                ])

  -- Render the tree
  selections <- renderTree container tree

  liftEffect do
    Console.log "=== Three Colored Circles ==="
    Console.log ""
    Console.log "Data: ['red', 'green', 'blue']"
    Console.log "Result: 3 circles, each with its data-driven color"
    Console.log ""

    case Map.lookup "circles" selections of
      Just _ -> Console.log "✓ Circles created with data-driven colors"
      Nothing -> Console.log "✗ Missing circles"

    Console.log ""
    Console.log "The key concept: Attributes can be FUNCTIONS of the data."
    Console.log "Here, fill d.color means each circle gets its color from its datum."
    Console.log "This is the foundation of data visualization: visual encoding of data."
    Console.log ""
