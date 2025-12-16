module D3.Viz.TreeAPI.ThreeLittleCirclesGreen where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Expr.Friendly (num, text, attr, viewBox, width, height, cx, cy, r, fill)
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST (Tree, joinData)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- | Three Little Circles (Green) - The absolute simplest example
-- |
-- | Data: ['a', 'b', 'c'] - just three pieces of data
-- | Visual: Three green circles positioned horizontally

-- We need to track position, so we'll use a record with the label and index
type SimpleDatum = { label :: String, index :: Number }

simpleData :: Array SimpleDatum
simpleData =
  [ { label: "a", index: 0.0 }
  , { label: "b", index: 1.0 }
  , { label: "c", index: 2.0 }
  ]

threeLittleCirclesGreen :: String -> Effect Unit
threeLittleCirclesGreen selector = runD3v2M do
  container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Define the tree with a DATA JOIN
  -- Each datum has a label and index for positioning
  -- Very compact size for foundational example
  let tree :: Tree SimpleDatum
      tree =
        T.named SVG "svg"
          [ width $ num 400.0
          , height $ num 100.0
          , viewBox 0.0 0.0 400.0 100.0
          , attr "id" $ text "three-circles-green-svg"
          , attr "class" $ text "tree-api-example"
          ]
          `T.withChild`
            -- joinData creates one circle per datum
            -- We use the index from the datum for positioning
            (joinData "circles" "circle" simpleData $ \d ->
              T.elem Circle
                [ cx $ num (100.0 + d.index * 100.0)    -- Space circles 100px apart
                , cy $ num 50.0                          -- All at same vertical position
                , r $ num 25.0                      -- All same size
                , fill $ text "green"                     -- All green
                ])

  -- Render the tree
  selections <- renderTree container tree

  liftEffect do
    Console.log "=== Three Little Circles (Green) ==="
    Console.log ""
    Console.log "Data: ['a', 'b', 'c']"
    Console.log "Result: 3 green circles"
    Console.log ""

    case Map.lookup "circles" selections of
      Just _ -> Console.log "✓ Circles created via data join"
      Nothing -> Console.log "✗ Missing circles"

    Console.log ""
    Console.log "The key concept: one circle per datum, positioned by index."
    Console.log "The datum content ('a', 'b', 'c') doesn't matter here -"
    Console.log "what matters is that we have THREE pieces of data."
    Console.log ""
