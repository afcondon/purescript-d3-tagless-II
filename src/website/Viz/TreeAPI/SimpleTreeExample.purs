module D3.Viz.TreeAPI.SimpleTreeExample where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3v2.Attribute.Types (width, height, viewBox, id_, class_, cx, cy, radius, fill, textContent, textAnchor, x, y)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3v2.VizTree.Tree as T

-- | Simple test: Render a basic tree structure without data joins
-- |
-- | Structure:
-- | ```
-- | div#viz
-- |   └─ svg (800x600)
-- |      └─ g.container
-- |         ├─ circle (at 100, 100)
-- |         └─ text (at 100, 130)
-- | ```
testSimpleTree :: Effect Unit
testSimpleTree = runD3v2M do
  -- Select the container
  container <- select "#viz"

  -- Define the tree structure using the declarative API
  let tree =
        T.named "svg" SVG [width 800.0, height 600.0, viewBox "0 0 800 600", id_ "simple-tree-svg"]
          `T.withChild`
            (T.named "container" Group [class_ "container"]
              `T.withChildren`
                [ T.named "circle" Circle [cx 100.0, cy 100.0, radius 20.0, fill "steelblue"]
                , T.named "text" Text [x 100.0, y 130.0, textContent "Hello Tree API!", textAnchor "middle"]
                ])

  -- Render the tree
  selections <- renderTree container tree

  -- Log what we got back
  liftEffect do
    case Map.lookup "svg" selections of
      Just _ -> Console.log "✓ Found svg selection"
      Nothing -> Console.log "✗ Missing svg selection"

    case Map.lookup "container" selections of
      Just _ -> Console.log "✓ Found container selection"
      Nothing -> Console.log "✗ Missing container selection"

    case Map.lookup "circle" selections of
      Just _ -> Console.log "✓ Found circle selection"
      Nothing -> Console.log "✗ Missing circle selection"

    case Map.lookup "text" selections of
      Just _ -> Console.log "✓ Found text selection"
      Nothing -> Console.log "✗ Missing text selection"

    Console.log "Simple tree rendering complete!"
