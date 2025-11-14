module D3.Viz.ThreeLittleCirclesV2 where

-- | Three Little Circles using PSD3v2
-- |
-- | Demonstrates the new type-safe selection API with:
-- | - Phantom type states (SEmpty, SBound, SPending, SExiting)
-- | - renderData high-level API
-- | - Type-safe attributes with smart constructors
-- |
-- | Based on Mike Bostock's "Three Little Circles" (https://bost.ocks.org/mike/circles/)
-- | See Acknowledgements page for full credits

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import PSD3v2.Attribute.Types (Attribute(..), AttributeName(..), AttributeValue(..), fill, radius)
import PSD3v2.Capabilities.Selection (class SelectionM, select, renderData)
import PSD3v2.Interpreter.D3v2 (runD3v2M)
import PSD3v2.Selection.Types (ElementType(..))

-- | Draw three circles using the new PSD3v2 API
-- |
-- | This demonstrates:
-- | - Type-safe selection operations
-- | - Clean DSL with SelectionM constraint
-- | - Phantom types prevent misuse (can't append to bound selections, etc.)
-- |
-- | Compare to original ThreeLittleCircles.purs:
-- | - No simpleJoin + setAttributes - just renderData
-- | - No keyIsID_ - Ord instance controls identity
-- | - Type-safe attributes (cx, cy, radius are smart constructors)
drawThreeCircles :: forall m sel. SelectionM sel m => m Unit
drawThreeCircles = do
  -- Select the SVG element
  -- The datum type (Int) is inferred from the data we bind below
  svg <- select "svg"

  -- Render circles with real data [32, 57, 293]
  -- The renderData function handles enter/update/exit automatically
  _ <- renderData Circle [32, 57, 293] "circle" svg
    (Just enterAttrs)  -- Enter: new circles
    (Just updateAttrs) -- Update: existing circles (if any)
    Nothing            -- Exit: just remove (no special styling)

  pure unit
  where
    -- Attributes for entering circles (green)
    -- Note: Using datum value to scale radius
    enterAttrs d =
      [ fill "green"
      , IndexedAttr (AttributeName "cx") (\_ i -> NumberValue (toNumber i * 100.0 + 50.0))  -- Position based on index
      , StaticAttr (AttributeName "cy") (NumberValue 50.0)
      , radius (toNumber d / 10.0)  -- Scale radius based on data value
      ]

    -- Attributes for updating circles (could change color, size, etc.)
    updateAttrs d =
      [ fill "orange"  -- Change existing circles to orange
      , IndexedAttr (AttributeName "cx") (\_ i -> NumberValue (toNumber i * 100.0 + 50.0))
      , StaticAttr (AttributeName "cy") (NumberValue 50.0)
      , radius (toNumber d / 10.0)
      ]

-- | Run the example with the D3v2 interpreter
main :: Effect Unit
main = runD3v2M drawThreeCircles
