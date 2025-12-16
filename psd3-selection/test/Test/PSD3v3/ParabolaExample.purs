-- | PSD3v3 Parabola Example
-- |
-- | A complete example showing the v3 DSL for a real visualization.
-- | Demonstrates how the same definitions work across all interpreters.
module Test.PSD3.Expr.ParabolaExample where

import Prelude hiding (add, sub, mul, div)

import Data.Array (mapWithIndex, foldl)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))

-- DSL
import PSD3.Expr.Expr (class NumExpr, lit, add, mul)
import PSD3.Expr.Units (class UnitExpr, class UnitArith, Pixels, px)
import PSD3.Expr.Datum (class DatumExpr, field, index)
import PSD3.Expr.Sugar ((*:), (+:), (-:), n)

-- Interpreters
import PSD3.Expr.Interpreter.Eval (EvalD, runEvalD)
import PSD3.Expr.Interpreter.CodeGen (CodeGen, runCodeGen)
import PSD3.Expr.Interpreter.SVG (SVGD, runSVGD)
import PSD3.Expr.Interpreter.PureSVG as Pure

-- =============================================================================
-- Data Types
-- =============================================================================

-- | The row type for our datum (used in constraints)
type ParabolaRow = (x :: Number, y :: Number)

-- | The actual record type
type ParabolaPoint = { x :: Number, y :: Number }

-- | Sample data - points on y = x²
parabolaData :: Array ParabolaPoint
parabolaData =
  [ { x: -10.0, y: 100.0 }
  , { x: -7.5, y: 56.25 }
  , { x: -5.0, y: 25.0 }
  , { x: -2.5, y: 6.25 }
  , { x: 0.0, y: 0.0 }
  , { x: 2.5, y: 6.25 }
  , { x: 5.0, y: 25.0 }
  , { x: 7.5, y: 56.25 }
  , { x: 10.0, y: 100.0 }
  ]

-- =============================================================================
-- Field Accessors (the "boilerplate" - could be generated)
-- =============================================================================

-- | Access x field from datum
xVal :: forall repr. DatumExpr repr ParabolaRow => repr Number
xVal = field (Proxy :: Proxy "x")

-- | Access y field from datum
yVal :: forall repr. DatumExpr repr ParabolaRow => repr Number
yVal = field (Proxy :: Proxy "y")

-- =============================================================================
-- Scale Functions (polymorphic!)
-- =============================================================================

-- | Scale x from domain [-10, 10] to range [50, 350]
-- | Formula: (x + 10) * 15 + 50
-- | This maps: -10 → 50, 0 → 200, 10 → 350
scaleX :: forall repr. NumExpr repr => DatumExpr repr ParabolaRow => repr Number
scaleX = (xVal +: 10.0) *: 15.0 +: 50.0

-- | Scale y from domain [0, 100] to range [250, 50] (inverted for SVG coords)
-- | Formula: 250 - (y * 2)
-- | This maps: 0 → 250, 100 → 50
scaleY :: forall repr. NumExpr repr => DatumExpr repr ParabolaRow => repr Number
scaleY = n 250.0 -: 0.0 +: 0.0  -- Start with literal, then subtract scaled y
-- Actually, let's write it properly:
-- scaleY = add (lit 250.0) (mul yVal (lit (-2.0)))

-- Better version using the full DSL
scaleYProper :: forall repr. NumExpr repr => DatumExpr repr ParabolaRow => repr Number
scaleYProper = add (lit 250.0) (mul yVal (lit (-2.0)))

-- =============================================================================
-- Attribute Expressions (polymorphic!)
-- =============================================================================

-- | Circle radius - could be static or data-driven
circleRadius :: forall repr. NumExpr repr => repr Number
circleRadius = lit 5.0

-- | Color based on y value - darker for higher y
-- | This is a placeholder - would need string interpolation for real HSL
circleColor :: forall repr. NumExpr repr => DatumExpr repr ParabolaRow => repr Number
circleColor = yVal  -- Just the y value for now

-- =============================================================================
-- Generate SVG Output
-- =============================================================================

-- | Generate a single circle element as SVG string
circleToSVG :: ParabolaPoint -> Int -> String
circleToSVG point idx =
  let
    cxVal = runSVGD (scaleX :: SVGD ParabolaPoint Number) point idx
    cyVal = runSVGD (scaleYProper :: SVGD ParabolaPoint Number) point idx
    rVal = runSVGD (circleRadius :: SVGD ParabolaPoint Number) point idx
  in
    "<circle cx=\"" <> cxVal <> "\" cy=\"" <> cyVal <> "\" r=\"" <> rVal <> "\" fill=\"steelblue\" />"

-- | Generate full SVG document (string concatenation approach)
generateSVG :: Array ParabolaPoint -> String
generateSVG points =
  let
    circles = mapWithIndex (\i p -> circleToSVG p i) points
    circleStr = foldl (\acc c -> acc <> "\n    " <> c) "" circles
  in
    """<svg width="400" height="300" xmlns="http://www.w3.org/2000/svg">
  <rect width="400" height="300" fill="#f5f5f5" />
  """ <> circleStr <> """
</svg>"""

-- | Generate SVG using the Pure SVG builder (more structured approach)
-- | This demonstrates a no-D3 approach that works in browser OR server
pureSVGParabola :: Array ParabolaPoint -> Pure.SVGDoc
pureSVGParabola points =
  Pure.svg 400.0 300.0
    [ -- Background
      Pure.rect
        { x: "0", y: "0", width: "400", height: "300" }
        [ { name: "fill", value: "#f5f5f5" } ]
    , -- Title
      Pure.text
        { x: "200", y: "25" }
        [ { name: "text-anchor", value: "middle" }
        , { name: "font-family", value: "sans-serif" }
        , { name: "font-size", value: "14" }
        ]
        "y = x² (Pure SVG, no D3!)"
    , -- Axis line
      Pure.line
        { x1: "50", y1: "250", x2: "350", y2: "250" }
        [ { name: "stroke", value: "#999" }
        , { name: "stroke-width", value: "1" }
        ]
    , -- Data points group
      Pure.g
        [ { name: "class", value: "data-points" } ]
        (Pure.forData points \p i ->
          let
            -- Use our polymorphic scale functions!
            cxVal = runSVGD (scaleX :: SVGD ParabolaPoint Number) p i
            cyVal = runSVGD (scaleYProper :: SVGD ParabolaPoint Number) p i
          in
            Pure.circle
              { cx: cxVal, cy: cyVal, r: "6" }
              [ { name: "fill", value: "steelblue" }
              , { name: "stroke", value: "white" }
              , { name: "stroke-width", value: "2" }
              ]
        )
    ]

-- =============================================================================
-- Demo
-- =============================================================================

runExample :: Effect Unit
runExample = do
  log "\n=== PSD3v3 Parabola Example ==="

  log "\n--- Scale Functions (polymorphic definitions) ---"
  log "scaleX = (xVal +: 10.0) *: 15.0 +: 50.0"
  log "scaleY = 250 - (y * 2)"

  log "\n--- CodeGen Output (PureScript source) ---"
  log $ "scaleX: " <> runCodeGen (scaleX :: CodeGen Number)
  log $ "scaleY: " <> runCodeGen (scaleYProper :: CodeGen Number)

  log "\n--- Evaluated for each data point ---"
  log "Point               → cx      cy"
  log "─────────────────────────────────"
  for_ (mapWithIndex (\i p -> { i, p }) parabolaData) \{ i, p } -> do
    let cxVal = runEvalD (scaleX :: EvalD ParabolaPoint Number) p i
    let cyVal = runEvalD (scaleYProper :: EvalD ParabolaPoint Number) p i
    log $ show p <> " → " <> show cxVal <> "   " <> show cyVal

  log "\n--- Generated SVG (server-side rendering!) ---"
  log $ generateSVG parabolaData

  log "\n--- Pure SVG Builder (no D3!) ---"
  log $ Pure.renderDoc $ pureSVGParabola parabolaData

  log "\n--- Key Insight ---"
  log "The SAME scaleX definition produces:"
  log "  • Runtime values for D3 DOM manipulation"
  log "  • Source code for SemiQuine round-tripping"
  log "  • SVG strings for server-side rendering"
  log ""
  log "For simple charts, skip D3 entirely!"
  log "  • No D3 bundle (~250KB savings)"
  log "  • Works server-side OR browser-side"
  log "  • Just innerHTML the SVG string"
