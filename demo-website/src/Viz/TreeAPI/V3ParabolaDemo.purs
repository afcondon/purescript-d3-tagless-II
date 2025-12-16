-- | Expression DSL Parabola Demo - Live Browser Rendering
-- |
-- | This demonstrates the v3 "Finally Tagless" DSL rendering to actual DOM via D3.
-- |
-- | The same polymorphic expressions produce:
-- |   - Runtime values for D3 (EvalD)
-- |   - Source code for display (CodeGen)
-- |   - SVG strings for server-side (SVGD) - not shown here
-- |
-- | This is the "money shot" - same expression, three different outputs!
module D3.Viz.TreeAPI.V3ParabolaDemo where

import Prelude hiding (add)

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Type.Proxy (Proxy(..))

-- v2 infrastructure for D3 rendering
import PSD3.Expr.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST (Tree)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- v3 DSL imports
import PSD3.Expr.Expr (class NumExpr, lit, add, str)
import PSD3.Expr.Datum (class DatumExpr, field)
import PSD3.Expr.Sugar ((*:), (+:))
import PSD3.Expr.Interpreter.CodeGen (CodeGen, runCodeGen)
import PSD3.Expr.Interpreter.Eval (EvalD, runEvalD)

-- =============================================================================
-- Data Type
-- =============================================================================

type ParabolaPoint = { x :: Number, y :: Number }

type ParabolaRow = ( x :: Number, y :: Number )

-- Sample data: y = x²
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
-- v3 Expressions (Polymorphic!)
-- =============================================================================

-- | Scale X coordinate: (x + 5) * 30 + 50
-- |
-- | This expression is polymorphic over ANY repr that implements the typeclasses.
-- | It can produce: Number (EvalD), String (CodeGen), String (SVGD)
scaleX :: forall repr. NumExpr repr => DatumExpr repr ParabolaRow => repr Number
scaleX = (xField +: 5.0) *: 30.0 +: 50.0
  where
    xField = field (Proxy :: Proxy "x")

-- | Scale Y coordinate: 200 - (y * 1.8)
scaleY :: forall repr. NumExpr repr => DatumExpr repr ParabolaRow => repr Number
scaleY = add (lit 200.0) (yField *: (-1.8))
  where
    yField = field (Proxy :: Proxy "y")

-- | Point radius based on distance from origin
-- | Larger radius for points near the vertex
pointRadius :: forall repr. NumExpr repr => DatumExpr repr ParabolaRow => repr Number
pointRadius = add (lit 6.0) (yField *: (-0.05))
  where
    yField = field (Proxy :: Proxy "y")

-- =============================================================================
-- v3→v2 Integration: Evaluate expressions with datum
-- =============================================================================

-- | Evaluate a v3 expression with a datum to get a concrete value
-- | This bridges v3 polymorphic expressions → v2 static attributes
evalExpr :: forall a. EvalD ParabolaPoint a -> ParabolaPoint -> a
evalExpr expr datum = runEvalD expr datum 0

-- =============================================================================
-- Browser Demo
-- =============================================================================

v3ParabolaDemo :: Effect Unit
v3ParabolaDemo = runD3v2M do
  container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Define the visualization tree using v3 expressions
  -- The template callback receives each datum, we evaluate v3 expressions with it
  let tree :: Tree ParabolaPoint
      tree =
        T.named SVG "svg"
          [ v3Attr "width" (lit 400.0)
          , v3Attr "height" (lit 250.0)
          , v3AttrStr "viewBox" (str "0 0 400 250")
          , v3AttrStr "id" (str "v3-parabola-svg")
          , v3AttrStr "class" (str "v3-demo")
          ]
          `T.withChild`
            (T.joinData "points" "circle" parabolaData $ \d ->
              -- v3 expressions are evaluated with datum → static v2 attributes
              T.elem Circle
                [ v3Attr "cx" (lit (evalExpr scaleX d))       -- v3 expression evaluated!
                , v3Attr "cy" (lit (evalExpr scaleY d))       -- v3 expression evaluated!
                , v3Attr "r" (lit (evalExpr pointRadius d))  -- v3 expression evaluated!
                , v3AttrStr "fill" (str "#3498db")
                , v3AttrStr "stroke" (str "white")
                , v3Attr "stroke-width" (lit 2.0)
                ])

  -- Render to DOM via D3!
  selections <- renderTree container tree

  -- Log the "money shot" - same expressions, different outputs
  liftEffect do
    Console.log "=== Expression DSL Parabola Demo: Live D3 Rendering ==="
    Console.log ""
    Console.log "v3 Expressions (polymorphic definitions):"
    Console.log "  scaleX = (x + 5) * 30 + 50"
    Console.log "  scaleY = 200 + (y * -1.8)"
    Console.log ""
    Console.log "CodeGen output (PureScript source):"
    Console.log $ "  scaleX → " <> runCodeGen (scaleX :: CodeGen Number)
    Console.log $ "  scaleY → " <> runCodeGen (scaleY :: CodeGen Number)
    Console.log ""
    Console.log "EvalD output (D3 runtime values for sample point):"
    let pt = { x: -10.0, y: 100.0 }  -- Sample point
    let cxVal = runEvalD scaleX pt 0
    let cyVal = runEvalD scaleY pt 0
    Console.log $ "  { x: " <> show pt.x <> ", y: " <> show pt.y <> " } → cx=" <> show cxVal <> ", cy=" <> show cyVal
    Console.log ""
    Console.log "Same expression, three outputs:"
    Console.log "  • CodeGen: produces PureScript source code"
    Console.log "  • EvalD: produces runtime Number values for D3"
    Console.log "  • SVGD: produces SVG attribute strings (server-side)"
    Console.log ""

    case Map.lookup "points" selections of
      Just _ -> Console.log "✓ Points rendered to DOM via D3 (check browser!)"
      Nothing -> Console.log "✗ No points selection found"
