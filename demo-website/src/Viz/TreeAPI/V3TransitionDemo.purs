-- | PSD3v3 Transition Demo - Declarative Transitions with v3 Expressions
-- |
-- | This demonstrates the fully declarative approach to transitions:
-- | - sceneJoin defines enter behavior with initial state + transition
-- | - Template defines final state using v3 expressions
-- | - No Map lookup, no unsafeCoerce - type flows through the tree
-- |
-- | Key insight: Transitions belong IN the tree structure, not after it.
-- | This keeps the datum type chain intact.
module D3.Viz.TreeAPI.V3TransitionDemo where

import Prelude hiding (add)

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Type.Proxy (Proxy(..))

-- v2 infrastructure
import PSD3.Expr.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.Internal.Transition.Types (staggeredTransition)
import PSD3.AST (Tree)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- v3 DSL
import PSD3.Expr.Expr (class NumExpr, lit, str)
import PSD3.Expr.Datum (class DatumExpr, field)
import PSD3.Expr.Sugar ((*:), (+:))
import PSD3.Expr.Interpreter.CodeGen (CodeGen, runCodeGen)
import PSD3.Expr.Interpreter.Eval (EvalD, runEvalD)

-- =============================================================================
-- Data Type
-- =============================================================================

type CircleData =
  { id :: Int
  , x :: Number -- X position
  , y :: Number -- Y position
  , value :: Number -- 0 to 100 (affects radius and color)
  }

type CircleRow = (id :: Int, x :: Number, y :: Number, value :: Number)

-- Circle data with positions spread out
circleData :: Array CircleData
circleData =
  [ { id: 1, x: 80.0, y: 80.0, value: 30.0 }
  , { id: 2, x: 160.0, y: 220.0, value: 60.0 }
  , { id: 3, x: 250.0, y: 60.0, value: 90.0 }
  , { id: 4, x: 340.0, y: 200.0, value: 45.0 }
  , { id: 5, x: 420.0, y: 100.0, value: 75.0 }
  ]

-- =============================================================================
-- v3 Expressions (Polymorphic!)
-- =============================================================================

-- | X position from datum
posX :: forall repr. NumExpr repr => DatumExpr repr CircleRow => repr Number
posX = field (Proxy :: Proxy "x")

-- | Y position from datum
posY :: forall repr. NumExpr repr => DatumExpr repr CircleRow => repr Number
posY = field (Proxy :: Proxy "y")

-- | Radius based on value: (value * 0.2) + 8
-- | Higher values = larger circles
circleRadius :: forall repr. NumExpr repr => DatumExpr repr CircleRow => repr Number
circleRadius = valueField *: 0.2 +: 8.0
  where
  valueField = field (Proxy :: Proxy "value")

-- =============================================================================
-- v3→v2 Integration
-- =============================================================================

-- | Evaluate v3 expression with datum (for templates)
evalExpr :: forall a. EvalD CircleData a -> CircleData -> a
evalExpr expr datum = runEvalD expr datum 0

-- =============================================================================
-- Browser Demo
-- =============================================================================

v3TransitionDemo :: Effect Unit
v3TransitionDemo = runD3v2M do
  container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Fully declarative tree with transitions INSIDE the structure
  -- No Map lookup needed - type flows through the entire tree
  let
    tree :: Tree CircleData
    tree =
      T.named SVG "svg"
        [ v3Attr "width" (lit 500.0)
        , v3Attr "height" (lit 300.0)
        , v3AttrStr "viewBox" (str "0 0 500 300")
        , v3AttrStr "id" (str "v3-transition-svg")
        , v3AttrStr "class" (str "v3-demo")
        ]
        `T.withChild`
          -- sceneJoin: template = final state, enterBehavior = initial state + transition
          ( T.sceneJoin "circles" "circle" circleData
              -- Template defines FINAL state (where elements end up)
              ( \d -> T.elem Circle
                  [ v3Attr "cx" (lit (evalExpr posX d)) -- v3: final X position
                  , v3Attr "cy" (lit (evalExpr posY d)) -- v3: final Y position
                  , v3Attr "r" (lit (evalExpr circleRadius d)) -- v3: radius from value
                  , v3AttrStr "fill" (str "#e67e22") -- final color (orange)
                  , v3AttrStr "stroke" (str "white")
                  , v3Attr "stroke-width" (lit 2.0)
                  ]
              )
              -- Behaviors: enter starts at center, transitions to final with stagger
              { keyFn: Nothing  -- No updates in this demo
              , enterBehavior: Just
                  { initialAttrs:
                      [ v3Attr "cx" (lit 250.0) -- start at center X
                      , v3Attr "cy" (lit 150.0) -- start at center Y
                      , v3AttrStr "fill" (str "#3498db") -- start color (blue)
                      ]
                  , transition: Just (staggeredTransition (Milliseconds 600.0) 100.0)
                  -- Each element delays 100ms more than previous
                  }
              , updateBehavior: Nothing
              , exitBehavior: Nothing
              }
          )

  -- Log what we're demonstrating
  liftEffect do
    Console.log "=== PSD3v3 Declarative Staggered Transition Demo ==="
    Console.log ""
    Console.log "v3 Expressions (final positions):"
    Console.log $ "  posX         → " <> runCodeGen (posX :: CodeGen Number)
    Console.log $ "  posY         → " <> runCodeGen (posY :: CodeGen Number)
    Console.log $ "  circleRadius → " <> runCodeGen (circleRadius :: CodeGen Number)
    Console.log ""
    Console.log "Approach: Fully declarative with sceneJoin + staggeredTransition"
    Console.log "  • Template defines final state (v3 expressions)"
    Console.log "  • enterBehavior defines initial state + staggered transition"
    Console.log "  • staggeredTransition: 600ms duration, 100ms stagger per element"
    Console.log "  • No Map lookup, no unsafeCoerce - type flows through tree"
    Console.log ""

  -- Render - transitions happen automatically!
  _ <- renderTree container tree

  liftEffect do
    Console.log "Staggered transition triggered by renderTree!"
    Console.log ""
    Console.log "Key insights:"
    Console.log "  1. Transitions belong IN the tree structure"
    Console.log "  2. staggerDelay in TransitionConfig handles per-element delays"
    Console.log "  3. The datum type chain stays intact from definition to render"
