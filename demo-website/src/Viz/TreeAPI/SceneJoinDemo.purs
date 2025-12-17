module D3.Viz.TreeAPI.SceneJoinDemo where

-- | Simple demonstration of SceneNestedJoin with enter/update/exit
-- |
-- | This shows how SceneNestedJoin solves the type mixing problem cleanly!

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import PSD3.Expr.Integration (evalAttr, evalAttrStr, fnAttr, fnAttrStr)
import PSD3.Expr.Expr (lit, str)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Interpreter.D3 (D3v2M)
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.Internal.Transition.Types (Easing(CubicIn, CubicInOut))
import PSD3.AST as T

-- | Simple data point
type DataPoint = { x :: Number, y :: Number, color :: String }

-- | Scene data (holds all points for the scene)
type SceneData = { points :: Array DataPoint }

-- | Create SVG container (clears existing content first)
createSvgContainer :: String -> D3v2M Unit
createSvgContainer containerSelector = do
  container <- select containerSelector
  -- Clear any existing content to avoid duplicate SVGs
  liftEffect $ clearInnerHTML containerSelector
  let svgTree :: T.Tree Unit
      svgTree = T.named SVG "svg"
        [ evalAttr "width" (lit 800.0)
        , evalAttr "height" (lit 300.0)
        , evalAttrStr "viewBox" (str "0 0 800 300")
        , evalAttrStr "id" (str "scene-join-demo-svg")
        , evalAttrStr "class" (str "scene-join-demo")
        ]
        `T.withChildren` []  -- Empty children initially
  _ <- renderTree container svgTree
  pure unit

-- | Clear innerHTML of an element (FFI helper)
foreign import clearInnerHTML :: String -> Effect Unit

-- | Initial render with 5 points
drawInitial :: String -> D3v2M Unit
drawInitial containerSelector = do
  -- Create SVG container (idempotent - only creates if doesn't exist)
  createSvgContainer containerSelector
  -- Now update the circles
  updateCircles
    [ { x: 100.0, y: 100.0, color: "red" }
    , { x: 200.0, y: 150.0, color: "blue" }
    , { x: 300.0, y: 100.0, color: "green" }
    , { x: 400.0, y: 150.0, color: "orange" }
    , { x: 500.0, y: 100.0, color: "purple" }
    ]

-- | Update with 3 points (some exit, some stay)
updateToThree :: String -> D3v2M Unit
updateToThree _ = do
  updateCircles
    [ { x: 150.0, y: 120.0, color: "red" }    -- Update (moved)
    , { x: 300.0, y: 100.0, color: "green" }  -- Stay same
    , { x: 450.0, y: 130.0, color: "orange" } -- Update (moved)
    ]

-- | Update with 7 points (some enter, some update)
updateToSeven :: String -> D3v2M Unit
updateToSeven _ = do
  updateCircles
    [ { x: 100.0, y: 100.0, color: "red" }
    , { x: 200.0, y: 150.0, color: "blue" }
    , { x: 300.0, y: 100.0, color: "green" }
    , { x: 400.0, y: 150.0, color: "orange" }
    , { x: 500.0, y: 100.0, color: "purple" }
    , { x: 600.0, y: 150.0, color: "pink" }    -- New
    , { x: 700.0, y: 100.0, color: "cyan" }    -- New
    ]

-- | Update circles using SceneNestedJoin
updateCircles :: Array DataPoint -> D3v2M Unit
updateCircles points = do
  svgContainer <- select "#scene-join-demo-svg"
  let scene = { points }
      tree = createCirclesTree scene
  _ <- renderTree svgContainer tree
  pure unit

-- | Create tree with SceneNestedJoin - THIS IS THE CLEAN SOLUTION!
-- |
-- | Notice how we:
-- | 1. Use SceneData at the container level (Tree SceneData)
-- | 2. Decompose it into DataPoint array
-- | 3. Get full GUP behavior with enter/update/exit
-- | 4. No type gymnastics needed!
createCirclesTree :: SceneData -> T.Tree SceneData
createCirclesTree scene =
  -- SceneNestedJoin: decompose SceneData -> DataPoints with GUP!
  T.sceneNestedJoin "circles" "circle"
    [scene]              -- Outer data: SceneData
    (_.points)           -- Decompose: extract points array
    (\point -> T.elem Circle  -- Template for each DataPoint
      [ evalAttr "cx" (lit point.x)
      , evalAttr "cy" (lit point.y)
      , evalAttr "r" (lit 20.0)
      , evalAttrStr "fill" (str point.color)
      , evalAttrStr "stroke" (str "#000")
      , evalAttr "stroke-width" (lit 2.0)
      ])
    { enterBehavior: Just  -- Elements enter from center
        { initialAttrs:
            [ evalAttr "cx" (lit 400.0)
            , evalAttr "cy" (lit 150.0)
            , evalAttr "r" (lit 0.0)
            ]
        , transition: Just
            { duration: Milliseconds 600.0
            , delay: Nothing
            , staggerDelay: Nothing
            , easing: Just CubicInOut
            }
        }
    , updateBehavior: Just  -- Elements transition to new positions
        { attrs:
            [ fnAttr "cx" (_.x)
            , fnAttr "cy" (_.y)
            , evalAttr "r" (lit 20.0)
            , fnAttrStr "fill" (_.color)
            , evalAttrStr "stroke" (str "#000")
            , evalAttr "stroke-width" (lit 2.0)
            ]
        , transition: Just
            { duration: Milliseconds 400.0
            , delay: Nothing
            , staggerDelay: Nothing
            , easing: Just CubicInOut
            }
        }
    , exitBehavior: Just  -- Elements shrink away
        { attrs:
            [ evalAttr "r" (lit 0.0)
            ]
        , transition: Just
            { duration: Milliseconds 300.0
            , delay: Nothing
            , staggerDelay: Nothing
            , easing: Just CubicIn
            }
        }
    }
