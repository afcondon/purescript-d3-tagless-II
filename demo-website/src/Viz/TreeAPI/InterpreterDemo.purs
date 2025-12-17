module D3.Viz.TreeAPI.InterpreterDemo where

-- | Simple example visualizations for demonstrating alternative interpreters

import Prelude

import Data.Int (toNumber)
import PSD3.Expr.Integration (evalAttr, evalAttrStr, fnAttr, fnAttrStr, fnAttrI)
import PSD3.Expr.Expr (lit, str)
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST as T

-- | Three Little Circles - simple and familiar
threeLittleCircles :: T.Tree Unit
threeLittleCircles =
  T.named SVG "svg"
    [ evalAttr "width" (lit 300.0)
    , evalAttr "height" (lit 100.0)
    , evalAttrStr "viewBox" (str "0 0 300 100")
    ]
    `T.withChild`
      (T.named Group "circles"
        [ evalAttrStr "class" (str "circles-group")
        ]
        `T.withChildren`
          [ T.elem Circle
              [ evalAttr "cx" (lit 50.0)
              , evalAttr "cy" (lit 50.0)
              , evalAttr "r" (lit 20.0)
              , evalAttrStr "fill" (str "red")
              ]
          , T.elem Circle
              [ evalAttr "cx" (lit 150.0)
              , evalAttr "cy" (lit 50.0)
              , evalAttr "r" (lit 20.0)
              , evalAttrStr "fill" (str "green")
              ]
          , T.elem Circle
              [ evalAttr "cx" (lit 250.0)
              , evalAttr "cy" (lit 50.0)
              , evalAttr "r" (lit 20.0)
              , evalAttrStr "fill" (str "blue")
              ]
          ])

-- | Simple bar chart with data join
simpleBarChart :: T.Tree Int
simpleBarChart =
  let barData = [10, 25, 15, 30, 20]
  in T.named SVG "svg"
       [ evalAttr "width" (lit 400.0)
       , evalAttr "height" (lit 200.0)
       , evalAttrStr "viewBox" (str "0 0 400 200")
       ]
       `T.withChild`
         (T.joinData "bars" "rect" barData $ \_ ->
           T.elem Rect
             [ fnAttrI "x" ((\ (_ :: Int) i -> toNumber i * 70.0 + 20.0) :: Int -> Int -> Number)
             , fnAttrI "y" ((\ (value :: Int) _ -> 180.0 - toNumber value * 5.0) :: Int -> Int -> Number)
             , evalAttr "width" (lit 50.0)
             , fnAttrI "height" ((\ (value :: Int) _ -> toNumber value * 5.0) :: Int -> Int -> Number)
             , evalAttrStr "fill" (str "steelblue")
             ])

-- | Simple bar chart without data join (for interpreter demos)
simpleBarChartNoJoin :: T.Tree Unit
simpleBarChartNoJoin =
  T.named SVG "svg"
    [ evalAttr "width" (lit 400.0)
    , evalAttr "height" (lit 200.0)
    , evalAttrStr "viewBox" (str "0 0 400 200")
    , evalAttrStr "class" (str "bar-chart")
    ]
    `T.withChild`
      (T.named Group "bars"
        [ evalAttrStr "class" (str "bars-group")
        ]
        `T.withChildren`
          [ T.elem Rect
              [ evalAttr "x" (lit 20.0)
              , evalAttr "y" (lit 130.0)
              , evalAttr "width" (lit 50.0)
              , evalAttr "height" (lit 50.0)
              , evalAttrStr "fill" (str "steelblue")
              ]
          , T.elem Rect
              [ evalAttr "x" (lit 90.0)
              , evalAttr "y" (lit 55.0)
              , evalAttr "width" (lit 50.0)
              , evalAttr "height" (lit 125.0)
              , evalAttrStr "fill" (str "steelblue")
              ]
          , T.elem Rect
              [ evalAttr "x" (lit 160.0)
              , evalAttr "y" (lit 105.0)
              , evalAttr "width" (lit 50.0)
              , evalAttr "height" (lit 75.0)
              , evalAttrStr "fill" (str "steelblue")
              ]
          , T.elem Rect
              [ evalAttr "x" (lit 230.0)
              , evalAttr "y" (lit 30.0)
              , evalAttr "width" (lit 50.0)
              , evalAttr "height" (lit 150.0)
              , evalAttrStr "fill" (str "steelblue")
              ]
          , T.elem Rect
              [ evalAttr "x" (lit 300.0)
              , evalAttr "y" (lit 80.0)
              , evalAttr "width" (lit 50.0)
              , evalAttr "height" (lit 100.0)
              , evalAttrStr "fill" (str "steelblue")
              ]
          ])

-- | Nested structure example
nestedStructure :: T.Tree Unit
nestedStructure =
  T.named SVG "svg"
    [ evalAttr "width" (lit 200.0)
    , evalAttr "height" (lit 200.0)
    , evalAttrStr "viewBox" (str "0 0 200 200")
    ]
    `T.withChild`
      (T.named Group "outer"
        []
        `T.withChild`
          (T.named Group "inner"
            []
            `T.withChildren`
              [ T.elem Circle
                  [ evalAttr "cx" (lit 100.0)
                  , evalAttr "cy" (lit 100.0)
                  , evalAttr "r" (lit 50.0)
                  , evalAttrStr "fill" (str "purple")
                  ]
              , T.elem Text
                  [ evalAttr "x" (lit 100.0)
                  , evalAttr "y" (lit 105.0)
                  , evalAttrStr "textContent" (str "Hello!")
                  , evalAttrStr "fill" (str "white")
                  ]
              ]))
