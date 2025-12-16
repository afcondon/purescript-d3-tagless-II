module D3.Viz.TreeAPI.InterpreterDemo where

-- | Simple example visualizations for demonstrating alternative interpreters

import Prelude

import Data.Int (toNumber)
import PSD3.Expr.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr, v3AttrFnI)
import PSD3.Expr.Expr (lit, str)
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST as T

-- | Three Little Circles - simple and familiar
threeLittleCircles :: T.Tree Unit
threeLittleCircles =
  T.named SVG "svg"
    [ v3Attr "width" (lit 300.0)
    , v3Attr "height" (lit 100.0)
    , v3AttrStr "viewBox" (str "0 0 300 100")
    ]
    `T.withChild`
      (T.named Group "circles"
        [ v3AttrStr "class" (str "circles-group")
        ]
        `T.withChildren`
          [ T.elem Circle
              [ v3Attr "cx" (lit 50.0)
              , v3Attr "cy" (lit 50.0)
              , v3Attr "r" (lit 20.0)
              , v3AttrStr "fill" (str "red")
              ]
          , T.elem Circle
              [ v3Attr "cx" (lit 150.0)
              , v3Attr "cy" (lit 50.0)
              , v3Attr "r" (lit 20.0)
              , v3AttrStr "fill" (str "green")
              ]
          , T.elem Circle
              [ v3Attr "cx" (lit 250.0)
              , v3Attr "cy" (lit 50.0)
              , v3Attr "r" (lit 20.0)
              , v3AttrStr "fill" (str "blue")
              ]
          ])

-- | Simple bar chart with data join
simpleBarChart :: T.Tree Int
simpleBarChart =
  let barData = [10, 25, 15, 30, 20]
  in T.named SVG "svg"
       [ v3Attr "width" (lit 400.0)
       , v3Attr "height" (lit 200.0)
       , v3AttrStr "viewBox" (str "0 0 400 200")
       ]
       `T.withChild`
         (T.joinData "bars" "rect" barData $ \_ ->
           T.elem Rect
             [ v3AttrFnI "x" ((\ (_ :: Int) i -> toNumber i * 70.0 + 20.0) :: Int -> Int -> Number)
             , v3AttrFnI "y" ((\ (value :: Int) _ -> 180.0 - toNumber value * 5.0) :: Int -> Int -> Number)
             , v3Attr "width" (lit 50.0)
             , v3AttrFnI "height" ((\ (value :: Int) _ -> toNumber value * 5.0) :: Int -> Int -> Number)
             , v3AttrStr "fill" (str "steelblue")
             ])

-- | Simple bar chart without data join (for interpreter demos)
simpleBarChartNoJoin :: T.Tree Unit
simpleBarChartNoJoin =
  T.named SVG "svg"
    [ v3Attr "width" (lit 400.0)
    , v3Attr "height" (lit 200.0)
    , v3AttrStr "viewBox" (str "0 0 400 200")
    , v3AttrStr "class" (str "bar-chart")
    ]
    `T.withChild`
      (T.named Group "bars"
        [ v3AttrStr "class" (str "bars-group")
        ]
        `T.withChildren`
          [ T.elem Rect
              [ v3Attr "x" (lit 20.0)
              , v3Attr "y" (lit 130.0)
              , v3Attr "width" (lit 50.0)
              , v3Attr "height" (lit 50.0)
              , v3AttrStr "fill" (str "steelblue")
              ]
          , T.elem Rect
              [ v3Attr "x" (lit 90.0)
              , v3Attr "y" (lit 55.0)
              , v3Attr "width" (lit 50.0)
              , v3Attr "height" (lit 125.0)
              , v3AttrStr "fill" (str "steelblue")
              ]
          , T.elem Rect
              [ v3Attr "x" (lit 160.0)
              , v3Attr "y" (lit 105.0)
              , v3Attr "width" (lit 50.0)
              , v3Attr "height" (lit 75.0)
              , v3AttrStr "fill" (str "steelblue")
              ]
          , T.elem Rect
              [ v3Attr "x" (lit 230.0)
              , v3Attr "y" (lit 30.0)
              , v3Attr "width" (lit 50.0)
              , v3Attr "height" (lit 150.0)
              , v3AttrStr "fill" (str "steelblue")
              ]
          , T.elem Rect
              [ v3Attr "x" (lit 300.0)
              , v3Attr "y" (lit 80.0)
              , v3Attr "width" (lit 50.0)
              , v3Attr "height" (lit 100.0)
              , v3AttrStr "fill" (str "steelblue")
              ]
          ])

-- | Nested structure example
nestedStructure :: T.Tree Unit
nestedStructure =
  T.named SVG "svg"
    [ v3Attr "width" (lit 200.0)
    , v3Attr "height" (lit 200.0)
    , v3AttrStr "viewBox" (str "0 0 200 200")
    ]
    `T.withChild`
      (T.named Group "outer"
        []
        `T.withChild`
          (T.named Group "inner"
            []
            `T.withChildren`
              [ T.elem Circle
                  [ v3Attr "cx" (lit 100.0)
                  , v3Attr "cy" (lit 100.0)
                  , v3Attr "r" (lit 50.0)
                  , v3AttrStr "fill" (str "purple")
                  ]
              , T.elem Text
                  [ v3Attr "x" (lit 100.0)
                  , v3Attr "y" (lit 105.0)
                  , v3AttrStr "textContent" (str "Hello!")
                  , v3AttrStr "fill" (str "white")
                  ]
              ]))
