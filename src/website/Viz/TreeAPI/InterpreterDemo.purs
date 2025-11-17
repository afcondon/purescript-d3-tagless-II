module D3.Viz.TreeAPI.InterpreterDemo where

-- | Simple example visualizations for demonstrating alternative interpreters

import Prelude

import Data.Int (toNumber)
import PSD3v2.Attribute.Types (cx, cy, fill, height, radius, viewBox, width, x, y, textContent, class_)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3v2.VizTree.Tree as T

-- | Three Little Circles - simple and familiar
threeLittleCircles :: T.Tree Unit
threeLittleCircles =
  T.named SVG "svg"
    [ width 300.0
    , height 100.0
    , viewBox "0 0 300 100"
    ]
    `T.withChild`
      (T.named Group "circles"
        [ class_ "circles-group"
        ]
        `T.withChildren`
          [ T.elem Circle
              [ cx 50.0
              , cy 50.0
              , radius 20.0
              , fill "red"
              ]
          , T.elem Circle
              [ cx 150.0
              , cy 50.0
              , radius 20.0
              , fill "green"
              ]
          , T.elem Circle
              [ cx 250.0
              , cy 50.0
              , radius 20.0
              , fill "blue"
              ]
          ])

-- | Simple bar chart with data join
simpleBarChart :: T.Tree Int
simpleBarChart =
  let barData = [10, 25, 15, 30, 20]
  in T.named SVG "svg"
       [ width 400.0
       , height 200.0
       , viewBox "0 0 400 200"
       ]
       `T.withChild`
         (T.joinData "bars" "rect" barData $ \_ ->
           T.elem Rect
             [ x ((\ (_ :: Int) i -> toNumber i * 70.0 + 20.0) :: Int -> Int -> Number)
             , y ((\ (value :: Int) _ -> 180.0 - toNumber value * 5.0) :: Int -> Int -> Number)
             , width 50.0
             , height ((\ (value :: Int) _ -> toNumber value * 5.0) :: Int -> Int -> Number)
             , fill "steelblue"
             ])

-- | Nested structure example
nestedStructure :: T.Tree Unit
nestedStructure =
  T.named SVG "svg"
    [ width 200.0
    , height 200.0
    , viewBox "0 0 200 200"
    ]
    `T.withChild`
      (T.named Group "outer"
        []
        `T.withChild`
          (T.named Group "inner"
            []
            `T.withChildren`
              [ T.elem Circle
                  [ cx 100.0
                  , cy 100.0
                  , radius 50.0
                  , fill "purple"
                  ]
              , T.elem Text
                  [ x 100.0
                  , y 105.0
                  , textContent "Hello!"
                  , fill "white"
                  ]
              ]))
