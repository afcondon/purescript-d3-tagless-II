module D3.Examples.GUP where

import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Attribute, Attributes, Datum, datumIsChar)
import D3.Attributes.Sugar (classed, cx, cy, fill, height, r, strokeColor, strokeOpacity, strokeWidth, width)
import D3.Interpreter.Tagless (class D3Tagless, append, hook, join)
import D3.Selection (D3Selection, D3State(..), D3_Node(..), EasingFunction(..), Element(..), Transition, TransitionStage(..), node__)
import Data.Char (toCharCode)
import Data.Int (toNumber)
import Prelude (class Bind, bind, pure, ($), (*), (+), (-), (<<<))


-- simple attributes don't use data, no hassle
svgAttributes :: Attributes
svgAttributes = [
    strokeColor "green"
  , strokeOpacity 0.75
  , width 1000.0
  , height 1000.0
  -- , viewBox 0.0 0.0 1000.0 1000.0
]

-- attributes that use (\d -> <something>) or (\d i -> <something>) need type hints
-- and coercion functions
type Model = Array Char

circlePositionX :: Datum -> Int -> Number
circlePositionX d i = 200.0 + (toNumber i) * 200.0

circleRadius :: Datum -> Number
circleRadius = toNumber <<< (_ * 10) <<< (_ - 96) <<< toCharCode <<< datumIsChar

circleStrokeWidth :: Datum -> Number
circleStrokeWidth = toNumber <<< (_ * 2) <<< (_ - 97) <<< toCharCode <<< datumIsChar

enterAttributes :: Attributes
enterAttributes = [
    classed "enter1"
  , strokeColor "blue"
  , strokeOpacity 0.25
  , fill "yellow"
  , strokeWidth circleStrokeWidth
  , r circleRadius
  , cx circlePositionX
  , cy 100.0
]

t :: Transition
t = { name: "", delay: 1000, duration: 1000, easing: DefaultCubic }

enterAttributes2 :: Attributes
enterAttributes2 = [
    strokeColor "red"
  , strokeOpacity 0.85
  , strokeWidth circleStrokeWidth
]

updateAttributes1 :: Array Attribute
updateAttributes1 = [ classed "update1"
                    , strokeColor "green" ]

updateAttributes2 :: Attributes
updateAttributes2 = [ strokeColor "purple"
                    , cx circlePositionX ]


-- TODO we can actually potentially return _much_ more structured output, selection tree etc
enter :: ∀ m. (D3Tagless m) => m D3Selection 
enter = do -- modelData is already in stateT   
  root <- hook "div#root"
  
  svg  <- append $ D3_Node Svg svgAttributes []

  circles <- append $ node__ Group
  -- now the active selection is "circles" and these circles that are joining will be inside it
  _ <- join Circle circles { enter: [ AttrsAndTransition (enterAttributes ) t
                                    , OnlyAttrs (enterAttributes2 )
                                    ]
                          , update: [ AttrsAndTransition updateAttributes1 t
                                    , OnlyAttrs (updateAttributes2 )
                                    ]
                          , exit: [] }

  pure circles


-- TODO we can actually return much more structured output, selection tree etc
update :: forall m. Bind m => MonadState D3State m => D3Tagless m => m D3Selection
update = do
  (D3State d circles) <- get  
  joinSelection <- join Circle circles { enter: [ AttrsAndTransition enterAttributes t
                                                , OnlyAttrs enterAttributes2
                                                ]
                                       , update: [ AttrsAndTransition updateAttributes1 t
                                                 , OnlyAttrs (updateAttributes2 )
                                                 ]
                                       , exit: [] }

  pure joinSelection
