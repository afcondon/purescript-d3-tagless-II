module D3.Examples.GUP where

import D3.Attributes.Sugar

import Control.Monad.State (class MonadState, get)
import D3.Attributes.Instances (Attribute, Attributes, Datum, datumIsChar)
import D3.Interpreter.Tagless (class D3Tagless, append, hook, join)
import D3.Selection (D3Selection, D3State(..), D3_Node(..), EasingFunction(..), Element(..), Transition, TransitionStage(..), node__)
import Data.Char (toCharCode)
import Data.Int (toNumber)
import Data.String.CodeUnits (singleton)
import Prelude (class Bind, bind, pure, show, ($), (*), (+), (-), (<<<))
import Unsafe.Coerce (unsafeCoerce)


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

offsetXByIndex :: Number -> Number -> Datum -> Int -> Number
offsetXByIndex offset factor d i = offset + (toNumber i) * factor

circleRadius :: Datum -> Number
circleRadius = toNumber <<< (_ * 10) <<< (_ - 96) <<< toCharCode <<< datumIsChar

circleStrokeWidth :: Datum -> Number
circleStrokeWidth = toNumber <<< (_ * 2) <<< (_ - 97) <<< toCharCode <<< datumIsChar

enterAttributes1 :: Attributes
enterAttributes1 = [
    classed "enter1"
  , fill "black"
  , x $ offsetXByIndex 200.0 20.0
  , y 0.0
  , text (\(d :: Datum) -> singleton (datumIsChar d) ) -- singleton $ unsafeCoerce $ datumIsChar d) -- TODO gross, needs fixing
  -- , strokeColor "blue"
  -- , strokeOpacity 0.25
  -- , strokeWidth circleStrokeWidth
  -- , r circleRadius
  -- , cx offsetXByIndex
  -- , cy 100.0
]

t :: Transition
t = { name: "", delay: 1000, duration: 1000, easing: DefaultCubic }

enterAttributes2 :: Attributes
enterAttributes2 = [
    fill "black"
  , y 100.0
]

updateAttributes1 :: Array Attribute
updateAttributes1 = [ classed "update1", x $ offsetXByIndex 200.0 20.0 ]

updateAttributes2 :: Attributes
updateAttributes2 = [ fill "green" ]

exitAttributes1 :: Array Attribute
exitAttributes1 = [ classed "exit1"
                  , fill "red"
                  , y 200.0 ]

-- TODO we can actually potentially return _much_ more structured output, selection tree etc
enter :: ∀ m. (D3Tagless m) => m D3Selection 
enter = do -- modelData is already in stateT   
  root <- hook "div#root"
  
  svg  <- append $ D3_Node Svg svgAttributes []

  letters <- append $ node__ Group
  -- now the active selection is "letters" and these letters that are joining will be inside it
  -- TODO we need to call "update" from here so as not to repeat this code
  joinSelection <- join Text letters { enter: [ AttrsAndTransition enterAttributes1 t
                                              , OnlyAttrs enterAttributes2
                                              ]
                                     , update: [ AttrsAndTransition updateAttributes1 t
                                               , OnlyAttrs updateAttributes2
                                               ]
                                     , exit: [ AttrsAndTransition exitAttributes1 t
                                            --  , OnlyAttrs exitAttributes2
                                             ] }

  pure letters


-- TODO we can actually return much more structured output, selection tree etc
update :: forall m. Bind m => MonadState D3State m => D3Tagless m => m D3Selection
update = do
  (D3State d letters) <- get  
  joinSelection <- join Text letters { enter: [ AttrsAndTransition enterAttributes1 t
                                              , OnlyAttrs enterAttributes2
                                              ]
                                     , update: [ AttrsAndTransition updateAttributes1 t
                                               , OnlyAttrs updateAttributes2
                                               ]
                                     , exit: [ AttrsAndTransition exitAttributes1 t
                                            --  , OnlyAttrs exitAttributes2
                                             ] }

  pure joinSelection
