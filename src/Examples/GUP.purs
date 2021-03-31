module D3.Examples.GUP where

import D3.Attributes.Instances
import D3.Attributes.Sugar
import Prelude

import Control.Monad.State (class MonadState, get)
import D3.Interpreter.Tagless (class D3Tagless, Keys(..), append, hook, join)
import D3.Selection (D3Selection, D3State(..), D3_Node(..), EasingFunction(..), Element(..), TransitionStage(..), Transition, node__)
import Data.Char (toCharCode)
import Data.Int (toNumber)
import Data.String.CodeUnits (singleton)
import Unsafe.Coerce (unsafeCoerce)


-- simple attributes don't use data, no hassle
svgAttributes :: Attributes
svgAttributes = [
    strokeColor "green"
  , strokeOpacity 0.75
  , width 1000.0
  , height 1000.0
  , viewBox (-500.0) (-500.0) 1000.0 1000.0
]

-- attributes that use (\d -> <something>) or (\d i -> <something>) need type hints
-- and coercion functions
type Model = Array Char

offsetXByIndex :: Datum -> Index -> Number
offsetXByIndex d i = offset + ((indexIsNumber i) * factor)
  where
    offset = 500.0
    factor = 48.0

textFromDatum :: Datum -> String
textFromDatum = singleton <<< datumIsChar

enterAttributes1 :: Attributes
enterAttributes1 = [
    classed "enter1"
  , fill "black"
  , x $ offsetXByIndex
  , y 0.0
  , text textFromDatum
  , fontSize 48.0
]

enterAttributes2 :: Attributes
enterAttributes2 = [
    classed "enter2"
  , fill "black"
  , y 500.0
]

updateAttributes1 :: Array Attribute
updateAttributes1 = [ classed "update1", x $ offsetXByIndex ]

updateAttributes2 :: Attributes
updateAttributes2 = [ classed "update2", fill "green" ]

exitAttributes1 :: Array Attribute
exitAttributes1 = [ classed "exit1"
                  , fill "red"
                  , y 900.0 ]

-- TODO we can actually potentially return _much_ more structured output, selection tree etc
enter :: ∀ m. (D3Tagless m) => m D3Selection 
enter = do -- modelData is already in stateT   
  root <- hook "div#root"
  svg  <- append $ D3_Node Svg svgAttributes []
  append $ node__ Group

-- TODO we can actually return much more structured output, selection tree etc
update :: forall m. Bind m => MonadState D3State m => D3Tagless m => Transition -> m D3Selection
update t = do
  (D3State _ letters) <- get  
  joinSelection <- join Text DatumIsKey letters { enter: [ AttrsAndTransition enterAttributes1 t
                                                          , OnlyAttrs enterAttributes2
                                                          ]
                                                , update: [ AttrsAndTransition updateAttributes2 t
                                                          , OnlyAttrs updateAttributes1
                                                          ]
                                                , exit: [ AttrsAndTransition exitAttributes1 t
                                                        ] }

  pure joinSelection
