module D3.Examples.GUP where

import D3.Attributes.Instances
import D3.Attributes.Sugar
import Prelude

import Control.Monad.State (class MonadState, get)
import D3.Interpreter.Tagless (class D3Tagless, Keys(..), append, hook, join)
import D3.Selection (D3Selection, D3State(..), D3_Node(..), EasingFunction(..), Element(..), Transition, TransitionStage(..), node__)
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

enterInitial :: Attributes
enterInitial = [
    classed "enter1"
  , fill "black"
  , x $ offsetXByIndex
  , y 0.0
  , text textFromDatum
  , fontSize 48.0
]

enterFinal :: Attributes
enterFinal = [
    classed "enter2"
  , fill "black"
  , y 500.0
]
updateInitial :: Attributes
updateInitial = [ classed "update2", fill "green" ]

updateFinal :: Array Attribute
updateFinal = [ classed "update1", x $ offsetXByIndex ]

exitInitial :: Array Attribute
exitInitial = [ classed "exit1", fill "red" ]

exitFinal :: Array Attribute
exitFinal = [ y 900.0
            ]

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
  joinSelection <- join Text DatumIsKey letters { enter: [ AttrsAndTransition enterInitial t
                                                          , OnlyAttrs enterFinal
                                                          ]
                                                , update: [ AttrsAndTransition updateInitial t
                                                          , OnlyAttrs updateFinal
                                                          ]
                                                , exit: [ AttrsAndTransition exitInitial t
                                                        , OnlyAttrs exitFinal
                                                        ] }

  pure joinSelection
