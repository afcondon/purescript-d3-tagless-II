module D3.Examples.ThreeLittleCircles where

import D3.Attributes.Sugar

import D3.Data.Types (D3Selection_, Datum_, Element(..), Index_, Selector)
import D3.FFI (keyIsID_)
import D3.Scales (d3SchemeCategory10N_)
import D3Tagless.Capabilities (class SelectionM, appendTo, attach, setAttributes, simpleJoin)
import Data.Int (toNumber)
import Math as Math
import Prelude (discard, bind, negate, pure, ($), (/), (*))
import Unsafe.Coerce (unsafeCoerce)

-- | simple utility function used in all three of these examples
xFromIndex :: Datum_ -> Index_ -> Number
xFromIndex _ i = ((indexIsNumber i) * 100.0)
  where
    indexIsNumber :: Index_ -> Number
    indexIsNumber = unsafeCoerce


-- | Pretty much the most basic example imaginable, three ints represented by three circles
threeLittleCircles :: forall m. SelectionM D3Selection_ m => Selector D3Selection_-> m D3Selection_
threeLittleCircles selector = do 

  let circleAttributes = [ fill "green", cx xFromIndex, cy 50.0, radius 20.0 ]

  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox (-100.0) (-100.0) 650.0 650.0, classed "d3svg gup" ]
  circleGroup <- appendTo svg  Group []
  circles     <- simpleJoin circleGroup Circle [32, 57, 293] keyIsID_ 
  setAttributes circles circleAttributes

  pure circles



-- | Passing in the data
threeLittleCircles2 :: forall m. SelectionM D3Selection_ m => Array Int -> Selector D3Selection_-> m D3Selection_
threeLittleCircles2 circleData selector = do 

  let circleAttributes = [ fill "green", cx xFromIndex, cy 50.0, radius 20.0 ]

  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox (-100.0) (-100.0) 650.0 650.0, classed "d3svg gup" ]
  circleGroup <- appendTo svg  Group []

  circles     <- simpleJoin circleGroup Circle circleData keyIsID_ 
  setAttributes circles circleAttributes

  pure circles




-- | finally, using the data (as opposed to merely the index) in the visualization  
type Model = Array Int  -- not necessary in such a simple example, of course

getDatum :: Datum_ -> Int
getDatum = unsafeCoerce

datum_ :: { color :: Datum_ -> String
, radius :: Datum_ -> Number
}
datum_ = {
    radius: \d -> Math.sqrt $ toNumber $ getDatum d
  , color : \d -> d3SchemeCategory10N_ ((toNumber $ getDatum d) / 100.0)
}

threeLittleCircles3 :: forall m. SelectionM D3Selection_ m => Model -> Selector D3Selection_-> m D3Selection_
threeLittleCircles3 circleData selector = do 

  let circleAttributes = [ fill datum_.color, cx xFromIndex, cy 50.0, radius datum_.radius ]

  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox (-100.0) (-100.0) 650.0 650.0, classed "d3svg gup" ]
  circleGroup <- appendTo svg  Group []

  circles     <- simpleJoin circleGroup Circle circleData keyIsID_ 
  setAttributes circles circleAttributes

  pure circles
