module D3.Data.Transition where

import D3.Data.Foreign (D3DomNode_, D3This_, Datum_)
import Data.Time.Duration (Milliseconds)

-- TODO make this a Newtype and give it monoid instance
type Transition = { name     :: String
                  , delay    :: Milliseconds-- can also be a function, ie (\d -> f d)
                  , duration :: Milliseconds -- can also be a function, ie (\d -> f d)
                  , easing   :: EasingFunction
}
type D3Group_ = Array D3DomNode_


type EasingTime = Number
type D3EasingFn = EasingTime -> EasingTime -- easing function maps 0-1 to 0-1 in some way with 0 -> 0, 1 -> 1
data EasingFunction = 
    DefaultCubic
  | EasingFunction D3EasingFn
  | EasingFactory (Datum_ -> Int -> D3Group_ -> D3This_ -> D3EasingFn)
