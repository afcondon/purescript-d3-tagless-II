module D3.Scales where

-- TODO much more to be ported over / wrapped here

--            COLOR & SCALE functions
-- gross simplification here, scales can take ranges and allsorts
-- we just want to be able to pass d3.schemeCategory10 back in from Purescript to prove the idea tho RN

type Scale = Number -> String 

foreign import d3SchemeCategory10_ :: Scale -- not modelling the scale / domain distinction yet
