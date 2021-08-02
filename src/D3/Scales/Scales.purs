module D3.Scales where

-- TODO much more to be ported over / wrapped here

--            COLOR & SCALE functions
-- gross simplification here, scales can take ranges and allsorts
-- we just want to be able to pass d3.schemeCategory10 back in from Purescript to prove the idea tho RN

type ScaleNumeric_ = Number -> String
type ScaleString_  = String -> String -- REVIEW this might need coercion?

foreign import d3SchemeCategory10N_  :: ScaleNumeric_ -- not modelling the scale / domain distinction yet
foreign import d3SchemeDiverging10N_ :: ScaleNumeric_ -- not modelling the scale / domain distinction yet
foreign import d3SchemeCategory10S_  :: ScaleString_ -- not modelling the scale / domain distinction yet
