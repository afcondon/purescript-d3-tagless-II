type Model = Array Int  -- not strictly necessary in such a simple example, of course

datum_ ::               -- a record containing all the accessor functions needed for attributes
  { color :: Datum_ -> String
  , x :: Datum_ -> Index_ -> Number
  , y :: Datum_ -> Number
  }
datum_ =
  let 
    -- we bury the unsafe functions inside the datum_ record, unsafeCoerce yes, but very restricted how it can be used
    -- in this simplest of examples, the index is also the datum itself
    getDatum :: Datum_ -> Int 
    getDatum = unsafeCoerce
  in {
    x :     \_ i -> (index_ToNumber i) * 20.0
  , y :     \d   -> 100.0 - (toNumber $ getDatum d) / 5.0
  , color : \d   -> d3SchemePairedN_ ((toNumber $ getDatum d) / 100.0)
}
