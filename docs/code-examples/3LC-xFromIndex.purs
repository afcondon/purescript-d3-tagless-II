-- | simple utility function used in all three of these examples
xFromIndex :: Datum_ -> Index_ -> Number
xFromIndex _ i = ((indexIsNumber i) * 30.0) + 10.0
  where
    indexIsNumber :: Index_ -> Number
    indexIsNumber = unsafeCoerce
