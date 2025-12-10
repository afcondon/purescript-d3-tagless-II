type Model = Array Char

-- in the interests of brevity these unsafe functions are defined here with the "script"
-- however, in a larger program both Model and Unsafe would be their own modules
datumIsChar :: Datum_ -> Char
datumIsChar = unsafeCoerce

indexIsNumber :: Index_ -> Number
indexIsNumber = unsafeCoerce

keyFunction :: Datum_ -> Index_ -- for this very simple example, the data (Char) can be used directly as the key
keyFunction = unsafeCoerce

exGeneralUpdatePattern :: forall m. SelectionM D3Selection_ m => Selector D3Selection_-> m ((Array Char) -> m D3Selection_)
exGeneralUpdatePattern selector = do 
  root           <- attach selector
  svg            <- appendTo root Svg [ viewBox 0.0 100.0 800.0 350.0, classed "d3svg gup" ]
  letterGroup    <- appendTo svg Group []
  
  pure $ \letters -> do
    enterSelection   <- openSelection letterGroup "text"
    updateSelections <- updateJoin enterSelection Text letters keyFunction
    setAttributes updateSelections.exit exit
    setAttributes updateSelections.update update

    newlyEntered     <- appendTo updateSelections.enter Text []
    setAttributes newlyEntered enter
    
    pure newlyEntered

  where 
    transition :: SelectionAttribute
    transition = transitionWithDuration $ Milliseconds 2000.0

    xFromIndex :: Datum_ -> Index_ -> Number
    xFromIndex _ i = 50.0 + ((indexIsNumber i) * 48.0) -- letters enter at this position, and then must transition to new position on each update

    enter = [ classed  "enter"
            , fill     "green"
            , x        xFromIndex
            , y        0.0
            , text     (singleton <<< datumIsChar)
            , fontSize 60.0 ]  
          `andThen` (transition `to` [ y 200.0 ]) 

    update =  [ classed "update", fill "gray", y 200.0 ] 
              `andThen` (transition `to` [ x xFromIndex ] ) 

    exit =  [ classed "exit", fill "brown"] 
            `andThen` (transition `to` [ y 400.0, remove ])
