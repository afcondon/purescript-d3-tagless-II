module Main where

import Prelude

import D3.Examples.GUP (script) as GUP
import D3.Interpreter.Tagless (d3Run, runD3M)
import D3.Selection (D3State(..), emptyD3Selection, makeD3State)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

initialState = makeD3State [ 'a', 'b', 'c', 'd' ]
secondState  = makeD3State [ 'a', 'c', 'd', 'f', 'z' ] 

main :: Effect Unit
main = launchAff_  do
  _ <- liftEffect $ runD3M GUP.script initialState
  _ <- delay  $ Milliseconds 5000.0
  _ <- liftEffect $ runD3M GUP.script secondState
  liftEffect $ log "🍝"
