module Main where

import Prelude

import D3.Examples.GUP (enter, update) as GUP
import D3.Interpreter.Tagless (d3Run, runD3M)
import D3.Selection (D3State(..), emptyD3Selection, makeD3State, makeD3State')
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

initialState = makeD3State' [ 'a', 'b', 'c', 'd' ]

main :: Effect Unit
main = launchAff_  do
  (D3State _ circles) <- liftEffect $ liftA1 snd $ runD3M GUP.enter initialState
  _ <- delay  $ Milliseconds 5000.0
  _ <- liftEffect $ runD3M GUP.update (makeD3State [ 'a', 'c', 'd', 'f', 'z' ] circles)
  liftEffect $ log "🍝"
