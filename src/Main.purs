module Main where

import Prelude

import D3.Examples.GUP (enter, update) as GUP
import D3.Interpreter.Tagless (d3Run, runD3M)
import D3.Selection (D3State(..), EasingFunction(..), Transition, emptyD3Selection, makeD3State, makeD3State')
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

initialState = makeD3State' [ 'a', 'b', 'c', 'd' ]

t :: Transition
t = { name: "", delay: 0, duration: 500, easing: DefaultCubic }

main :: Effect Unit
main = launchAff_  do
  (D3State _ circles) <- liftEffect $ liftA1 snd $ runD3M GUP.enter initialState
  _ <- liftEffect $ runD3M (GUP.update t) (makeD3State [ 'a', 'b', 'c', 'd' ] circles)
  _ <- delay  $ Milliseconds 1000.0
  _ <- liftEffect $ runD3M (GUP.update t) (makeD3State [ 'a', 'c', 'd', 'f', 'z' ] circles)
  _ <- delay  $ Milliseconds 1000.0
  _ <- liftEffect $ runD3M (GUP.update t) (makeD3State [ 'c', 'd', 'f', 'p', 's', 'z' ] circles)
  liftEffect $ log "🍝"
