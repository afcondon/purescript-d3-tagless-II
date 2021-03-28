module Main where

import D3.Interpreter.Tagless (d3Run)

import D3.Examples.GUP (script) as GUP
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, bind)

main :: Effect Unit
main = do
  _ <- d3Run GUP.script
  log "🍝"
