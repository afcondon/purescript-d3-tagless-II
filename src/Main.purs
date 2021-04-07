module Main where

import D3.Examples.Force as Graph
import D3.Examples.GUP (runGeneralUpdatePattern) as GUP
import D3.Examples.Tree as Tree
import Effect (Effect)
import Effect.Aff (forkAff, launchAff_)
import Prelude (Unit, pure, unit, bind)

main :: Effect Unit
main = launchAff_  do
  -- _ <- forkAff GUP.runGeneralUpdatePattern
  -- _ <-         Tree.drawTree
  _ <- forkAff Graph.drawGraph

  pure unit
