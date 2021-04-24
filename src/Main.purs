module Main where

import Prelude

import D3.Examples.Force as Graph
import D3.Examples.GUP (runGeneralUpdatePattern) as GUP
import D3.Examples.Tree.Radial as RadialTree
import D3.Layouts.Hierarchical (getTreeViaAJAX, initRadialTree, makeModel)
import Data.Either (Either(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (forkAff, launchAff_)

main :: Effect Unit
main = launchAff_  do
  _        <- forkAff GUP.runGeneralUpdatePattern
  _        <- forkAff Graph.drawGraph
  treeJSON <- getTreeViaAJAX "http://localhost:1234/flare-2.json"

  _ <- case treeJSON of
          (Left error) -> pure unit
          (Right json) -> RadialTree.drawTree =<< makeModel initRadialTree json

  json <- RadialTree.getMetaTreeJSON
  let fuux = spy "json: " json
  model <- makeModel initRadialTree json
  let quux = spy "model: " model
  baz <- RadialTree.drawTree model 

  pure unit
