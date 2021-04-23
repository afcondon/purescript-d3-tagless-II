module Main where

import Prelude

import D3.Examples.Force as Graph
import D3.Examples.GUP (runGeneralUpdatePattern) as GUP
import D3.Examples.Tree.Horizontal as HorizontalTree
import D3.Examples.Tree.Radial as RadialTree
import Data.Bifunctor (rmap)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (forkAff, launchAff_)

main :: Effect Unit
main = launchAff_  do
  _             <- forkAff GUP.runGeneralUpdatePattern
  _             <- forkAff Graph.drawGraph
  treeModel     <- RadialTree.getTreeViaAJAX
  treeMetaModel <- RadialTree.getMetaTree
  sequence_ $ rmap RadialTree.drawTree treeModel
  sequence_ $ rmap RadialTree.printTree treeModel
  sequence_ $ rmap RadialTree.drawTree treeMetaModel
  -- _          <- HorizontalTree.drawTree
  pure unit
