module Main where

import Prelude

import D3.Examples.Force as Graph
import D3.Examples.GUP (runGeneralUpdatePattern) as GUP
import D3.Examples.Tree.Horizontal as HorizontalTree
import D3.Examples.Tree.Radial as RadialTree
import D3.Layouts.Hierarchical (TreeConfig(..), TreeJson_, getTreeViaAJAX, initHorizontalTree, initRadialTree, makeModel)
import Data.Bifunctor (rmap)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff_)

drawTree :: TreeJson_ -> Aff Unit
drawTree json = RadialTree.drawTree =<< makeModel initRadialTree json

drawMetaTree :: TreeJson_ -> Aff Unit
drawMetaTree json = HorizontalTree.drawTree =<< makeModel initHorizontalTree =<< RadialTree.getMetaTreeJSON =<< makeModel initRadialTree json


main :: Effect Unit
main = launchAff_  do
  _        <- forkAff GUP.runGeneralUpdatePattern
  _        <- forkAff Graph.drawGraph

  -- fetch an example model for the tree examples, the canonical flare dependency json in this case
  treeJSON <- getTreeViaAJAX "http://localhost:1234/flare-2.json"
  -- draw a radial tree using the flare data
  sequence_ $ rmap drawTree treeJSON
  -- extract the structure of the radial tree "D3 script" and draw a radial tree of this "meta" tree
  sequence_ $ rmap drawMetaTree treeJSON

  pure unit
