module Main where

import Prelude

import D3.Examples.Force as Graph
import D3.Examples.GUP (runGeneralUpdatePattern) as GUP
import D3.Examples.Tree.Cluster as Cluster
import D3.Examples.Tree.Horizontal as Tidy
import D3.Examples.Tree.Radial as Radial
import D3.Layouts.Hierarchical (TreeJson_, getTreeViaAJAX, makeModel)
import D3.Layouts.Hierarchical.Types (TreeLayout(..), TreeType(..))
import Data.Bifunctor (rmap)
import Data.Foldable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff_)

drawUsingRadialLayout :: TreeJson_ -> Aff Unit
drawUsingRadialLayout json = Radial.drawTree =<< makeModel TidyTree Radial json

drawUsingHorizontalLayout :: TreeJson_ -> Aff Unit
drawUsingHorizontalLayout json = Tidy.drawTree =<< makeModel TidyTree Horizontal json

drawUsingClusterLayout :: TreeJson_ -> Aff Unit
drawUsingClusterLayout json = Cluster.drawTree =<< makeModel Dendrogram Horizontal json

drawMetaTree :: TreeJson_ -> Aff Unit
drawMetaTree json = Tidy.drawTree =<< makeModel TidyTree Horizontal =<< Radial.getMetaTreeJSON =<< makeModel TidyTree Radial json


main :: Effect Unit
main = launchAff_  do
  _        <- forkAff GUP.runGeneralUpdatePattern
  _        <- forkAff Graph.drawGraph

  -- fetch an example model for the tree examples, the canonical flare dependency json in this case
  treeJSON <- getTreeViaAJAX "http://localhost:1234/flare-2.json"
  -- draw a radial tree using the flare data
  -- sequence_ $ rmap drawUsingRadialLayout treeJSON
  -- sequence_ $ rmap drawUsingHorizontalLayout treeJSON
  sequence_ $ rmap drawUsingClusterLayout treeJSON
  -- extract the structure of the radial tree "D3 script" and draw a radial tree of this "meta" tree
  -- sequence_ $ rmap drawMetaTree treeJSON

  pure unit
