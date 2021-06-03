module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Stories.Count as ExpCount
import Stories.GUP as D3GUP
import Stories.Index as ExpIndex
import Stories.Input as ExpInput
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories, runStorybook, proxy)
import D3.Examples.LesMiserables as LesMis
import D3.Examples.Spago as Spago
import D3.Examples.GUP (runGeneralUpdatePattern) as GUP
import D3.Examples.Tree.Configure as Tree
import D3.Examples.MetaTree as MetaTree
import D3.Layouts.Hierarchical (getTreeViaAJAX, makeModel)
import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeType(..))
import Data.Bifunctor (rmap)
import Data.Foldable (sequence_)
import Effect.Aff (Aff, forkAff, launchAff_)

drawMetaTree :: TreeJson_ -> Aff Unit
drawMetaTree json =
  MetaTree.drawTree =<< makeModel TidyTree Vertical =<< Tree.getMetaTreeJSON =<< makeModel TidyTree Radial json

ddi :: Effect Unit
ddi = launchAff_  do
  _        <- forkAff Spago.drawGraph
  
-- {-
  _        <- forkAff LesMis.drawGraph

  _        <- forkAff GUP.runGeneralUpdatePattern

  -- fetch an example model for the tree examples, the canonical flare dependency json in this case
  treeJSON <- getTreeViaAJAX "http://localhost:1234/flare-2.json"

  sequence_ $ rmap (\json -> Tree.drawTree =<< makeModel Dendrogram Horizontal json) treeJSON
  sequence_ $ rmap (\json -> Tree.drawTree =<< makeModel Dendrogram Vertical json)   treeJSON
  sequence_ $ rmap (\json -> Tree.drawTree =<< makeModel Dendrogram Radial json)     treeJSON
  sequence_ $ rmap (\json -> Tree.drawTree =<< makeModel TidyTree Horizontal json)   treeJSON
  sequence_ $ rmap (\json -> Tree.drawTree =<< makeModel TidyTree Vertical json)     treeJSON
  sequence_ $ rmap (\json -> Tree.drawTree =<< makeModel TidyTree Radial json)       treeJSON

  sequence_ $ rmap (\json -> Tree.printTree =<< makeModel TidyTree Radial json)       treeJSON

  -- extract the structure of the radial tree "D3 script" and draw a radial tree of this "meta" tree
  sequence_ $ rmap drawMetaTree treeJSON
-- -}

  pure unit


stories :: forall m. Stories m
stories = Object.fromFoldable
  [ Tuple "" $ proxy ExpIndex.component
  , Tuple "D3" $ proxy D3GUP.component
  , Tuple "count" $ proxy ExpCount.component
  , Tuple "Form|input" $ proxy ExpInput.component
  ]

logo :: HH.PlainHTML
logo = HH.text "PureScript Data Driven Interfaces"

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>= runStorybook
    { stories
    , logo: Just logo
    }
