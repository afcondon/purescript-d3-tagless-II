module Main where

import Prelude

import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeType(..))
import D3.Examples.Tree.Configure as Tree
import D3.Layouts.Hierarchical (makeModel)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories, runStorybook, proxy)
import Stories.GUP as D3GUP
import Stories.LesMis as LesMis
import Stories.MetaTree as MetaTree
import Stories.PrintTree as PrintTree
import Stories.Trees as Trees
import Stories.Spago as Spago


stories :: forall m. (MonadAff m) => Stories m
stories = Object.fromFoldable
  [ Tuple ""           $ proxy Trees.component
  , Tuple "GUP"        $ proxy D3GUP.component
  , Tuple "LesMis"     $ proxy LesMis.component
  , Tuple "Trees"      $ proxy Trees.component
  , Tuple "Meta-Tree"  $ proxy MetaTree.component
  , Tuple "Print-Tree" $ proxy PrintTree.component
  , Tuple "Spago"      $ proxy Spago.component
  ]

logo :: HH.PlainHTML
logo = HH.text "Data Driven Interfaces in PureScript"

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>= runStorybook
    { stories
    , logo: Just logo
    }
