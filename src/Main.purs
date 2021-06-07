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
import Halogen.HTML.Properties as HP
import Halogen.Storybook (Stories, runStorybook, proxy)
import Stories.GUP as D3GUP
import Stories.LesMis as LesMis
import Stories.MetaTree as MetaTree
import Stories.PrintTree as PrintTree
import Stories.Spago as Spago
import Stories.Trees as Trees


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
logo = HH.img
            [ HP.class_ $ HH.ClassName "logo"
            , HP.src "PSD3-logo.png"
            ]
        

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>= runStorybook
    { stories
    , logo: Just logo
    }
