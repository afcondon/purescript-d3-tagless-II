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
