module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Stories.Tailwind.Styles as Tailwind
import UIGuide.Block.Backdrop (backdrop) as Backdrop

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = ShowGUP | ShowTrees | ShowLesMis | ShowMetaTree | ShowPrinter | ShowSpago

type State = String

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: forall input. input -> State
  initialState _ = "empty"

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.body_
    [ HH.div [ Tailwind.apply "app-container" ]
      [ renderSidebar state
      , renderSlot state -- render whichever route is active in the slot
      ]
    ]

  renderSidebar :: State -> H.ComponentHTML Action () m
  renderSidebar state =
    Backdrop.backdrop [ Tailwind.apply "story-sidebar" ]
    [ HH.div
      [ HP.class_ $ HH.ClassName "flex-1 p-2 overflow-y-auto" ]
      [ HH.img
          [ HP.class_ $ HH.ClassName "w-24 mb-8 p-2 bg-white"
          , HP.src "PSD3-logo.png"
          ]
      , HH.nav
        [ HP.class_ $ HH.ClassName "text-base overflow-y-auto" ]
        [ ]
      ]
    ]

  renderSlot :: State -> H.ComponentHTML Action () m
  renderSlot state =
    HH.div_ [ HH.text state ]

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    ShowGUP      -> H.modify_ \state -> "GUP"
    ShowTrees    -> H.modify_ \state -> "Trees"
    ShowLesMis   -> H.modify_ \state -> "LesMis"
    ShowMetaTree -> H.modify_ \state -> "MetaTree"
    ShowPrinter  -> H.modify_ \state -> "Printer"
    ShowSpago    -> H.modify_ \state -> "Spago"
      