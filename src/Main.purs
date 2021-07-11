module Main where

import Prelude

import D3Tagless.Block.Expandable as Expandable
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Stories.GUP as GUP
import Stories.Tailwind.Styles as Tailwind
import Type.Proxy (Proxy(..))
import UIGuide.Block.Backdrop (backdrop) as Backdrop

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI parent unit body

type Slots = ( gup    :: forall q. H.Slot q Void Unit
             , button :: forall q. H.Slot q Void Int )

_gup    = Proxy :: Proxy "gup"
_button = Proxy :: Proxy "button"

type ParentState = String

data ParentAction = Initialize | ShowGUP | ShowTrees | ShowLesMis | ShowMetaTree | ShowPrinter | ShowSpago

parent :: forall query input output m. (MonadAff m) => H.Component query input output m
parent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
        { handleAction = handleAction
        , initialize   = Just Initialize }
    }
  where
  initialState :: input -> ParentState
  initialState _ = "empty"

  render :: ParentState -> H.ComponentHTML ParentAction Slots m
  render state = 
    HH.body_ 
      [ HH.div [ Tailwind.apply "app-container" ]
               [ renderSidebar state
               , HH.div_ [ HH.slot_ _gup    unit GUP.component GUP.Paused ]
               , HH.div_ [ HH.slot_ _button 0    button        { label: "button value" } ]
               ]
      ] 

  renderSidebar :: ParentState -> H.ComponentHTML ParentAction Slots m
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

  handleAction :: ParentAction -> H.HalogenM ParentState ParentAction Slots output m Unit
  handleAction = case _ of
    Initialize   -> H.modify_ \state -> "A gallery of D3 examples"
    ShowGUP      -> H.modify_ \state -> "GUP"
    ShowTrees    -> H.modify_ \state -> "Trees"
    ShowLesMis   -> H.modify_ \state -> "LesMis"
    ShowMetaTree -> H.modify_ \state -> "MetaTree"
    ShowPrinter  -> H.modify_ \state -> "Printer"
    ShowSpago    -> H.modify_ \state -> "Spago"
      

-- Now we turn to our child component, the button.

type ButtonInput = { label :: String }

type ButtonState = { label :: String }

button :: forall query output m. H.Component query ButtonInput output m
button =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  initialState :: ButtonInput -> ButtonState
  initialState { label } = { label }

  render :: forall action. ButtonState -> H.ComponentHTML action () m
  render { label } = HH.button_ [ HH.text label ]