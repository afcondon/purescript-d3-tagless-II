module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Ocelot.Block.Format as Format
import Stories.GUP as GUP
import Stories.Tailwind.Styles as Tailwind
import Type.Proxy (Proxy(..))
import UIGuide.Block.Backdrop (backdrop) as Backdrop

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI parent unit body

type Slots = ( gup    :: forall q. H.Slot q Void Unit )
            --  , button :: forall q. H.Slot q Void Int )

_gup    = Proxy :: Proxy "gup"
-- _button = Proxy :: Proxy "button"

type ParentState = ExampleType

data ExampleType = None | ExampleGUP | ExampleTrees | ExampleLesMis | ExampleMetaTree | ExamplePrinter | ExampleSpago
derive instance Eq ExampleType
instance showExampleType :: Show ExampleType where
  show = case _ of
    None -> "No example selected"
    ExampleGUP      -> "GUP"
    ExampleTrees    -> "Trees"
    ExampleLesMis   -> "LesMis"
    ExampleMetaTree -> "MetaTree"
    ExamplePrinter  -> "Printer"
    ExampleSpago    -> "Spago"   

data ParentAction = Initialize | Example ExampleType


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
  initialState _ = ExampleGUP

  render :: ParentState -> H.ComponentHTML ParentAction Slots m
  render currentExample = 
    HH.body_ 
      [ HH.div [ Tailwind.apply "app-container" ]
               [ renderSidebar currentExample
               , renderExample currentExample
               ]
      ] 

  renderSidebar :: ParentState -> H.ComponentHTML ParentAction Slots m
  renderSidebar currentExample =
    Backdrop.backdrop [ Tailwind.apply "story-sidebar" ]
    [ HH.div
      [ HP.class_ $ HH.ClassName "flex-1 p-2 overflow-y-auto" ]
      [ HH.img
          [ HP.class_ $ HH.ClassName "w-24 mb-8 p-2 bg-white"
          , HP.src "PSD3-logo.png"
          ]
      , HH.nav
        [ HP.class_ $ HH.ClassName "text-base overflow-y-auto" ]
        [ renderNavGroup currentExample ]
      ]
    ]

  renderNavGroup :: ParentState -> H.ComponentHTML ParentAction Slots m
  renderNavGroup currentExample = 
    HH.div
    [ HP.class_ $ HH.ClassName "text-base overflow-y-auto" ]
    [ Format.caption_ [ HH.text "Group name" ]
    , HH.ul [ HP.class_ $ HH.ClassName "list-reset" ] 
            ((renderExampleNav currentExample) <$> 
              [ ExampleGUP, ExampleTrees, ExampleMetaTree, ExamplePrinter, ExampleLesMis, ExampleSpago ])
    ]

  renderExampleNav :: ParentState -> ExampleType -> H.ComponentHTML ParentAction Slots m
  renderExampleNav current example =
    HH.li
      [ HP.class_ $ HH.ClassName "mb-3" ]
      [ HH.a 
        [ HP.classes $
          Format.linkClasses <> 
            (if current == example then [ HH.ClassName "font-medium"] else [] )
        , HE.onClick (const $ Example example)
        ]
        [ HH.text (show example) ]
      ]


  renderExample :: ParentState -> H.ComponentHTML ParentAction Slots m
  renderExample = 
    case _ of
      None -> HH.div_ [ HH.text "No example has been selected" ]
      ExampleGUP      -> HH.slot_ _gup    unit GUP.component GUP.Paused
      ExampleTrees    -> HH.slot_ _gup    unit GUP.component GUP.Paused
      ExampleLesMis   -> HH.slot_ _gup    unit GUP.component GUP.Paused
      ExampleMetaTree -> HH.slot_ _gup    unit GUP.component GUP.Paused
      ExamplePrinter  -> HH.slot_ _gup    unit GUP.component GUP.Paused
      ExampleSpago    -> HH.slot_ _gup    unit GUP.component GUP.Paused


  handleAction :: ParentAction -> H.HalogenM ParentState ParentAction Slots output m Unit
  handleAction = case _ of
    Initialize   -> H.modify_ \_ -> None
    (Example ex) -> H.modify_ \_ -> ex

