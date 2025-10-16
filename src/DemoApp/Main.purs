module Main where

import Prelude

import Data.Maybe (Maybe(..))
import DemoApp.UI.Backdrop (backdrop) as Backdrop
import DemoApp.UI.Format as Format
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Stories.GUP as GUP
import Stories.LineChart as LineChart
import Stories.Sankey as Sankey
import Stories.LesMis as LesMis
import Stories.MetaTree as MetaTree
import Stories.PrintTree as PrintTree
import Stories.Spago as Spago
import Stories.ThreeLittleCircles as Circles
import Stories.Trees as Trees
import Stories.Utilities as Utils
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI parent unit body

type Slots = ( index     :: forall q. H.Slot q Void Unit
             , circles   :: forall q. H.Slot q Void Unit
             , gup       :: forall q. H.Slot q Void Unit
             , trees     :: forall q. H.Slot q Void Unit
             , metatree  :: forall q. H.Slot q Void Unit
             , printtree :: forall q. H.Slot q Void Unit
             , lesmis    :: forall q. H.Slot q Void Unit
             , spago     :: forall q. H.Slot q Void Unit
             , sankey    :: forall q. H.Slot q Void Unit
             , linechart :: forall q. H.Slot q Void Unit
             )

_index     = Proxy :: Proxy "index"
_circles   = Proxy :: Proxy "circles"
_gup       = Proxy :: Proxy "gup"
_trees     = Proxy :: Proxy "trees"
_metatree  = Proxy :: Proxy "metatree"
_printtree = Proxy :: Proxy "printtree"
_lesmis    = Proxy :: Proxy "lesmis"
_spago     = Proxy :: Proxy "spago"
_sankey    = Proxy :: Proxy "sankey"
_linechart = Proxy :: Proxy "linechart"

type ParentState = ExampleType

data ExampleType = None | ExampleCircles | ExampleGUP | ExampleTrees | ExampleLesMis | ExampleMetaTree | ExamplePrinter | ExampleSankey | ExampleLineChart | ExampleSpago
derive instance Eq ExampleType
instance showExampleType :: Show ExampleType where
  show = case _ of
    None -> "No example selected"
    ExampleCircles  -> "Three Little Circles"
    ExampleGUP      -> "General Update Pattern"
    ExampleTrees    -> "Trees"
    ExampleLesMis   -> "LesMis"
    ExampleMetaTree -> "MetaTree"
    ExamplePrinter  -> "Printer"
    ExampleSpago    -> "Spago"
    ExampleSankey   -> "Sankey"
    ExampleLineChart -> "Line Chart" 

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
  initialState _ = ExampleSpago

  render :: ParentState -> H.ComponentHTML ParentAction Slots m
  render currentExample = 
    HH.body_ 
      [ HH.div [ Utils.tailwindClass "app-container" ]
               [ renderSidebar currentExample
               , renderExample currentExample
               ]
      ] 

  renderSidebar :: ParentState -> H.ComponentHTML ParentAction Slots m
  renderSidebar currentExample =
    Backdrop.backdrop [ Utils.tailwindClass "story-sidebar" ]
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
    [ Format.caption_ [ HH.text "Simple examples" ]
    , HH.ul [ HP.class_ $ HH.ClassName "list-reset" ]
            ((renderExampleNav currentExample) <$>
              [ ExampleCircles, ExampleGUP, ExampleTrees, ExampleLesMis, ExampleSankey, ExampleLineChart ])
    , Format.caption_ [ HH.text "Alternate interpreters" ]
    , HH.ul [ HP.class_ $ HH.ClassName "list-reset" ] 
            ((renderExampleNav currentExample) <$> 
              [ ExampleMetaTree, ExamplePrinter ])
    , Format.caption_ [ HH.text "Halogen Application" ]
    , HH.ul [ HP.class_ $ HH.ClassName "list-reset" ] 
            ((renderExampleNav currentExample) <$> 
              [ ExampleSpago ])
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
  renderExample = do
    case _ of
    -- TODO write an index page that explains the project
      -- None            -> HH.slot_ _index     unit Index.component unit
      None            -> HH.slot_ _index     unit Spago.component unit
      ExampleCircles  -> HH.slot_ _circles   unit Circles.component unit
      ExampleGUP      -> HH.slot_ _gup       unit GUP.component GUP.Paused
      ExampleTrees    -> HH.slot_ _trees     unit Trees.component unit 
      ExampleMetaTree -> HH.slot_ _metatree  unit MetaTree.component unit 
      ExamplePrinter  -> HH.slot_ _printtree unit PrintTree.component unit 
      ExampleLesMis    -> HH.slot_ _lesmis    unit LesMis.component unit
      ExampleSankey    -> HH.slot_ _sankey    unit Sankey.component unit
      ExampleLineChart -> HH.slot_ _linechart unit LineChart.component unit
      ExampleSpago     -> HH.slot_ _spago     unit Spago.component unit
      -- _ -> HH.div_ [ HH.text "That example is currently not available" ]


  handleAction :: ParentAction -> H.HalogenM ParentState ParentAction Slots output m Unit
  handleAction = case _ of
    Initialize   -> H.modify_ \_ -> None
    (Example ex) -> H.modify_ \_ -> ex
  
