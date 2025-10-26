module PSD3.Understanding.SankeyDiagram where -- Understanding

import Prelude

import D3.Viz.Sankey.Model as Sankey
import D3.Viz.SankeyDiagram as SankeyViz
import PSD3.Internal.Sankey.Types (initialSankeyLayoutState_, SankeyLayoutState_)
import PSD3.Interpreter.D3 (runWithD3_Sankey)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SectionNav as SectionNav
import PSD3.Website.Types (Route(..), Section(..))
import Type.Proxy (Proxy(..))

-- | SankeyDiagram page state
type State = {
  sankeyLayout :: SankeyLayoutState_
}

-- | SankeyDiagram page actions
data Action = Initialize

-- | Child component slots
type Slots = ( sectionNav :: forall q. H.Slot q Void Unit )

_sectionNav = Proxy :: Proxy "sectionNav"

-- | SankeyDiagram page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> { sankeyLayout: initialSankeyLayoutState_ }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ -- TOC Panel (LHS)
      HH.div
        [ HP.classes [ HH.ClassName "toc-panel" ] ]
        [ HH.img
            [ HP.src "bookmark.jpeg"
            , HP.alt ""
            , HP.classes [ HH.ClassName "toc-panel__bookmark-pin" ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "toc-panel__main" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "floating-panel__header" ] ]
                [ HH.h3
                    [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                    [ HH.text "Contents" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "floating-panel__toggle" ]
                    , HP.type_ HP.ButtonButton
                    ]
                    [ HH.text "−" ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "floating-panel__content", HH.ClassName "toc-panel__content" ] ]
                [ HH.nav
                    [ HP.classes [ HH.ClassName "toc-nav" ] ]
                    [ HH.a [ HP.href "#section-1", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "1. UK Energy Flows" ]
                    ]
                ]
            ]
        ]

    -- Navigation Panel (RHS)
    , HH.slot_ _sectionNav unit SectionNav.component
        { currentSection: UnderstandingSection
        , currentRoute: SankeyDiagram
        , sectionPages:
            [ { route: About, label: "About" }
            , { route: Tutorial, label: "Tutorial" }
            , { route: SimpleCharts, label: "Simple Charts" }
            , { route: ChordDiagram, label: "Chord Diagram" }
            , { route: BubbleChart, label: "Bubble Chart" }
            , { route: SankeyDiagram, label: "Sankey Diagram" }
            , { route: Hierarchies, label: "Hierarchies" }
            , { route: Interpreters, label: "Interpreters" }
            , { route: CodeExplorer, label: "Code Explorer" }
            ]
        , moduleCategories: Nothing
        }

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Sankey Diagram: Flow Visualization" ]
        , HH.p_
            [ HH.text "Sankey diagrams visualize the flow of resources, energy, costs, or other quantities through a system. The width of each connection is proportional to the flow quantity, making it easy to identify dominant flows and inefficiencies." ]
        , HH.p_
            [ HH.text "These diagrams are particularly effective for showing how quantities are distributed, transformed, and consumed across multiple stages of a process." ]
        ]

    -- Section 1: UK Energy System
    , HH.section
        [ HP.id "section-1"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. UK Energy System Flows" ]
        , HH.p_
            [ HH.text "This Sankey diagram shows energy flows in the UK energy system, from primary energy sources through transformation and distribution to final consumption. The diagram uses D3's Sankey layout algorithm to automatically position nodes and create smooth flow paths." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "sankey-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "The width of each flow represents the quantity of energy. Notice how the diagram reveals energy losses in transformation processes and highlights which sources contribute most to final consumption." ]
        ]
    ]

-- Handle actions
handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Run Sankey with required state context (H.HalogenM provides MonadState)
    runWithD3_Sankey do
      SankeyViz.draw Sankey.energyData "div.sankey-viz"
