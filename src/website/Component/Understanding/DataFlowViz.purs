module PSD3.Understanding.DataFlowViz where -- Understanding

import Prelude

import D3.Viz.ChordDiagram as Chord
import D3.Viz.Sankey.Model as Sankey
import D3.Viz.SankeyDiagram as SankeyViz
import PSD3.Internal.Sankey.Types (initialSankeyLayoutState_, SankeyLayoutState_)
import PSD3.Interpreter.D3 (eval_D3M, runWithD3_Sankey)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.ExamplesNav as ExamplesNav
import PSD3.Understanding.TOC (renderTOC, tocAnchor, tocRoute)
import PSD3.Website.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | DataFlowViz page state
type State = {
  sankeyLayout :: SankeyLayoutState_
}

-- | DataFlowViz page actions
data Action = Initialize

-- | Child component slots
type Slots = ( examplesNav :: forall q. H.Slot q Void Unit )

_examplesNav = Proxy :: Proxy "examplesNav"

-- | DataFlowViz page component
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
      renderTOC
        { title: "Page Contents"
        , items:
            [ tocAnchor "chord" "1. Chord Diagram" 0
            , tocRoute (Explore "ChordDiagramDraw") "→ How-to guide" 1
            , tocAnchor "sankey" "2. Sankey Diagram" 0
            , tocRoute (Explore "SankeyDraw") "→ How-to guide" 1
            ]
        , image: Just "images/understanding-bookmark-trees.jpeg"
        }

    -- Navigation Panel (RHS)
    , HH.slot_ _examplesNav unit ExamplesNav.component DataFlowViz

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Data Flow Visualizations" ]
        , HH.p_
            [ HH.text "Chord and Sankey diagrams are specialized visualizations for showing relationships and flows between entities. Both use visual metaphors - ribbons and flows - to make complex interconnections immediately comprehensible." ]
        , HH.p_
            [ HH.text "These visualization types excel at revealing patterns in network data, resource flows, and dependencies that would be difficult to understand in tabular form." ]
        ]

    -- Section 1: Chord Diagram
    , HH.section
        [ HP.id "chord"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. Chord Diagram: Circular Relationships" ]
        , HH.p_
            [ HH.text "Chord diagrams show relationships and flows between entities in a circular layout. They're particularly effective for displaying interconnected systems, dependencies, or flows between groups." ]
        , HH.p_
            [ HH.text "This example visualizes dependencies between fundamental programming concepts. Each arc represents a concept, and the ribbons show how strongly they depend on each other." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "chord-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "The circular layout makes it easy to see both direct dependencies (following a single chord) and the overall pattern of interconnections in the system. The thickness of each chord represents the strength of the relationship." ]
        ]

    -- Section 2: Sankey Diagram
    , HH.section
        [ HP.id "sankey"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "2. Sankey Diagram: Flow Visualization" ]
        , HH.p_
            [ HH.text "Sankey diagrams visualize the flow of resources, energy, costs, or other quantities through a system. The width of each connection is proportional to the flow quantity, making it easy to identify dominant flows and inefficiencies." ]
        , HH.p_
            [ HH.text "This diagram shows energy flows in the UK energy system, from primary energy sources through transformation and distribution to final consumption. The Sankey layout algorithm automatically positions nodes and creates smooth flow paths." ]
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
    _ <- H.liftEffect $ eval_D3M $ Chord.draw Chord.exampleMatrix Chord.exampleLabels "div.chord-viz"
    runWithD3_Sankey do
      SankeyViz.draw Sankey.energyData "div.sankey-viz"
