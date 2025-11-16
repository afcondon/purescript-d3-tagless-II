module Component.Tour.TourFlow where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))

-- | Tour page state
type State = Unit

-- | Tour page actions
data Action = Initialize

-- | Tour page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- No examples to render yet
    pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourFlow
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "4. Data Flow Visualizations" ]
            , HH.p_
                [ HH.text "Chord and Sankey diagrams are specialized visualizations for showing relationships and flows between entities. Both use visual metaphors - ribbons and flows - to make complex interconnections immediately comprehensible." ]
            , HH.p_
                [ HH.text "These visualization types excel at revealing patterns in network data, resource flows, and dependencies that would be difficult to understand in tabular form." ]
            ]

        -- Section 1: Chord Diagram
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "chord"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "1. Chord Diagram: Circular Relationships" ]
            , HH.p_
                [ HH.text "Chord diagrams show relationships and flows between entities in a circular layout. They're particularly effective for displaying interconnected systems, dependencies, or flows between groups." ]
            , HH.p_
                [ HH.text "This example visualizes dependencies between fundamental programming concepts. Each arc represents a concept, and the ribbons show how strongly they depend on each other." ]
            , HH.p_
                [ HH.em_ [ HH.text "[Chord diagram not yet implemented in TreeAPI - coming soon]" ] ]
            , HH.p_
                [ HH.text "The circular layout makes it easy to see both direct dependencies (following a single chord) and the overall pattern of interconnections in the system. The thickness of each chord represents the strength of the relationship." ]
            ]

        -- Section 2: Sankey Diagram
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "sankey"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "2. Sankey Diagram: Flow Visualization" ]
            , HH.p_
                [ HH.text "Sankey diagrams visualize the flow of resources, energy, costs, or other quantities through a system. The width of each connection is proportional to the flow quantity, making it easy to identify dominant flows and inefficiencies." ]
            , HH.p_
                [ HH.text "This diagram shows energy flows in the UK energy system, from primary energy sources through transformation and distribution to final consumption. The Sankey layout algorithm automatically positions nodes and creates smooth flow paths." ]
            , HH.p_
                [ HH.em_ [ HH.text "[Sankey diagram not yet implemented in TreeAPI - coming soon]" ] ]
            , HH.p_
                [ HH.text "The width of each flow represents the quantity of energy. Notice how the diagram reveals energy losses in transformation processes and highlights which sources contribute most to final consumption." ]
            ]
        ]
    ]
