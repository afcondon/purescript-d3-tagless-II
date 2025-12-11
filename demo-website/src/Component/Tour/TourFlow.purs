module Component.Tour.TourFlow where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))
import Effect.Class (liftEffect)
import Effect.Aff (Milliseconds(..), delay)
import PSD3.Shared.DataLoader (simpleLoadText)
import D3.Viz.TreeAPI.SankeyDiagram as SankeyDiagram
import D3.Viz.TreeAPI.ChordDiagram as ChordDiagram

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
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)

    -- Load and render Sankey
    csvText <- H.liftAff $ simpleLoadText "./data/energy.csv"
    liftEffect $ SankeyDiagram.startSankey csvText "#sankey-container"

    -- Render Chord diagram (Baton Rouge traffic data)
    liftEffect $ ChordDiagram.startChord "#chord-container"

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
                [ HH.text "3. Data Flow" ]
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
                [ HH.text "1. Chord Diagram: Traffic Flow Analysis" ]
            , HH.p_
                [ HH.text "Chord diagrams show bidirectional relationships and flows between entities in a circular layout. They're particularly effective for displaying interconnected systems where flow goes in both directions between pairs." ]
            , HH.p_
                [ HH.text "This example shows traffic flow between 11 regions in Baton Rouge, Louisiana, supporting analysis for a new bridge location. Each arc represents a region, and the ribbons show the volume of trips between region pairs. Ribbons are colored by their origin region, making it easy to trace outbound traffic from each area." ]
            , HH.div
                [ HP.id "chord-container"
                , HP.classes [ HH.ClassName "viz-container" ]
                ]
                []
            , HH.p_
                [ HH.text "Inspired by "
                , HH.a
                    [ HP.href "https://www.streetlightdata.com/planning-bridges-louisiana/"
                    , HP.target "_blank"
                    ]
                    [ HH.text "StreetLight Data's bridge planning analysis" ]
                , HH.text ". The circular layout reveals dominant traffic patterns - notice how West Baton Rouge (WBR, in red) and East Baton Rouge Main (EBR-M, in teal) serve as major hubs with thick ribbons to multiple regions."
                ]
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
                [ HH.text "This diagram shows energy flows in the UK energy system, from primary energy sources through transformation and distribution to final consumption. The Sankey layout uses a pure PureScript algorithm (not D3 FFI) that automatically positions nodes and creates smooth flow paths." ]
            , HH.div
                [ HP.id "sankey-container"
                , HP.classes [ HH.ClassName "viz-container" ]
                ]
                []
            , HH.p_
                [ HH.text "The width of each flow represents the quantity of energy. Notice how the diagram reveals energy losses in transformation processes and highlights which sources contribute most to final consumption." ]
            ]
        ]
    ]
