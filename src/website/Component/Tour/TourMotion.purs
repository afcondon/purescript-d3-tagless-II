module Component.Tour.TourMotion where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
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
    -- No examples to render on this page (link to separate pages)
    pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourMotion
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "5. Movement & Transition" ]
            , HH.p_
                [ HH.text "This page explores transitions and movement in D3 visualizations, from simple color and position changes to complex physics simulations with the General Update Pattern." ]
            ]

        -- Section 1: Three Circles Color Mixing
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "section-1"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "1. Basic Transitions: Color Mixing" ]
            , HH.p_
                [ HH.text "Transitions allow smooth animated changes to visual properties. This simple example shows three circles transitioning from green to RGB primary colors (red, green, blue) with 50% opacity. The circles reposition to overlap, demonstrating additive color mixing where overlapping colors create secondary colors: cyan (green + blue), magenta (red + blue), and yellow (red + green)." ]
            , HH.p_
                [ HH.em_ [ HH.text "[Color mixing transition example not yet implemented in TreeAPI - coming soon]" ] ]
            ]

        -- Section 2: General Update Pattern
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "section-2"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "2. The General Update Pattern" ]
            , HH.p_
                [ HH.text "This deceptively simple example shows off an aspect of screen-based data visualization that has no analogue in paper visualizations: the ability to specify how updates to the data should be represented." ]
            , HH.p_
                [ HH.text "In this example, some letters of the alphabet are presented and then constantly updated. When a letter enters at first, it falls in from the top and it is green. If it's still present in the next set of letters it stays on the screen, but it turns gray and moves to an alphabetically correct new position. And if it's not present in the new data, it turns red and falls out before disappearing." ]
            , HH.p_
                [ HH.em_ [ HH.text "[General Update Pattern example not yet implemented in TreeAPI - coming soon]" ] ]
            ]

        -- Section 3: Les Misérables Force Layout
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "section-3"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "3. Force-Directed Graph: Les Misérables" ]
            , HH.p_
                [ HH.text "Force-directed graphs use physics simulation to position nodes and links. Nodes repel each other like charged particles, while links act as springs pulling connected nodes together. The simulation finds an equilibrium that naturally reveals the structure of the network." ]
            , HH.p_
                [ HH.text "This example uses the simplified SimulationM API - a single "
                , HH.code_ [ HH.text "init" ]
                , HH.text " call with a configuration record, followed by "
                , HH.code_ [ HH.text "start" ]
                , HH.text ". The graph shows character co-occurrence in Victor Hugo's Les Misérables, where node size represents importance and link thickness shows the strength of connections."
                ]
            , HH.p_
                [ HH.em_ [ HH.text "[Les Misérables force layout example not yet implemented in TreeAPI - coming soon]" ] ]
            , HH.p_
                [ HH.text "Drag nodes to see the force simulation respond. The simulation applies multiple forces: center (pulls toward middle), charge (nodes repel), collision (prevents overlap), and link (pulls connected nodes together)." ]
            ]

        -- Section 4: Animated Tree Transitions
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "section-4"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "4. Animated Tree Transitions" ]
            , HH.p_
                [ HH.text "This demonstration shows smooth transitions between Tidy Tree and Dendrogram (Cluster) layouts. The same nodes smoothly animate to their new positions—no enter/exit, just updates. This example showcases the TreeAPI's ability to handle complex layout transitions with data-driven animations." ]
            , HH.p_
                [ HH.text "Visit the "
                , HH.a
                    [ HP.href $ "#" <> routeToPath AnimatedTreeCluster ]
                    [ HH.text "Animated Tree ↔ Cluster demo" ]
                , HH.text " to see this in action. The demo shows 252 nodes from the Flare visualization toolkit transitioning smoothly between different hierarchical layouts."
                ]
            , HH.p_
                [ HH.text "The transitions are implemented using D3's data join with identity-based keys, ensuring each node maintains its identity across layout changes. Children are sorted by height to eliminate crossovers during animation, creating smooth and comprehensible transitions."
                ]
            ]
        ]
    ]
