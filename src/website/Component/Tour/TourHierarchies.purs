module Component.Tour.TourHierarchies where

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
    -- No examples to render in this page (link to separate pages instead)
    pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourHierarchies
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "3. Hierarchical Layouts" ]
            , HH.p_
                [ HH.text "Hierarchical data structures are everywhere in computing: file systems, organizational charts, taxonomies, JSON documents, and abstract syntax trees. Different visualization layouts reveal different aspects of the same hierarchical data." ]
            , HH.p_
                [ HH.text "This page demonstrates different ways to visualize hierarchical datasets. Use the links below to explore each layout type and see how each representation emphasizes different relationships in the data." ]
            ]

        -- Section 1: Node-Link Diagrams
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "node-link"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "1. Node-Link Diagrams (Trees and Dendrograms)" ]
            , HH.p_
                [ HH.text "Node-link diagrams show hierarchies as connected nodes, with parent-child relationships represented by links. These layouts can be oriented horizontally, vertically, or radially." ]
            , HH.h3_
                [ HH.text "Tidy Trees" ]
            , HH.p_
                [ HH.text "Compact tidy tree layout using the Reingold-Tilford algorithm. This algorithm minimizes the width of the tree while maintaining clear parent-child relationships and avoiding node overlaps." ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Horizontal Tidy Tree: " ]
                    , HH.text "Root on the left, grows to the right. Efficient use of horizontal space."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Vertical Tidy Tree: " ]
                    , HH.text "Root at the top, grows downward. Common in organizational charts."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Radial Tidy Tree: " ]
                    , HH.text "Emanates from center in a circular layout. Space-efficient for large hierarchies."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Isometric Tidy Tree: " ]
                    , HH.text "Unique 2.5D isometric projection creates technical drawing aesthetic with 30° cabinet projection for depth perception."
                    ]
                ]
            , HH.h3_
                [ HH.text "Dendrograms (Cluster Diagrams)" ]
            , HH.p_
                [ HH.text "Dendrogram layouts place all leaf nodes at the same level, creating a uniform appearance useful for comparing leaf nodes. Available in the same orientations as tidy trees." ]
            , HH.p_
                [ HH.text "We have an interactive demo showing "
                , HH.a
                    [ HP.href $ "#" <> routeToPath AnimatedTreeCluster ]
                    [ HH.text "animated transitions between Tree and Cluster layouts" ]
                , HH.text " that demonstrates how the same data looks different with these two algorithms."
                ]
            , HH.p_
                [ HH.em_ [ HH.text "[Additional orientation examples not yet implemented in TreeAPI - coming soon]" ] ]
            ]

        -- Section 2: Adjacency Diagrams
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "adjacency"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "2. Adjacency Diagrams (Icicle/Sunburst)" ]
            , HH.p_
                [ HH.text "Adjacency diagrams show hierarchy through relative placement of rectangles or arcs. They're space-efficient and can encode quantitative values through area." ]
            , HH.h3_
                [ HH.text "Icicle Diagram" ]
            , HH.p_
                [ HH.text "Rectangular subdivisions showing hierarchy through relative placement. Each level of the hierarchy is a horizontal band, with children subdividing their parent's width. Area encodes quantitative values." ]
            , HH.p_
                [ HH.em_ [ HH.text "[Icicle diagram not yet implemented in TreeAPI - coming soon]" ] ]
            , HH.h3_
                [ HH.text "Sunburst Diagram" ]
            , HH.p_
                [ HH.text "The radial equivalent of icicle diagrams. The root is at the center, and each ring represents a level of the hierarchy. Angular width represents quantitative value or number of children." ]
            , HH.p_
                [ HH.em_ [ HH.text "[Sunburst diagram not yet implemented in TreeAPI - coming soon]" ] ]
            ]

        -- Section 3: Enclosure Diagrams
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "enclosure"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "3. Enclosure Diagrams (Treemap and Circle Packing)" ]
            , HH.p_
                [ HH.text "Enclosure diagrams show hierarchy through containment, where parent elements completely enclose their children. These layouts efficiently use space and make quantitative comparisons easy." ]
            , HH.h3_
                [ HH.text "Treemap" ]
            , HH.p_
                [ HH.text "Space-efficient rectangular subdivisions where area is proportional to value. Treemaps show hierarchy through nesting and excel at displaying large amounts of hierarchical data in a compact space. The recursive subdivision algorithm creates rectangles whose area represents quantitative values." ]
            , HH.p_
                [ HH.em_ [ HH.text "[Treemap not yet implemented in TreeAPI - coming soon]" ] ]
            , HH.h3_
                [ HH.text "Circle Packing" ]
            , HH.p_
                [ HH.text "Tightly nested circles showing hierarchy through containment. Area encodes values, and the circular layout readily shows topology. The packing algorithm positions circles to minimize wasted space while maintaining clear hierarchical relationships." ]
            , HH.p_
                [ HH.em_ [ HH.text "[Circle packing not yet implemented in TreeAPI - coming soon]" ] ]
            ]

        -- Section 4: Edge Bundling
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "edge-bundling"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "4. Hierarchical Edge Bundling" ]
            , HH.p_
                [ HH.text "Edge bundling is a technique for reducing visual clutter in graphs with many edges. Edges are bundled together along the hierarchical structure, creating visually appealing flow patterns that reveal high-level connectivity patterns." ]
            , HH.p_
                [ HH.text "This technique is particularly effective for visualizing dependencies, relationships, or flows in large hierarchical datasets like software dependencies, communication networks, or biological pathways." ]
            , HH.p_
                [ HH.em_ [ HH.text "[Edge bundling not yet implemented in TreeAPI - coming soon]" ] ]
            ]

        -- Interactive exploration note
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Interactive Exploration" ]
            , HH.p_
                [ HH.text "To see a working example of hierarchical layouts with interactive switching between tree and cluster algorithms, visit the "
                , HH.a
                    [ HP.href $ "#" <> routeToPath AnimatedTreeCluster ]
                    [ HH.text "Animated Tree ↔ Cluster demo" ]
                , HH.text " which shows smooth transitions between different layouts of the same hierarchical data."
                ]
            , HH.p_
                [ HH.text "More hierarchical layout examples will be added to the "
                , HH.a
                    [ HP.href $ "#" <> routeToPath Gallery ]
                    [ HH.text "Examples Gallery" ]
                , HH.text " as they are implemented in TreeAPI."
                ]
            ]
        ]
    ]
