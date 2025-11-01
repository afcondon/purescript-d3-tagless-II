module PSD3.FpFtw.HTML where

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.FpFtw.Actions (Action)
import PSD3.FpFtw.State (State)
import PSD3.Understanding.TOC (renderTOC, tocAnchor)

-- | Render the FP FTW page
render :: forall w. State -> HH.HTML w Action
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ -- TOC Panel (LHS)
      renderTOC
        { title: "Page Contents"
        , items:
            [ tocAnchor "map-quartet" "1. Map-Based Quartet" 0
            , tocAnchor "topological-sort" "2. Topological Sort" 0
            ]
        , image: Just "images/understanding-bookmark-trees.jpeg"
        }

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "FP FTW" ]
        , HH.p_
            [ HH.text "Functional Programming For The Win! This page showcases PSD3's unique capabilities in handling different data structures through its Finally Tagless design." ]
        , HH.p_
            [ HH.text "Unlike traditional D3 bindings that work only with arrays, PSD3 can visualize any Foldable data structure - Maps, Sets, Lists, and more - thanks to its polymorphic type class constraints." ]
        ]

    -- Section 1: Map-Based Quartet
    , HH.section
        [ HP.id "map-quartet"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. Map-Based Scatter Plot Quartet" ]
        , HH.p_
            [ HH.text "This visualization demonstrates PSD3's ability to work directly with Map data structures. Each of the four scatter plots receives a different sparse Map with only 15 data points out of a possible 200 x-values, showing how PSD3 handles sparse data naturally through its Foldable constraint." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "map-quartet-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "Unlike arrays where all datasets would need 200 elements (with nulls or placeholder values for missing data), Maps only store the actual measurements. Each dataset has different randomly-selected keys, demonstrating that Maps naturally handle irregular sampling. This is particularly useful for time-series data with missing values, experimental data with different sample points, or any scenario where data is inherently sparse." ]
        ]

    -- Section 2: Topological Sort
    , HH.section
        [ HP.id "topological-sort"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "2. Topological Sort with Data.Graph" ]
        , HH.p_
            [ HH.text "This visualization demonstrates PureScript's Data.Graph library and its topologicalSort function. The Graph type is elegantly defined as: Graph k v = Graph (Map k (Tuple v (List k))), where each key maps to its value and a list of dependencies. We build a build pipeline as a directed acyclic graph (DAG) and use topologicalSort to determine execution order." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "topological-sort-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "Tasks are arranged in layers computed from the topological sort. Layer 0 contains tasks with no dependencies, Layer 1 contains tasks depending only on Layer 0, and so on. All tasks within a layer can be executed in parallel. The topologicalSort function handles cycle detection and returns tasks in a valid execution order - demonstrating how functional data structures and algorithms elegantly solve dependency resolution problems found in build systems, task schedulers, and module systems." ]
        ]
    ]
