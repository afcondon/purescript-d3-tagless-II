module PSD3.FpFtw.HTML where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.FpFtw.Actions (Action)
import PSD3.FpFtw.State (State)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))

-- | Render the FP FTW page
render :: forall w. State -> HH.HTML w Action
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "example-page" ] ]
    [ -- Navigation Header
      TutorialNav.renderHeader FpFtw

    -- Page content
    , HH.main
        [ HP.classes [ HH.ClassName "tutorial-content" ] ]
        [ HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "6. FP FTW" ]
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
        , HH.p_
            [ HH.a
                [ HP.href "#/example/map-quartet"
                , HP.classes [ HH.ClassName "tutorial-link" ]
                ]
                [ HH.text "View interactive example with full source code →" ]
            ]
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
        , HH.p_
            [ HH.a
                [ HP.href "#/example/topological-sort"
                , HP.classes [ HH.ClassName "tutorial-link" ]
                ]
                [ HH.text "View interactive example with full source code →" ]
            ]
        ]

    -- Section 3: Les Misérables Topological Layers
    , HH.section
        [ HP.id "lesmis-layers"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "3. Les Misérables Character Dependencies" ]
        , HH.p_
            [ HH.text "Applying topological sort to the Les Misérables character co-appearance graph reveals a surprising result: the graph is acyclic! With 77 characters arranged across 27 dependency layers (0-26), we can see the narrative structure of Victor Hugo's novel. Characters are positioned vertically by their dependency layer, showing which characters' storylines build upon others." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "lesmis-layers-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "This visualization uses the same Data.Graph topological sort, but on a much larger real-world dataset. Characters within the same layer have no dependencies on each other and represent parallel narrative threads. The layered structure reveals the hierarchical relationships in the story - from the foundational characters at layer 0 to the culminating interactions at layer 26." ]
        ]
      ]
    ]
