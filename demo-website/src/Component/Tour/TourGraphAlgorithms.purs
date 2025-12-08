module Component.Tour.TourGraphAlgorithms where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Aff (Milliseconds(..), delay)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))
import D3.Viz.FPFTW.TopologicalSort as TopoSort
import D3.Viz.FPFTW.TransitiveReduction as TransReduction
import D3.Viz.FPFTW.KoenigsbergBridges as Koenigsberg
import D3.Viz.FPFTW.DAGTreeExample as DAGTree

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

    -- Render topological sort
    liftEffect $ TopoSort.drawTopologicalSort "#topological-sort"

    -- Render transitive reduction
    liftEffect $ TransReduction.drawTransitiveReduction "#transitive-reduction"

    -- Render Königsberg bridges
    _ <- liftEffect $ Koenigsberg.drawKoenigsbergBridges "#koenigsberg-bridges"

    -- Render DAG tree example
    liftEffect $ DAGTree.drawDAGTreeExample "#dag-tree-example"

    pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourGraphAlgorithms
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Graph Algorithms: FP Wizardry" ]
            , HH.p_
                [ HH.text "Graphs are everywhere in software: dependency trees, build systems, task schedulers, network routing, code analysis. This page demonstrates how "
                , HH.strong_ [ HH.text "pure functional graph algorithms" ]
                , HH.text " combined with "
                , HH.strong_ [ HH.text "declarative visualization" ]
                , HH.text " create powerful, elegant solutions to real-world problems."
                ]
            , HH.p_
                [ HH.text "We'll explore two classic algorithms that solve practical problems: topological sort (ordering tasks with dependencies) and transitive reduction (removing redundant edges). Both showcase the FP advantage: immutable transformations, pure functions, and easy-to-test logic." ]
            ]

        -- Section 1: Topological Sort
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "topological-sort-section"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "1. Topological Sort: Ordering Dependencies" ]
            , HH.p_
                [ HH.text "Topological sort solves a fundamental problem: "
                , HH.em_ [ HH.text "\"In what order should I execute tasks that depend on each other?\"" ]
                , HH.text " It's used in build systems (compile before link), package managers (install dependencies first), and task schedulers."
                ]
            , HH.p_
                [ HH.text "The algorithm takes a "
                , HH.strong_ [ HH.text "directed acyclic graph (DAG)" ]
                , HH.text " and produces a "
                , HH.strong_ [ HH.text "linear ordering" ]
                , HH.text " where every task comes after its dependencies. Below is a build pipeline with 7 tasks:"
                ]
            , HH.div
                [ HP.id "topological-sort"
                , HP.classes [ HH.ClassName "viz-container" ]
                , HP.style "margin: 20px 0; text-align: center;"
                ]
                []
            , HH.p_
                [ HH.text "Notice how tasks are arranged in "
                , HH.strong_ [ HH.text "layers" ]
                , HH.text " (L0, L1, L2, etc.). Each layer can execute in parallel since tasks in the same layer don't depend on each other. This is exactly what CI/CD systems do!"
                ]
            , HH.h3_ [ HH.text "The FP Win: Pure Graph Transformation" ]
            , HH.p_
                [ HH.text "The topological sort implementation uses PureScript's "
                , HH.code_ [ HH.text "Data.Graph" ]
                , HH.text " library with a purely functional algorithm:"
                ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ]
                , HP.style "background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto;"
                ]
                [ HH.code_
                    [ HH.text """-- Build a Graph from tasks
buildTaskGraph :: Array Task -> Graph String Task
buildTaskGraph tasks = fromMap graphMap
  where
    graphMap = foldl addTask Map.empty tasks
    addTask acc task =
      Map.insert task.id
        (Tuple task (List.fromFoldable task.depends))
        acc

-- Pure topological sort (from Data.Graph)
getTopologicalOrder :: Array Task -> List String
getTopologicalOrder tasks =
  topologicalSort (buildTaskGraph tasks)

-- Compute layers: tasks with no deps at layer 0,
-- tasks depending on layer 0 at layer 1, etc.
computeLayers :: Array Task -> Map String Int
computeLayers tasks = foldl processTask Map.empty sortedIds
  where
    sortedIds = List.reverse $ getTopologicalOrder tasks"""
                    ]
                ]
            , HH.p_
                [ HH.text "Key advantages:" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Immutable: " ], HH.text "Graph transformations return new graphs, never mutate" ]
                , HH.li_ [ HH.strong_ [ HH.text "Pure: " ], HH.text "Same input always produces same output, easy to test" ]
                , HH.li_ [ HH.strong_ [ HH.text "Composable: " ], HH.text "Topological sort → layer computation → visualization pipeline" ]
                , HH.li_ [ HH.strong_ [ HH.text "Type-safe: " ], HH.text "Compiler ensures graph structure is valid" ]
                ]
            ]

        -- Section 2: Transitive Reduction
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "transitive-reduction-section"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "2. Transitive Reduction: Removing Redundant Edges" ]
            , HH.p_
                [ HH.text "Transitive reduction solves another practical problem: "
                , HH.em_ [ HH.text "\"Which dependencies are redundant?\"" ]
                , HH.text " In dependency graphs, some edges are "
                , HH.strong_ [ HH.text "implied by transitivity" ]
                , HH.text ". For example, if A depends on B, and B depends on C, then we don't need to explicitly state that A depends on C."
                ]
            , HH.p_
                [ HH.text "Removing transitive edges simplifies graphs, making them easier to understand and maintain. This is crucial for build systems, package managers, and code dependency analysis."
                ]
            , HH.div
                [ HP.id "transitive-reduction"
                , HP.classes [ HH.ClassName "viz-container" ]
                , HP.style "margin: 20px 0; text-align: center;"
                ]
                []
            , HH.p_
                [ HH.text "The visualization shows "
                , HH.strong_ [ HH.text "before and after" ]
                , HH.text ". Redundant edges are highlighted in "
                , HH.span [ HP.style "color: #E74C3C;" ] [ HH.text "red" ]
                , HH.text " on the left, and removed on the right. Notice how the reduced graph preserves all reachability (A can still reach E through C → E) but with fewer explicit edges."
                ]
            , HH.h3_ [ HH.text "The Algorithm: Reachability Analysis" ]
            , HH.p_
                [ HH.text "The transitive reduction algorithm checks each edge: if the target is reachable through an "
                , HH.em_ [ HH.text "alternate path" ]
                , HH.text ", the edge is redundant:"
                ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ]
                , HP.style "background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto;"
                ]
                [ HH.code_
                    [ HH.text """-- Compute all nodes reachable from a given node
reachableFrom :: String -> Graph -> Set String
reachableFrom start graph = go Set.empty (Set.singleton start)
  where
    go visited frontier
      | Set.isEmpty frontier = visited
      | otherwise =
          let neighbors = getNeighbors frontier
              newNodes = Set.difference neighbors visited
          in go (Set.union visited frontier) newNodes

-- Remove transitive edges
transitiveReduction :: Graph -> Graph
transitiveReduction graph =
  { nodes: graph.nodes
  , edges: Map.mapWithKey reduceNode graph.edges
  }
  where
    reduceNode source targets =
      Set.filter (not <<< isTransitive source) targets

    isTransitive source target =
      -- Is target reachable through intermediate nodes?
      let intermediates = Set.delete target targets
      in Array.any (\\i -> Set.member target (reachableFrom i graph))
           (Set.toUnfoldable intermediates)"""
                    ]
                ]
            , HH.p_
                [ HH.text "Key FP advantages:" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Graph as value: " ], HH.text "Original and reduced graphs coexist, easy to compare" ]
                , HH.li_ [ HH.strong_ [ HH.text "Recursive traversal: " ], HH.text "Reachability computed via tail-recursive function" ]
                , HH.li_ [ HH.strong_ [ HH.text "Set operations: " ], HH.text "Union, difference, membership checks are declarative and clear" ]
                , HH.li_ [ HH.strong_ [ HH.text "Visualization-friendly: " ], HH.text "Easy to highlight removed edges for before/after comparison" ]
                ]
            ]

        -- Section 3: Königsberg Bridges
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "koenigsberg-section"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "3. Königsberg Bridges: The Birth of Graph Theory" ]
            , HH.p_
                [ HH.text "In 1736, Euler solved the famous "
                , HH.em_ [ HH.text "\"Seven Bridges of Königsberg\"" ]
                , HH.text " problem and invented graph theory in the process. The city had four land masses connected by seven bridges. The question: "
                , HH.strong_ [ HH.text "Can you walk through the city crossing each bridge exactly once?" ]
                ]
            , HH.div
                [ HP.id "koenigsberg-bridges"
                , HP.classes [ HH.ClassName "viz-container" ]
                , HP.style "margin: 20px 0; text-align: center;"
                ]
                []
            , HH.p_
                [ HH.text "Euler proved it's "
                , HH.strong_ [ HH.text "impossible" ]
                , HH.text ". His insight: a walk crossing each edge exactly once ("
                , HH.em_ [ HH.text "Eulerian path" ]
                , HH.text ") exists only if at most 2 nodes have odd degree. Here, "
                , HH.strong_ [ HH.text "all 4 nodes have odd degree" ]
                , HH.text " (the Island has degree 5, all others have degree 3)."
                ]
            , HH.p_
                [ HH.text "This tiny graph—just 4 nodes and 7 edges—launched an entire field of mathematics. Today we use Eulerian paths for DNA sequencing, circuit board routing, and garbage truck routes."
                ]
            ]

        -- Section 4: DAG Tree
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "dag-tree-section"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "4. DAG Tree: Trees with Extra Links" ]
            , HH.p_
                [ HH.text "Many real-world structures are "
                , HH.em_ [ HH.text "\"mostly hierarchical\"" ]
                , HH.text " with some extra non-tree connections. Git commit graphs have merge links. State machines have back-edges. The D3 Update Pattern has merge points."
                ]
            , HH.p_
                [ HH.text "The "
                , HH.strong_ [ HH.text "DAG Tree" ]
                , HH.text " library feature handles this elegantly: standard tree layout positions nodes, then extra links are overlaid. Tree edges are shown in "
                , HH.span [ HP.style "color: #708090;" ] [ HH.text "gray" ]
                , HH.text ", extra links in "
                , HH.span [ HP.style "color: #F4A460;" ] [ HH.text "orange" ]
                , HH.text ":"
                ]
            , HH.div
                [ HP.id "dag-tree-example"
                , HP.classes [ HH.ClassName "viz-container" ]
                , HP.style "margin: 20px 0; text-align: center;"
                ]
                []
            , HH.p_
                [ HH.text "Notice how the orange extra links "
                , HH.strong_ [ HH.text "leap across layers" ]
                , HH.text ": A→E skips a level, C→G crosses branches and skips a level, B→F crosses branches at the same depth. This is perfect for visualizing:"
                ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Git history: " ], HH.text "Tree follows first-parent, merge commits add extra links" ]
                , HH.li_ [ HH.strong_ [ HH.text "State machines: " ], HH.text "Tree of states, back-edges for loops" ]
                , HH.li_ [ HH.strong_ [ HH.text "Data flows: " ], HH.text "Tree structure with join/merge points" ]
                , HH.li_ [ HH.strong_ [ HH.text "Dependency graphs: " ], HH.text "Primary hierarchy with cross-dependencies" ]
                ]
            , HH.h3_ [ HH.text "Clean Separation of Concerns" ]
            , HH.p_
                [ HH.text "The DAG Tree API keeps tree layout and extra links separate:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ]
                , HP.style "background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto;"
                ]
                [ HH.code_
                    [ HH.text """-- Build DAG from tree + extra links
dag = dagTree myTree _.id
    # addLinks
        [ { source: "A", target: "E", linkType: "skip" }
        , { source: "C", target: "G", linkType: "cross" }
        ]

-- Layout returns positioned nodes + resolved links
positioned = layoutDAGTree Vertical size dag

-- positioned.nodes      -- Array of nodes with x, y coords
-- positioned.treeLinks  -- Parent→child links
-- positioned.extraLinks -- Extra links with source/target coords"""
                    ]
                ]
            , HH.p_
                [ HH.text "Tree algorithms handle positioning. Extra links are resolved by ID lookup. Rendering is trivial—just map over each array."
                ]
            ]

        -- Conclusion
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Why FP Wins for Graph Algorithms" ]
            , HH.p_
                [ HH.text "These examples demonstrate fundamental advantages of functional programming for graph algorithms:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Immutability enables comparison: " ]
                    , HH.text "We can keep the original graph and the transformed graph, making before/after visualizations trivial."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Purity enables testing: " ]
                    , HH.text "No hidden state, no side effects. Feed in a graph, get back a result. Test with edge cases, verify correctness."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Composition enables pipelines: " ]
                    , HH.text "Topological sort → layer computation → layout → visualization. Each step is a pure function."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Types prevent bugs: " ]
                    , HH.text "The compiler ensures you don't create cycles in a DAG, or reference non-existent nodes."
                    ]
                ]
            , HH.p_
                [ HH.text "Real-world applications:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Build systems: Bazel, Buck, Gradle all use topological sort for parallelization" ]
                , HH.li_ [ HH.text "Package managers: npm, cargo, pip use dependency resolution with cycle detection" ]
                , HH.li_ [ HH.text "Task schedulers: Apache Airflow, Luigi, Prefect order DAG execution" ]
                , HH.li_ [ HH.text "Code analysis: Import graphs, call graphs, module dependencies" ]
                ]
            , HH.p_
                [ HH.text "This is "
                , HH.strong_ [ HH.text "functional programming wizardry" ]
                , HH.text ": elegant algorithms solving hard problems, with visualization making the solutions immediately obvious."
                ]
            ]
        ]
    , HH.footer
        [ HP.classes [ HH.ClassName "tutorial-footer" ] ]
        [ HH.p_
            [ HH.text "Want more FP demonstrations? Check out "
            , HH.a
                [ HP.href $ "#" <> routeToPath TourFPFTW ]
                [ HH.text "FP For The Win" ]
            , HH.text " for examples with Maps, Sets, and functional patterns."
            ]
        ]
    ]
