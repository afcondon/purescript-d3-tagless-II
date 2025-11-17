module Component.Tour.TourFPFTW where

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
import D3.Viz.FPFTW.AnscombeQuartet as Anscombe
import D3.Viz.FPFTW.SetOperations as SetOps

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

    -- Render Anscombe's Quartet
    liftEffect $ Anscombe.drawAnscombeQuartet "#anscombe-quartet"

    -- Render Set Operations
    liftEffect $ SetOps.drawSetOperations "#set-operations"

    pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ TutorialNav.renderHeader TourFPFTW
    , HH.main_
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Functional Programming For The Win" ]
            , HH.p_
                [ HH.text "PureScript's powerful type system and functional programming features unlock visualization capabilities that are difficult or impossible in JavaScript. This page showcases how Maps, Sets, Foldable types, and contravariant functors make data visualization more flexible and type-safe." ]
            , HH.p_
                [ HH.text "These examples demonstrate the \"FP For The Win\" philosophy: using advanced functional programming techniques to create more composable, reusable, and maintainable visualization code." ]
            ]

        -- Section 1: Anscombe's Quartet - The Power of Map
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "anscombe"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "1. Anscombe's Quartet: The Power of Map" ]
            , HH.p_
                [ HH.text "Anscombe's Quartet is a famous dataset demonstrating why visualization matters. All four datasets have "
                , HH.strong_ [ HH.text "identical statistical properties" ]
                , HH.text " (mean, variance, correlation ≈ 0.816), but "
                , HH.strong_ [ HH.text "completely different distributions" ]
                , HH.text ". Statistics can lie; plots don't!"
                ]
            , HH.p_
                [ HH.text "This example showcases functional programming's " ]
            , HH.em_ [ HH.text "map" ]
            , HH.text " pattern: we define "
            , HH.strong_ [ HH.text "one scatterplot component" ]
            , HH.text " and "
            , HH.strong_ [ HH.text "map it over four datasets" ]
            , HH.text ". Same code, different data, four visualizations. This is composability in action:"
            ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ]
                , HP.style "background: #f5f5f5; padding: 15px; border-radius: 5px; color: #333; overflow-x: auto;"
                ]
                [ HH.code_
                    [ HH.text "-- One component definition\nscatterplot :: String -> Array Point -> Tree\nscatterplot name points = ...\n\n-- Map over all four datasets!\nmap (\\{ name, points } -> scatterplot name points) anscombeData" ]
                ]
            , HH.div
                [ HP.id "anscombe-quartet"
                , HP.classes [ HH.ClassName "viz-container" ]
                , HP.style "margin: 20px 0; text-align: center;"
                ]
                []
            , HH.p_
                [ HH.text "Notice how:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Dataset I" ]
                    , HH.text ": Linear relationship with some scatter"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Dataset II" ]
                    , HH.text ": Perfect quadratic curve"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Dataset III" ]
                    , HH.text ": Linear with one outlier skewing statistics"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Dataset IV" ]
                    , HH.text ": No relationship except one influential point"
                    ]
                ]
            , HH.p_
                [ HH.text "The FP win: We wrote the visualization component once and reused it four times. In imperative code, you'd repeat yourself or write complex loops. With "
                , HH.code_ [ HH.text "map" ]
                , HH.text ", composition is natural and type-safe."
                ]
            ]

        -- Section 2: Three Little Sets (moved from Foundations)
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "sets"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "2. Set Operations: Map + Foldable + Phylotaxis" ]
            , HH.p_
                [ HH.text "Sets are unordered, unique collections. This example demonstrates " ]
            , HH.strong_ [ HH.text "three FP wins at once" ]
            , HH.text ":"
            
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Map pattern: " ]
                    , HH.text "One visualizer component, mapped over four different sets (A, B, A ∪ B, A ∩ B)"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Foldable abstraction: " ]
                    , HH.text "We iterate over a Set, not an Array! The visualization works with any Foldable type."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Code reuse: " ]
                    , HH.text "Uses the same phylotaxis (sunflower spiral) layout from our network simulations."
                    ]
                ]
            , HH.p_
                [ HH.text "Each set is visualized as colored circles arranged in a phylotaxis pattern (the mathematical arrangement of seeds in a sunflower). Watch how union combines elements and intersection shows only shared elements:"
                ]
            , HH.div
                [ HP.id "set-operations"
                , HP.classes [ HH.ClassName "viz-container" ]
                , HP.style "margin: 20px 0; text-align: center;"
                ]
                []
            , HH.p_
                [ HH.text "The FP win: This visualization demonstrates that PureScript's type classes let you write generic, reusable code. The same phylotaxis layout function works in network graphs "
                , HH.em_ [ HH.text "and" ]
                , HH.text " set visualizations. In JavaScript, you'd rewrite it each time."
                ]
            ]

        -- Section 3: TreeAPI Language Power
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "tree-api-power"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "3. TreeAPI: A Composable DSL" ]
            , HH.p_
                [ HH.text "The TreeAPI demonstrates functional programming principles in action. It's a deeply embedded DSL (Domain-Specific Language) that leverages PureScript's type system to provide:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Composability: " ]
                    , HH.text "Build complex visualizations from simple, reusable pieces. Trees compose with attributes, which compose with scales and axes."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Type Safety: " ]
                    , HH.text "The compiler ensures you can't attach circle attributes to rect elements, or pass the wrong data type to a layout."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Multiple Interpretations: " ]
                    , HH.text "The same TreeAPI code can be interpreted as SVG, Mermaid diagrams, or even generated JavaScript."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Referential Transparency: " ]
                    , HH.text "Tree descriptions are pure values that can be tested, transformed, and reasoned about before rendering."
                    ]
                ]
            , HH.p_
                [ HH.text "For examples of TreeAPI in action, see the "
                , HH.a
                    [ HP.href $ "#" <> routeToPath TreeAPI ]
                    [ HH.text "Tree API Examples" ]
                , HH.text " page, or explore the "
                , HH.a
                    [ HP.href $ "#" <> routeToPath AnimatedTreeCluster ]
                    [ HH.text "Animated Tree ↔ Cluster" ]
                , HH.text " demonstration which shows how the same tree structure can smoothly transition between different layout algorithms."
                ]
            ]

        -- Conclusion
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Why Functional Programming Wins" ]
            , HH.p_
                [ HH.text "These techniques might seem abstract, but they solve real problems:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Maintainability: " ]
                    , HH.text "Type-safe code means fewer runtime errors and easier refactoring."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Reusability: " ]
                    , HH.text "Contravariant functors and type classes let you write code once and use it with many data types."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Correctness: " ]
                    , HH.text "The compiler catches bugs that would only appear at runtime in JavaScript."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Expressiveness: " ]
                    , HH.text "Foldable, Functor, and other abstractions let you express complex data transformations concisely."
                    ]
                ]
            , HH.p_
                [ HH.text "This is why we say \"Functional Programming For The Win\" - not because it's academically interesting, but because it produces better visualization code that's easier to write, test, and maintain." 
                ]
            ]
        ]
    

