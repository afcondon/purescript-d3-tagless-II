module PSD3.Understanding.SimpleCharts1 where -- Understanding

import Prelude

import D3.Viz.BarChart as BarChart
import D3.Viz.Charts.Model (monthlySales, sineWaveData, anscombesQuartet)
import D3.Viz.LineChart as LineChart
import D3.Viz.Parabola as Parabola
import D3.Viz.ScatterPlot as ScatterPlot
import D3.Viz.ThreeLittleCircles as Circles
import D3.Viz.ThreeLittleDimensions as Dimensions
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Interpreter.D3 (eval_D3M)
import PSD3.RoutingDSL (routeToPath)
import CodeSnippet (codeSnippet, triggerPrismHighlighting)
import PSD3.Shared.ExamplesNav as ExamplesNav
import PSD3.Understanding.TOC (renderTOC, tocAnchor, tocRoute)
import PSD3.Website.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | Tutorial page state
type State = Unit

-- | Tutorial page actions
data Action = Initialize

-- | Child component slots
type Slots = ( examplesNav :: forall q. H.Slot q Void Unit )

_examplesNav = Proxy :: Proxy "examplesNav"

-- | Tutorial page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

lhsNav :: H.ComponentHTML Action Slots Aff
lhsNav = renderTOC
  { title: "Page Contents"
  , items:
      [ tocAnchor "section-1" "1. Three Little Circles" 0
      , tocRoute (Explore "TLCSimple") "→ How-to guide" 1
      , tocAnchor "section-2" "2. Data-Driven Positioning" 0
      , tocRoute (Explore "TLCParabola") "→ How-to guide" 1
      , tocAnchor "section-3" "3. Bar Charts with Scales" 0
      , tocRoute (Explore "BarChartDraw") "→ How-to guide" 1
      , tocAnchor "section-4" "4. Line Charts and Paths" 0
      , tocRoute (Explore "LineChartDraw") "→ How-to guide" 1
      , tocAnchor "section-5" "5. Anscombe's Quartet" 0
      , tocRoute (Explore "ScatterPlotQuartet") "→ How-to guide" 1
      , tocAnchor "section-6" "6. Next Steps" 0
      ]
  , image: Just "images/understanding-bookmark-trees.jpeg"
  }


render :: State -> H.ComponentHTML Action Slots Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ lhsNav
    -- Navigation Panel (RHS) - unified examples navigation
    , HH.slot_ _examplesNav unit ExamplesNav.component SimpleCharts1

    -- Tutorial introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Tutorial: Building Visualizations with PureScript D3" ]
        , HH.p_
            [ HH.text "We'll show just the very simplest examples of putting elements in the DOM, in this case into an SVG, using the PS<$>D3 grammar." ]
        , HH.p_
            [ HH.text "In the How-to manual we'll explain in detail what each of these lines means, but if you're already familiar with D3.js the shape of the code should look very familiar." ]
        ]

    -- Section 1: Three Little Circles
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-1"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. Three Little Circles" ]
        , HH.p_
            [ HH.text "Simplest possible example, just to show syntax." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "three-circles-viz" ] ]
                []
            ]
        -- SNIPPET: TLCSimple src/website/Viz/ThreeLittleCircles/ThreeLittleCircles.purs 16-31
        , codeSnippet "TLCSimple" "haskell"
        ]

    -- Section 1b: Three Little Dimensions (Nested Data Binding)
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-1b"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1b. Nested Data Binding: Three Little Dimensions" ]
        , HH.p_
            [ HH.text "This example demonstrates nested data binding, where child elements derive their data from the parent element's bound datum. We take a 2D array [[1,2,3],[4,5,6],[7,8,9]] and create a nested structure using a proper HTML table: rows bound to outer arrays, cells bound to inner arrays." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "three-dimensions-viz" ] ]
                []
            ]
        -- SNIPPET: ThreeDimensions src/website/Viz/ThreeLittleDimensions/ThreeLittleDimensions.purs 16-38
        , codeSnippet "ThreeDimensions" "haskell"
        , HH.h3_
            [ HH.text "Beyond Arrays: Working with Sets" ]
        , HH.p_
            [ HH.text "The real power of PS<$>D3's nestedJoin is its Foldable constraint. This means you can use ANY Foldable type for nested data, not just Arrays. Below, we use Sets (unordered, unique collections) to represent product categories or tags. Notice how some products have many tags, some have few, and some have none - the library handles all cases gracefully." ]
        , HH.p_
            [ HH.text "This flexibility goes far beyond standard D3.js, where nested selections only work with arrays. With PureScript's type classes, the same visualization code works with Sets, Lists, Maps, or any custom Foldable you define." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "three-dimensions-sets-viz" ] ]
                []
            ]
        -- SNIPPET: ThreeDimensionsSets src/website/Viz/ThreeLittleDimensions/ThreeLittleDimensions.purs 40-73
        , codeSnippet "ThreeDimensionsSets" "haskell"
        ]

    -- Section 2: Parabola of Circles
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-2"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "2. Data-Driven Positioning" ]
        , HH.p_
            [ HH.text "This extends the super-simple model in the direction one would go for a more real-world example. In this example, the data is passed in and must match the type specified in the Model. Because the data loses its type information when joined to the DOM elements, we use the datum_ record to provide typed accessors for extracting values." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "parabola-viz" ] ]
                []
            ]
        -- SNIPPET: TLCParabola src/website/Viz/Parabola/Parabola.purs 33-48
        , codeSnippet "TLCParabola" "haskell"
        ]

    -- Section 3: Bar Chart
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-3"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "3. Bar Charts with Scales" ]
        , HH.p_
            [ HH.text "Bar charts are ideal for comparing discrete categories or showing changes across time periods. They use rectangular bars with heights or lengths proportional to the values they represent." ]
        , HH.p_
            [ HH.text "This example shows monthly sales data using a vertical bar chart. Each bar represents a month, and the height indicates the sales value. The implementation uses D3 scales to map data values to pixel coordinates." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "barchart-viz" ] ]
                []
            ]
        -- SNIPPET: BarChartDraw src/website/Viz/Charts/BarChart.purs 39-115
        , codeSnippet "BarChartDraw" "haskell"
        ]

    -- Section 4: Line Chart
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-4"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "4. Line Charts and Paths" ]
        , HH.p_
            [ HH.text "Line charts are one of the most fundamental visualizations for showing trends over time or continuous data. They excel at displaying patterns, trends, and changes in data series." ]
        , HH.p_
            [ HH.text "This example demonstrates a simple line chart showing a sine wave pattern. The implementation uses D3's scale functions to map data values to pixel coordinates, and a line generator to create the SVG path." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "linechart-viz" ] ]
                []
            ]
        -- SNIPPET: LineChartDraw src/website/Viz/Charts/LineChart.purs 41-101
        , codeSnippet "LineChartDraw" "haskell"
        ]

    -- Section 5: Anscombe's Quartet
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-5"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "5. Anscombe's Quartet" ]
        , HH.p_
            [ HH.text "This example demonstrates Anscombe's Quartet, a famous dataset created by statistician Francis Anscombe in 1973. All four datasets have nearly identical statistical properties (same mean, variance, correlation, and linear regression line), yet when visualized they reveal completely different patterns." ]
        , HH.p_
            [ HH.text "The quartet powerfully illustrates why data visualization is essential. Summary statistics alone can be misleading - you need to look at the data to understand its true structure. This implementation uses a 'small multiples' layout, displaying the four related charts side-by-side for easy comparison." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "quartet-viz" ] ]
                []
            ]
        -- SNIPPET: ScatterPlotQuartet src/website/Viz/Charts/ScatterPlot.purs 106-197
        , codeSnippet "ScatterPlotQuartet" "haskell"
        ]

    -- Section 6: Next Steps with margin links
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-conclusion" ]
        , HP.id "section-6"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "6. Next Steps" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed consequat, leo eget bibendum sodales, augue velit cursus nunc, quis gravida magna mi a libero. Fusce vulputate eleifend sapien." ]

        -- Contextual "learn more" links
        , HH.aside
            [ HP.classes [ HH.ClassName "tutorial-margin-note" ] ]
            [ HH.p
                [ HP.classes [ HH.ClassName "tutorial-margin-note__label" ] ]
                [ HH.text "Learn More" ]
            , HH.a
                [ HP.href $ "#" <> routeToPath Hierarchies
                , HP.classes [ HH.ClassName "tutorial-margin-note__link" ]
                ]
                [ HH.text "Hierarchies →" ]
            ]

        , HH.ul_
            [ HH.li_ [ HH.text "Explore hierarchical data visualizations" ]
            , HH.li_ [ HH.text "Learn about the Finally Tagless pattern with interpreters" ]
            , HH.li_ [ HH.text "Dive into the Code Explorer for complex applications" ]
            ]

        -- More contextual links
        , HH.aside
            [ HP.classes [ HH.ClassName "tutorial-margin-note" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath Interpreters
                , HP.classes [ HH.ClassName "tutorial-margin-note__link" ]
                ]
                [ HH.text "Interpreters →" ]
            , HH.a
                [ HP.href $ "#" <> routeToPath CodeExplorer
                , HP.classes [ HH.ClassName "tutorial-margin-note__link" ]
                ]
                [ HH.text "Code Explorer →" ]
            ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Trigger Prism highlighting for code snippets
    triggerPrismHighlighting

    -- Draw Three Little Circles
    _ <- H.liftEffect $ eval_D3M $ Circles.drawThreeCircles "div.three-circles-viz"

    -- Draw Three Little Dimensions
    _ <- H.liftEffect $ eval_D3M $ Dimensions.drawThreeDimensions "div.three-dimensions-viz"

    -- Draw Three Little Dimensions with Sets
    _ <- H.liftEffect $ eval_D3M $ Dimensions.drawThreeDimensionsSets "div.three-dimensions-sets-viz"

    -- Draw Parabola of Circles
    _ <- H.liftEffect $ eval_D3M $ Parabola.drawWithData [310, 474, 613, 726, 814, 877, 914, 926, 914, 877, 814, 726, 613, 474, 310] "div.parabola-viz"

    -- Draw Bar Chart
    _ <- H.liftEffect $ eval_D3M $ BarChart.draw monthlySales "div.barchart-viz"

    -- Draw Line Chart
    _ <- H.liftEffect $ eval_D3M $ LineChart.draw sineWaveData "div.linechart-viz"

    -- Draw Anscombe's Quartet
    _ <- H.liftEffect $ eval_D3M $ ScatterPlot.drawQuartet anscombesQuartet "div.quartet-viz"

    pure unit

