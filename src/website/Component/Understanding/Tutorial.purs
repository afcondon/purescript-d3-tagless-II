module PSD3.Understanding.Tutorial where -- Understanding

import Prelude

import Control.Monad.Rec.Class (forever)
import D3.Viz.ThreeLittleCircles as Circles
import D3.Viz.Parabola as Parabola
import D3.Viz.GUP as GUP
import D3.Viz.BarChart as BarChart
import D3.Viz.LineChart as LineChart
import D3.Viz.ScatterPlot as ScatterPlot
import D3.Viz.Charts.Model (monthlySales, sineWaveData, anscombesQuartet)
import PSD3.Interpreter.D3 (eval_D3M, runD3M)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, killFiber)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.CodeExample (renderCodeExampleSimple)
import PSD3.Shared.SectionNav as SectionNav
import PSD3.RoutingDSL (routeToPath)
import PSD3.Understanding.TOC (renderTOC)
import PSD3.Website.Types (Route(..), Section(..))
import Snippets (readSnippetFiles)
import Type.Proxy (Proxy(..))

-- | Tutorial page state
type State = {
  gupFiber :: Maybe (Fiber Unit)
, threeCirclesSnippet :: Maybe String
, gupSnippet :: Maybe String
, parabolaSnippet :: Maybe String
, barChartSnippet :: Maybe String
, lineChartSnippet :: Maybe String
, quartetSnippet :: Maybe String
}

-- | Tutorial page actions
data Action = Initialize | Finalize

-- | Child component slots
type Slots = ( sectionNav :: forall q. H.Slot q Void Unit )

_sectionNav = Proxy :: Proxy "sectionNav"

-- | Tutorial page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { gupFiber: Nothing
      , threeCirclesSnippet: Nothing
      , gupSnippet: Nothing
      , parabolaSnippet: Nothing
      , barChartSnippet: Nothing
      , lineChartSnippet: Nothing
      , quartetSnippet: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , finalize = Just Finalize
      }
  }

lhsNav :: H.ComponentHTML Action Slots Aff
lhsNav = renderTOC
  { title: "Page Contents"
  , items:
      [ { anchor: "section-1", label: "1. Three Little Circles", level: 0 }
      , { anchor: "section-2", label: "2. General Update Pattern", level: 0 }
      , { anchor: "section-3", label: "3. Data-Driven Positioning", level: 0 }
      , { anchor: "section-4", label: "4. Bar Charts with Scales", level: 0 }
      , { anchor: "section-5", label: "5. Line Charts and Paths", level: 0 }
      , { anchor: "section-6", label: "6. Anscombe's Quartet", level: 0 }
      , { anchor: "section-7", label: "7. Next Steps", level: 0 }
      ]
  , image: Just "images/understanding-bookmark-trees.jpeg"
  }


render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ lhsNav
    -- Navigation Panel (RHS)
    , HH.slot_ _sectionNav unit SectionNav.component
        { currentSection: UnderstandingSection
        , currentRoute: Tutorial
        , sectionPages:
            [ { route: About, label: "About" }
            , { route: Tutorial, label: "Tutorial" }
            , { route: SimpleCharts, label: "Simple Charts" }
            , { route: ChordDiagram, label: "Chord Diagram" }
            , { route: BubbleChart, label: "Bubble Chart" }
            , { route: SankeyDiagram, label: "Sankey Diagram" }
            , { route: Hierarchies, label: "Hierarchies" }
            , { route: Interpreters, label: "Interpreters" }
            , { route: CodeExplorer, label: "Code Explorer" }
            ]
        , moduleCategories: Nothing
        }

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
        , renderCodeExampleSimple
            (fromMaybe "-- Snippet not defined: TLCSimple.purs" state.threeCirclesSnippet)
            "TLCSimple"
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
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "gup-viz" ] ]
                []
            ]
        , renderCodeExampleSimple
            (fromMaybe "-- Snippet not defined: GUP.purs" state.gupSnippet)
            "GUP"
        ]

    -- Section 3: Parabola of Circles
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-3"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "3. Data-Driven Positioning" ]
        , HH.p_
            [ HH.text "This extends the super-simple model in the direction one would go for a more real-world example. In this example, the data is passed in and must match the type specified in the Model. Because the data loses its type information when joined to the DOM elements, we use the datum_ record to provide typed accessors for extracting values." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "parabola-viz" ] ]
                []
            ]
        , renderCodeExampleSimple
            (fromMaybe "-- Snippet not defined: TLCParabola.purs" state.parabolaSnippet)
            "TLCParabola"
        ]

    -- Section 4: Bar Chart
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-4"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "4. Bar Charts with Scales" ]
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
        , renderCodeExampleSimple
            (fromMaybe "-- Snippet not defined: BarChartDraw.purs" state.barChartSnippet)
            "BarChartDraw"
        ]

    -- Section 5: Line Chart
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-5"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "5. Line Charts and Paths" ]
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
        , renderCodeExampleSimple
            (fromMaybe "-- Snippet not defined: LineChartDraw.purs" state.lineChartSnippet)
            "LineChartDraw"
        ]

    -- Section 6: Anscombe's Quartet
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-6"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "6. Anscombe's Quartet" ]
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
        , renderCodeExampleSimple
            (fromMaybe "-- Snippet not defined: ScatterPlotQuartet.purs" state.quartetSnippet)
            "ScatterPlotQuartet"
        ]

    -- Section 7: Next Steps with margin links
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-conclusion" ]
        , HP.id "section-7"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Next Steps" ]
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
    -- Load code snippets
    threeCircles <- H.liftAff $ readSnippetFiles "TLCSimple.purs"
    gup <- H.liftAff $ readSnippetFiles "GUP.purs"
    parabola <- H.liftAff $ readSnippetFiles "TLCParabola.purs"
    barChart <- H.liftAff $ readSnippetFiles "BarChartDraw.purs"
    lineChart <- H.liftAff $ readSnippetFiles "LineChartDraw.purs"
    quartet <- H.liftAff $ readSnippetFiles "ScatterPlotQuartet.purs"

    H.modify_ _ { threeCirclesSnippet = Just threeCircles
                , gupSnippet = Just gup
                , parabolaSnippet = Just parabola
                , barChartSnippet = Just barChart
                , lineChartSnippet = Just lineChart
                , quartetSnippet = Just quartet
                }

    -- Draw Three Little Circles
    _ <- H.liftEffect $ eval_D3M $ Circles.drawThreeCircles "div.three-circles-viz"

    -- Set up General Update Pattern animation
    updateFn <- runGeneralUpdatePattern
    fiber <- H.liftAff $ forkAff $ forever $ runUpdate updateFn
    H.modify_ (\state -> state { gupFiber = Just fiber })

    -- Draw Parabola of Circles
    _ <- H.liftEffect $ eval_D3M $ Parabola.drawWithData [310, 474, 613, 726, 814, 877, 914, 926, 914, 877, 814, 726, 613, 474, 310] "div.parabola-viz"

    -- Draw Bar Chart
    _ <- H.liftEffect $ eval_D3M $ BarChart.draw monthlySales "div.barchart-viz"

    -- Draw Line Chart
    _ <- H.liftEffect $ eval_D3M $ LineChart.draw sineWaveData "div.linechart-viz"

    -- Draw Anscombe's Quartet
    _ <- H.liftEffect $ eval_D3M $ ScatterPlot.drawQuartet anscombesQuartet "div.quartet-viz"

    pure unit

  Finalize -> do
    -- Kill the GUP animation fiber
    maybeFiber <- H.gets _.gupFiber
    case maybeFiber of
      Nothing -> pure unit
      Just fiber -> H.liftAff $ killFiber (error "Cancelling GUP animation") fiber

-- Helper functions for GUP animation
runGeneralUpdatePattern :: forall m. MonadEffect m => m (Array Char -> Aff Unit)
runGeneralUpdatePattern = do
  log "General Update Pattern example"
  update <- H.liftEffect $ eval_D3M $ GUP.exGeneralUpdatePattern "div.gup-viz"
  pure (\letters -> H.liftEffect $ runD3M (update letters) *> pure unit)

runUpdate :: (Array Char -> Aff Unit) -> Aff Unit
runUpdate update = do
  letters <- H.liftEffect $ getLetters
  update letters
  delay (Milliseconds 2300.0)
  where
    -- | choose a string of random letters (no duplicates), ordered alphabetically
    getLetters :: Effect (Array Char)
    getLetters = do
      let
        letters = toCharArray "abcdefghijklmnopqrstuvwxyz"
        coinToss :: Char -> Effect (Maybe Char)
        coinToss c = do
          n <- random
          pure $ if n > 0.6 then Just c else Nothing

      choices <- sequence $ coinToss <$> letters
      pure $ catMaybes choices

