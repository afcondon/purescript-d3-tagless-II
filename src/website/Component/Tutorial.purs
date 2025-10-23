module PSD3.Tutorial where

import Prelude

import Control.Monad.Rec.Class (forever)
import D3.Viz.ThreeLittleCircles as Circles
import D3.Viz.GUP as GUP
import D3.Viz.BarChart as BarChart
import D3.Viz.LineChart as LineChart
import D3.Viz.ScatterPlot as ScatterPlot
import D3.Viz.Charts.Model (monthlySales, sineWaveData, anscombesQuartet)
import D3Tagless.Instance.Selection (eval_D3M, runD3M)
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
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
import PSD3.RoutingDSL (routeToPath)
import PSD3.Types (Route(..))
import PSD3.Utilities (syntaxHighlightedCode)

-- | Tutorial page state
type State = {
  gupFiber :: Maybe (Fiber Unit)
}

-- | Tutorial page actions
data Action = Initialize | Finalize

-- | Tutorial page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> { gupFiber: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , finalize = Just Finalize
      }
  }

render :: State -> H.ComponentHTML Action () Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ -- TOC Panel (LHS)
      HH.div
        [ HP.classes [ HH.ClassName "toc-panel" ] ]
        [ HH.img
            [ HP.src "bookmark.jpeg"
            , HP.alt ""
            , HP.classes [ HH.ClassName "toc-panel__bookmark-pin" ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "toc-panel__main" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "floating-panel__header" ] ]
                [ HH.h3
                    [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                    [ HH.text "Contents" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "floating-panel__toggle" ]
                    , HP.type_ HP.ButtonButton
                    ]
                    [ HH.text "−" ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "floating-panel__content", HH.ClassName "toc-panel__content" ] ]
                [ HH.nav
                    [ HP.classes [ HH.ClassName "toc-nav" ] ]
                    [ HH.a [ HP.href "#section-1", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "1. Three Little Circles" ]
                    , HH.a [ HP.href "#section-2", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "2. General Update Pattern" ]
                    , HH.a [ HP.href "#section-3", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "3. Data-Driven Positioning" ]
                    , HH.a [ HP.href "#section-4", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "4. Bar Charts with Scales" ]
                    , HH.a [ HP.href "#section-5", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "5. Line Charts and Paths" ]
                    , HH.a [ HP.href "#section-6", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "6. Anscombe's Quartet" ]
                    , HH.a [ HP.href "#section-7", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "7. Next Steps" ]
                    ]
                ]
            ]
        ]

    -- Navigation Panel (RHS)
    , HH.div
        [ HP.classes [ HH.ClassName "tutorial-page__nav-panel" ] ]
        [ HH.h3
            [ HP.classes [ HH.ClassName "tutorial-page__nav-title" ] ]
            [ HH.text "Explore" ]
        , HH.nav
            [ HP.classes [ HH.ClassName "tutorial-page__nav-links" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath About
                , HP.classes [ HH.ClassName "tutorial-page__nav-link" ]
                ]
                [ HH.text "About →" ]
            , HH.a
                [ HP.href $ "#" <> routeToPath Hierarchies
                , HP.classes [ HH.ClassName "tutorial-page__nav-link" ]
                ]
                [ HH.text "Hierarchies →" ]
            , HH.a
                [ HP.href $ "#" <> routeToPath Interpreters
                , HP.classes [ HH.ClassName "tutorial-page__nav-link" ]
                ]
                [ HH.text "Interpreters →" ]
            , HH.a
                [ HP.href $ "#" <> routeToPath CodeExplorer
                , HP.classes [ HH.ClassName "tutorial-page__nav-link" ]
                ]
                [ HH.text "Code Explorer →" ]
            , HH.a
                [ HP.href "https://github.com/afcondon/purescript-d3-tagless"
                , HP.target "_blank"
                , HP.rel "noopener noreferrer"
                , HP.classes [ HH.ClassName "tutorial-page__nav-link", HH.ClassName "tutorial-page__nav-link--external" ]
                ]
                [ HH.text "GitHub ↗" ]
            ]
        ]

    -- Tutorial introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Tutorial: Building Visualizations with PureScript D3" ]
        , HH.p_
            [ HH.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat." ]
        , HH.p_
            [ HH.text "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." ]
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
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-code-block" ] ]
            (syntaxHighlightedCode threeCirclesCode)
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
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-code-block" ] ]
            (syntaxHighlightedCode gupCode)
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
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-code-block" ] ]
            (syntaxHighlightedCode parabolaCode)
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
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-code-block" ] ]
            (syntaxHighlightedCode barChartCode)
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
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-code-block" ] ]
            (syntaxHighlightedCode lineChartCode)
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
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-code-block" ] ]
            (syntaxHighlightedCode quartetCode)
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

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Draw Three Little Circles
    _ <- H.liftEffect $ eval_D3M $ Circles.drawThreeCircles "div.three-circles-viz"

    -- Set up General Update Pattern animation
    updateFn <- runGeneralUpdatePattern
    fiber <- H.liftAff $ forkAff $ forever $ runUpdate updateFn
    H.modify_ (\state -> state { gupFiber = Just fiber })

    -- Draw Parabola of Circles
    _ <- H.liftEffect $ eval_D3M $ Circles.drawWithData [310, 474, 613, 726, 814, 877, 914, 926, 914, 877, 814, 726, 613, 474, 310] "div.parabola-viz"

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

-- Code snippets for tutorial examples
threeCirclesCode :: String
threeCirclesCode = """drawThreeCircles :: forall m. SelectionM D3Selection_ m => Selector D3Selection_-> m D3Selection_
drawThreeCircles selector = do
  let circleAttributes = [ fill "green", cx xFromIndex, cy 50.0, radius 20.0 ]

  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox (-10.0) 20.0 120.0 60.0, classed "d3svg" ]
  circleGroup <- appendTo svg  Group []
  circles     <- simpleJoin circleGroup Circle [32, 57, 293] keyIsID_
  setAttributes circles circleAttributes

  pure circles"""

gupCode :: String
gupCode = """exGeneralUpdatePattern :: forall m. SelectionM D3Selection_ m => Selector D3Selection_ -> m ((Array Char) -> m D3Selection_)
exGeneralUpdatePattern selector = do
  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox 0.0 100.0 800.0 350.0, classed "d3svg gup" ]
  letterGroup <- appendTo svg Group []

  pure $ \\letters -> do
    enterSelection   <- openSelection letterGroup "text"
    updateSelections <- updateJoin enterSelection Text letters keyFunction
    setAttributes updateSelections.exit exit
    setAttributes updateSelections.update update
    newlyEntered     <- appendTo updateSelections.enter Text []
    setAttributes newlyEntered enter
    pure newlyEntered
  where
    transition = transitionWithDuration $ Milliseconds 2000.0
    enter  = [ classed "enter", fill "green", x xFromIndex, y 0.0, text (singleton <<< datumIsChar), fontSize 96.0 ]
             `andThen` (transition `to` [ y 200.0 ])
    update = [ classed "update", fill "gray", y 200.0 ]
             `andThen` (transition `to` [ x xFromIndex ])
    exit   = [ classed "exit", fill "brown"]
             `andThen` (transition `to` [ y 400.0, remove ])"""

parabolaCode :: String
parabolaCode = """drawWithData :: forall m. SelectionM D3Selection_ m => Model -> Selector D3Selection_ -> m D3Selection_
drawWithData circleData selector = do
  let circleAttributes = [
      strokeColor datum_.color
    , strokeWidth 3.0
    , fill "none"
    , cx datum_.x
    , cy datum_.y
    , radius 10.0 ]

  root        <- attach selector
  svg         <- appendTo root Svg [ viewBox (-10.0) (-100.0) 320.0 160.0, classed "d3svg" ]
  circleGroup <- appendTo svg  Group []
  circles     <- simpleJoin circleGroup Circle circleData keyIsID_
  setAttributes circles circleAttributes
  pure circles"""

barChartCode :: String
barChartCode = """draw :: Array DataPoint -> Selector D3Selection_ -> m Unit
draw dataPoints selector = do
  let dims = defaultDimensions

  root <- attach selector
  svg <- appendTo root Svg [ viewBox 0.0 0.0 dims.width dims.height, classed "bar-chart" ]
  chartGroup <- appendTo svg Group [
      transform [ \\_ -> "translate(" <> show dims.margin.left <> "," <> show dims.margin.top <> ")" ]
    ]

  -- Create scales
  xScale <- liftEffect $ createLinearScale { domain: [minX, maxX], range: [0.0, iWidth] }
  yScale <- liftEffect $ createLinearScale { domain: [minY, maxY], range: [iHeight, 0.0] }

  -- Add axes
  xAxisGroup <- appendTo chartGroup Group [ classed "x-axis", transform [ \\_ -> "translate(0," <> show iHeight <> ")" ] ]
  yAxisGroup <- appendTo chartGroup Group [ classed "y-axis" ]
  _ <- liftEffect $ callAxis xAxisGroup (axisBottom xScale)
  _ <- liftEffect $ callAxis yAxisGroup (axisLeft yScale)

  -- Add bars
  let addBar point = do
        let xPos = applyScale xScale point.x - (barWidth / 2.0)
        let yPos = applyScale yScale point.y
        let barHeight = iHeight - yPos
        _ <- appendTo chartGroup Rect [ x xPos, y yPos, width barWidth, height barHeight, fill "#4a90e2" ]
        pure unit
  _ <- traverse_ addBar dataPoints
  pure unit"""

lineChartCode :: String
lineChartCode = """draw :: Array DataPoint -> Selector D3Selection_ -> m Unit
draw dataPoints selector = do
  let dims = defaultDimensions

  root <- attach selector
  svg <- appendTo root Svg [ viewBox 0.0 0.0 dims.width dims.height, classed "line-chart" ]
  chartGroup <- appendTo svg Group [
      transform [ \\_ -> "translate(" <> show dims.margin.left <> "," <> show dims.margin.top <> ")" ]
    ]

  -- Create scales
  xScale <- liftEffect $ createLinearScale { domain: [minX, maxX], range: [0.0, iWidth] }
  yScale <- liftEffect $ createLinearScale { domain: [minY, maxY], range: [iHeight, 0.0] }

  -- Add axes
  xAxisGroup <- appendTo chartGroup Group [ classed "x-axis", transform [ \\_ -> "translate(0," <> show iHeight <> ")" ] ]
  yAxisGroup <- appendTo chartGroup Group [ classed "y-axis" ]
  _ <- liftEffect $ callAxis xAxisGroup (axisBottom xScale)
  _ <- liftEffect $ callAxis yAxisGroup (axisLeft yScale)

  -- Create line generator and add the line path
  lineGen <- liftEffect $ createLineGenerator { xScale, yScale }
  let pathData = generateLinePath lineGen dataPoints
  _ <- appendTo chartGroup Path [ d pathData, fill "none", strokeColor "#4a90e2", strokeWidth 2.0, classed "line" ]
  pure unit"""

quartetCode :: String
quartetCode = """drawQuartet :: QuartetData -> Selector D3Selection_ -> m Unit
drawQuartet quartet selector = do
  -- Overall dimensions for the quartet display (2x2 grid of small multiples)
  let totalWidth = 900.0
  let totalHeight = 700.0

  root <- attach selector
  svg <- appendTo root Svg [ viewBox 0.0 0.0 totalWidth totalHeight, classed "scatter-quartet" ]

  -- Helper function to draw a single subplot
  let drawSubplot title dataPoints xOffset yOffset = do
        subplotGroup <- appendTo svg Group [ classed "subplot", transform [ \\_ -> "translate(" <> show xOffset <> "," <> show yOffset <> ")" ] ]

        -- Add title
        _ <- appendTo svg Text [ x (xOffset + plotWidth / 2.0), y (yOffset + 15.0), text title, textAnchor "middle" ]

        -- Create scales (use fixed domains for valid comparison)
        xScale <- liftEffect $ createLinearScale { domain: [0.0, 20.0], range: [0.0, iWidth] }
        yScale <- liftEffect $ createLinearScale { domain: [0.0, 14.0], range: [iHeight, 0.0] }

        -- Add axes and data points...

  -- Draw all four subplots in a 2x2 grid
  _ <- drawSubplot "Dataset I" quartet.dataset1 padding padding
  _ <- drawSubplot "Dataset II" quartet.dataset2 (padding + plotWidth + padding) padding
  _ <- drawSubplot "Dataset III" quartet.dataset3 padding (padding + plotHeight + padding)
  _ <- drawSubplot "Dataset IV" quartet.dataset4 (padding + plotWidth + padding) (padding + plotHeight + padding)
  pure unit"""
