-- | Simpson's Paradox Visualization - Halogen Application
-- |
-- | A PureScript port of the classic Simpson's Paradox visualization
-- | by Lewis Lehe & Victor Powell (2014): https://setosa.io/simpsons/
module D3.Viz.Simpsons.App
  ( component
  , Query
  ) where

import Prelude

import D3.Viz.Simpsons.DataTable as DataTable
import D3.Viz.Simpsons.DonutChart as Donut
import D3.Viz.Simpsons.ForceViz as ForceViz
import D3.Viz.Simpsons.LineChart as Line
import D3.Viz.Simpsons.ScatterChart as Scatter
import D3.Viz.Simpsons.Types (DerivedData, Proportions, defaultProportions, deriveData, green, overallAcceptanceRates, purple)
import Data.Array (mapWithIndex) as Array
import Data.Int (round) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3v2.Capabilities.Selection (renderTree, select)
import PSD3v2.Interpreter.D3v2 (D3v2M, runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import PSD3v3.Expr (lit, str)
import PSD3v3.Integration (v3Attr, v3AttrStr)
import Web.DOM.Element (Element)

-- =============================================================================
-- Module-level state for force visualization handle
-- =============================================================================

-- | Mutable ref holding the ForceViz handle (set during initialization)
forceVizHandleRef :: Ref.Ref (Maybe ForceViz.ForceVizHandle)
forceVizHandleRef = unsafePerformEffect $ Ref.new Nothing

-- =============================================================================
-- Component Types
-- =============================================================================

-- | Component state
type State =
  { proportions :: Proportions
  , isCombined :: Boolean
  , initialized :: Boolean
  }

-- | Initial state
initialState :: forall i. i -> State
initialState _ =
  { proportions: defaultProportions
  , isCombined: false
  , initialized: false
  }

-- | Component actions
data Action
  = Initialize
  | SetMaleProportion Number
  | SetFemaleProportion Number
  | ToggleCombined
  | Render

-- | Query type (empty for now)
data Query a = NoOp a

-- =============================================================================
-- Component
-- =============================================================================

component :: forall i o m. MonadAff m => MonadEffect m => H.Component Query i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    derived = deriveData state.proportions
  in
    HH.div
      [ HP.classes [ HH.ClassName "simpsons-paradox" ] ]
      [ -- Header
        renderHeader

      -- Donut charts (constant - showing Berkeley stats)
      , renderDonutSection

      -- Explanation with scatter plot
      , renderScatterSection

      -- Interactive line chart with sliders
      , renderInteractiveSection state derived

      -- Data table
      , renderDataTable state derived

      -- Force-directed visualization
      , renderForceSection state

      -- Credits
      , renderCredits
      ]

-- | Header section
renderHeader :: forall w i. HH.HTML w i
renderHeader =
  HH.header
    [ HP.classes [ HH.ClassName "simpsons-header" ] ]
    [ HH.h1_ [ HH.text "Simpson's Paradox" ]
    , HH.p_
        [ HH.text "How grouped and combined statistics can show opposite trends" ]
    ]

-- | Donut charts showing overall acceptance rates
renderDonutSection :: forall w i. HH.HTML w i
renderDonutSection =
  HH.section
    [ HP.classes [ HH.ClassName "simpsons-donuts" ] ]
    [ HH.h2_ [ HH.text "UC Berkeley Graduate Admissions (1973)" ]
    , HH.p_
        [ HH.text "The data initially appeared to show discrimination against women:" ]
    , HH.div
        [ HP.classes [ HH.ClassName "simpsons-donuts-container" ] ]
        [ HH.div
            [ HP.id "donut-men"
            , HP.classes [ HH.ClassName "donut-chart" ]
            ]
            []
        , HH.div
            [ HP.id "donut-women"
            , HP.classes [ HH.ClassName "donut-chart" ]
            ]
            []
        ]
    , HH.p
        [ HP.classes [ HH.ClassName "simpsons-paradox-note" ] ]
        [ HH.text "But wait... when you look at admission rates "
        , HH.em_ [ HH.text "by department" ]
        , HH.text ", women were actually admitted at "
        , HH.strong_ [ HH.text "higher rates" ]
        , HH.text " in most departments!"
        ]
    ]

-- | Scatter chart explaining the paradox
renderScatterSection :: forall w i. HH.HTML w i
renderScatterSection =
  HH.section
    [ HP.classes [ HH.ClassName "simpsons-scatter" ] ]
    [ HH.h2_ [ HH.text "The Paradox Explained" ]
    , HH.p_
        [ HH.text "Simpson's Paradox occurs when grouped data shows one trend, but combined data shows the opposite." ]
    , HH.div
        [ HP.id "scatter-chart"
        , HP.classes [ HH.ClassName "scatter-chart" ]
        ]
        []
    , HH.p_
        [ HH.text "In each color group, y increases with x. But overall, y decreases with x!" ]
    ]

-- | Interactive line chart with sliders
renderInteractiveSection :: forall m. State -> DerivedData -> H.ComponentHTML Action () m
renderInteractiveSection state derived =
  HH.section
    [ HP.classes [ HH.ClassName "simpsons-interactive" ] ]
    [ HH.h2_ [ HH.text "Interactive Demonstration" ]
    , HH.p_
        [ HH.text "Adjust the sliders to see how the distribution of applicants affects overall rates." ]

    -- Sliders
    , HH.div
        [ HP.classes [ HH.ClassName "simpsons-sliders" ] ]
        [ renderSlider "Women to Easy Dept" state.proportions.easyFemale green SetFemaleProportion
        , renderSlider "Men to Easy Dept" state.proportions.easyMale purple SetMaleProportion
        ]

    -- Line chart
    , HH.div
        [ HP.id "line-chart"
        , HP.classes [ HH.ClassName "line-chart" ]
        ]
        []

    -- Paradox indicator
    , HH.div
        [ HP.classes
            [ HH.ClassName "simpsons-paradox-indicator"
            , HH.ClassName if derived.isParadox then "is-paradox" else "no-paradox"
            ]
        ]
        [ if derived.isParadox
            then HH.text "Simpson's Paradox is present! Men have higher overall rate despite lower per-department rates."
            else HH.text "No paradox - the group with higher per-department rates also has higher overall rate."
        ]
    ]

-- | Render a slider control
renderSlider :: forall m. String -> Number -> String -> (Number -> Action) -> H.ComponentHTML Action () m
renderSlider label value color action =
  HH.div
    [ HP.classes [ HH.ClassName "simpsons-slider" ] ]
    [ HH.label
        [ HP.style ("color: " <> color) ]
        [ HH.text label ]
    , HH.input
        [ HP.type_ HP.InputRange
        , HP.min 0.0
        , HP.max 100.0
        , HP.value (show (value * 100.0))
        , HP.step (HP.Step 1.0)
        , HP.style ("accent-color: " <> color)
        , HE.onValueInput \val ->
            action (fromMaybe (value * 100.0) (Number.fromString val) / 100.0)
        ]
    , HH.span_ [ HH.text (show (Int.round (value * 100.0)) <> "%") ]
    ]

-- | Data table showing current statistics with bar charts and donuts
renderDataTable :: forall w i. State -> DerivedData -> HH.HTML w i
renderDataTable _state derived =
  let
    rows = DataTable.buildRowData derived
  in
    HH.section
      [ HP.classes [ HH.ClassName "simpsons-table" ] ]
      [ HH.h2_ [ HH.text "Illustration" ]
      , HH.p
          [ HP.classes [ HH.ClassName "simpsons-table-description" ] ]
          [ HH.text "Suppose there are two departments: one easy, one hard ('hard' as in 'hard to get into'). The sliders below set what percentage each gender applies to the easy department. Both departments prefer women, but if too many women apply to the hard one, their acceptance rate drops below the men's." ]
      , HH.table
          [ HP.classes [ HH.ClassName "simpsons-data-table" ] ]
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "departments" ]
                  , HH.th
                      [ HP.colSpan 2 ]
                      [ HH.text "# applied" ]
                  , HH.th
                      [ HP.colSpan 2 ]
                      [ HH.text "# admitted" ]
                  , HH.th
                      [ HP.colSpan 2 ]
                      [ HH.text "% admitted" ]
                  ]
              , HH.tr
                  [ HP.classes [ HH.ClassName "simpsons-subheader" ] ]
                  [ HH.th_ []
                  , HH.th
                      [ HP.style ("color: " <> purple) ]
                      [ HH.text "men" ]
                  , HH.th
                      [ HP.style ("color: " <> green) ]
                      [ HH.text "women" ]
                  , HH.th
                      [ HP.style ("color: " <> purple) ]
                      [ HH.text "men" ]
                  , HH.th
                      [ HP.style ("color: " <> green) ]
                      [ HH.text "women" ]
                  , HH.th
                      [ HP.style ("color: " <> purple) ]
                      [ HH.text "men" ]
                  , HH.th
                      [ HP.style ("color: " <> green) ]
                      [ HH.text "women" ]
                  ]
              ]
          , HH.tbody_
              ( rows # Array.mapWithIndex \idx row ->
                  let
                    prefix = "row-" <> show idx
                  in
                    HH.tr_
                      [ -- Department name
                        HH.td
                          [ HP.classes [ HH.ClassName "simpsons-dept-name" ] ]
                          [ HH.text row.department ]
                      , -- Men applied
                        HH.td
                          [ HP.classes [ HH.ClassName "simpsons-data-cell" ] ]
                          [ HH.span_ [ HH.text (formatNumber row.maleApplied) ]
                          , HH.div
                              [ HP.id (prefix <> "-male-applied")
                              , HP.classes [ HH.ClassName "bar-container" ]
                              ]
                              []
                          ]
                      , -- Women applied
                        HH.td
                          [ HP.classes [ HH.ClassName "simpsons-data-cell" ] ]
                          [ HH.span_ [ HH.text (formatNumber row.femaleApplied) ]
                          , HH.div
                              [ HP.id (prefix <> "-female-applied")
                              , HP.classes [ HH.ClassName "bar-container" ]
                              ]
                              []
                          ]
                      , -- Men admitted
                        HH.td
                          [ HP.classes [ HH.ClassName "simpsons-data-cell" ] ]
                          [ HH.span_ [ HH.text (formatNumber row.maleAdmitted) ]
                          , HH.div
                              [ HP.id (prefix <> "-male-admitted")
                              , HP.classes [ HH.ClassName "bar-container" ]
                              ]
                              []
                          ]
                      , -- Women admitted
                        HH.td
                          [ HP.classes [ HH.ClassName "simpsons-data-cell" ] ]
                          [ HH.span_ [ HH.text (formatNumber row.femaleAdmitted) ]
                          , HH.div
                              [ HP.id (prefix <> "-female-admitted")
                              , HP.classes [ HH.ClassName "bar-container" ]
                              ]
                              []
                          ]
                      , -- Men rate donut
                        HH.td
                          [ HP.classes [ HH.ClassName "simpsons-donut-cell" ] ]
                          [ HH.div
                              [ HP.id (prefix <> "-male-rate")
                              , HP.classes [ HH.ClassName "donut-container" ]
                              ]
                              []
                          ]
                      , -- Women rate donut
                        HH.td
                          [ HP.classes [ HH.ClassName "simpsons-donut-cell" ] ]
                          [ HH.div
                              [ HP.id (prefix <> "-female-rate")
                              , HP.classes [ HH.ClassName "donut-container" ]
                              ]
                              []
                          ]
                      ]
              )
          ]
      , HH.div
          [ HP.classes
              [ HH.ClassName "simpsons-paradox-result"
              , HH.ClassName if derived.isParadox then "is-paradox" else "no-paradox"
              ]
          ]
          [ HH.strong_ [ HH.text "Simpson's paradox? " ]
          , HH.span_ [ HH.text if derived.isParadox then "yes" else "no" ]
          ]
      ]

-- | Format a number with commas for display
formatNumber :: Number -> String
formatNumber n = formatWithCommas (show (Int.round n))

foreign import formatWithCommas :: String -> String

-- | Force-directed visualization section
renderForceSection :: forall m. State -> H.ComponentHTML Action () m
renderForceSection state =
  HH.section
    [ HP.classes [ HH.ClassName "simpsons-force" ] ]
    [ HH.h2_ [ HH.text "Applicant Cohorts" ]
    , HH.p_
        [ HH.text "Each dot represents an applicant. Blue = accepted, Red = rejected." ]
    , HH.button
        [ HP.classes [ HH.ClassName "simpsons-toggle" ]
        , HE.onClick \_ -> ToggleCombined
        ]
        [ HH.text if state.isCombined then "Separate by Department" else "Combine All" ]
    , HH.div
        [ HP.id "force-viz"
        , HP.classes [ HH.ClassName "force-viz" ]
        ]
        []
    ]

-- | Credits section
renderCredits :: forall w i. HH.HTML w i
renderCredits =
  HH.footer
    [ HP.classes [ HH.ClassName "simpsons-credits" ] ]
    [ HH.p_
        [ HH.text "Based on the visualization by "
        , HH.a
            [ HP.href "https://setosa.io/simpsons/"
            , HP.target "_blank"
            ]
            [ HH.text "Lewis Lehe & Victor Powell" ]
        , HH.text " (2014). Reimplemented in PureScript using "
        , HH.a
            [ HP.href "https://github.com/psd3/psd3"
            , HP.target "_blank"
            ]
            [ HH.text "PS<$>D3" ]
        , HH.text "."
        ]
    ]

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall o m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Give Halogen time to render the DOM
    H.liftAff $ Aff.delay (Milliseconds 100.0)

    -- Initialize all charts
    state <- H.get
    let derived = deriveData state.proportions
    liftEffect $ initializeCharts derived
    H.modify_ _ { initialized = true }

    -- Initial render
    handleAction Render

  SetMaleProportion value -> do
    H.modify_ \s -> s { proportions = s.proportions { easyMale = value } }
    handleAction Render

  SetFemaleProportion value -> do
    H.modify_ \s -> s { proportions = s.proportions { easyFemale = value } }
    handleAction Render

  ToggleCombined -> do
    H.modify_ \s -> s { isCombined = not s.isCombined }
    -- Toggle the force visualization
    liftEffect do
      maybeHandle <- Ref.read forceVizHandleRef
      case maybeHandle of
        Just handle -> handle.toggle
        Nothing -> pure unit

  Render -> do
    state <- H.get
    when state.initialized do
      let derived = deriveData state.proportions
      liftEffect do
        updateLineChart state.proportions
        DataTable.updateDataTable derived

-- =============================================================================
-- Chart Initialization
-- =============================================================================

-- | Initialize all static charts
initializeCharts :: DerivedData -> Effect Unit
initializeCharts derived = do
  -- Initialize static Tree-based charts
  runD3v2M do
    initDonutCharts
    initScatterChart
    initLineChart
  -- Initialize force visualization (uses its own Effect-based API)
  initForceViz
  -- Initialize the data table with D3 visualizations
  DataTable.initDataTable derived

-- | Initialize donut charts
initDonutCharts :: D3v2M Unit
initDonutCharts = do
  let donutConfig = Donut.defaultConfig

  -- Men's donut
  menContainer <- select "#donut-men" :: _ (D3v2Selection_ SEmpty Element Unit)
  let menDonut = Donut.donutChart donutConfig (overallAcceptanceRates.male * 100.0) "Men"
  let
    menTree :: T.Tree Unit
    menTree =
      T.elem SVG
        [ v3Attr "width" (lit (donutConfig.centerX * 2.0))
        , v3Attr "height" (lit (donutConfig.centerY + donutConfig.outerRadius + 30.0))
        , v3AttrStr "viewBox" (str ("0 0 " <> show (donutConfig.centerX * 2.0) <> " " <> show (donutConfig.centerY + donutConfig.outerRadius + 30.0)))
        , v3AttrStr "class" (str "donut-svg")
        ]
        `T.withChildren`
          [ T.elem Group [ v3AttrStr "transform" (str menDonut.centerTransform) ]
              `T.withChildren`
                [ menDonut.label ]
          ]
  _ <- renderTree menContainer menTree

  -- We need to render arcs separately due to datum type
  menArcsContainer <- select "#donut-men svg" :: _ (D3v2Selection_ SEmpty Element Unit)
  let
    menArcsGroup :: T.Tree Unit
    menArcsGroup = T.elem Group [ v3AttrStr "transform" (str menDonut.centerTransform), v3AttrStr "class" (str "arcs") ]
  _ <- renderTree menArcsContainer menArcsGroup

  menArcsInner <- select "#donut-men svg .arcs" :: _ (D3v2Selection_ SEmpty Element Donut.IndexedSlice)
  _ <- renderTree menArcsInner (menDonut.arcs :: T.Tree Donut.IndexedSlice)

  -- Women's donut
  womenContainer <- select "#donut-women" :: _ (D3v2Selection_ SEmpty Element Unit)
  let womenDonut = Donut.donutChart donutConfig (overallAcceptanceRates.female * 100.0) "Women"
  let
    womenTree :: T.Tree Unit
    womenTree =
      T.elem SVG
        [ v3Attr "width" (lit (donutConfig.centerX * 2.0))
        , v3Attr "height" (lit (donutConfig.centerY + donutConfig.outerRadius + 30.0))
        , v3AttrStr "viewBox" (str ("0 0 " <> show (donutConfig.centerX * 2.0) <> " " <> show (donutConfig.centerY + donutConfig.outerRadius + 30.0)))
        , v3AttrStr "class" (str "donut-svg")
        ]
        `T.withChildren`
          [ T.elem Group [ v3AttrStr "transform" (str womenDonut.centerTransform) ]
              `T.withChildren`
                [ womenDonut.label ]
          ]
  _ <- renderTree womenContainer womenTree

  womenArcsContainer <- select "#donut-women svg" :: _ (D3v2Selection_ SEmpty Element Unit)
  let
    womenArcsGroup :: T.Tree Unit
    womenArcsGroup = T.elem Group [ v3AttrStr "transform" (str womenDonut.centerTransform), v3AttrStr "class" (str "arcs") ]
  _ <- renderTree womenArcsContainer womenArcsGroup

  womenArcsInner <- select "#donut-women svg .arcs" :: _ (D3v2Selection_ SEmpty Element Donut.IndexedSlice)
  _ <- renderTree womenArcsInner (womenDonut.arcs :: T.Tree Donut.IndexedSlice)

  pure unit

-- | Initialize scatter chart
initScatterChart :: D3v2M Unit
initScatterChart = do
  container <- select "#scatter-chart" :: _ (D3v2Selection_ SEmpty Element Unit)

  let config = Scatter.defaultConfig
  let chart = Scatter.scatterChart config

  -- Create SVG with static content
  let svgTree =
        T.elem SVG chart.containerAttrs
          `T.withChildren`
            [ T.elem Group [ v3AttrStr "transform" (str ("translate(" <> show config.marginLeft <> "," <> show config.marginTop <> ")")) ]
                `T.withChildren`
                  [ chart.static ]
            ]
  _ <- renderTree container svgTree

  -- Add the data-bound points in a separate group
  pointsContainer <- select "#scatter-chart svg" :: _ (D3v2Selection_ SEmpty Element Unit)
  let
    pointsGroup :: T.Tree Unit
    pointsGroup = T.elem Group
      [ v3AttrStr "transform" (str ("translate(" <> show config.marginLeft <> "," <> show config.marginTop <> ")"))
      , v3AttrStr "class" (str "points")
      ]
  _ <- renderTree pointsContainer pointsGroup

  pointsInner <- select "#scatter-chart svg .points" :: _ (D3v2Selection_ SEmpty Element Scatter.IndexedPoint)
  _ <- renderTree pointsInner (chart.points :: T.Tree Scatter.IndexedPoint)

  pure unit

-- | Initialize line chart
initLineChart :: D3v2M Unit
initLineChart = do
  container <- select "#line-chart" :: _ (D3v2Selection_ SEmpty Element Unit)

  let config = Line.defaultConfig

  -- Create SVG with static content
  let svgTree =
        T.elem SVG
          [ v3Attr "width" (lit config.width)
          , v3Attr "height" (lit config.height)
          , v3AttrStr "viewBox" (str ("0 0 " <> show config.width <> " " <> show config.height))
          , v3AttrStr "class" (str "line-chart-svg")
          ]
          `T.withChildren`
            [ T.elem Group
                [ v3AttrStr "transform" (str ("translate(" <> show config.marginLeft <> "," <> show config.marginTop <> ")")) ]
                `T.withChildren`
                  [ Line.lineChartStatic config ]
            , -- Points group (will be updated)
              T.elem Group
                [ v3AttrStr "transform" (str ("translate(" <> show config.marginLeft <> "," <> show config.marginTop <> ")"))
                , v3AttrStr "class" (str "rate-points-group")
                ]
            ]
  _ <- renderTree container svgTree

  pure unit

-- | Initialize force visualization
initForceViz :: Effect Unit
initForceViz = do
  -- Create the force visualization and store the handle
  handle <- ForceViz.initForceViz "#force-viz"
  Ref.write (Just handle) forceVizHandleRef

-- | Update line chart with new proportions
updateLineChart :: Proportions -> Effect Unit
updateLineChart props = runD3v2M do
  let config = Line.defaultConfig

  -- Clear and re-render the points group
  pointsContainer <- select "#line-chart .rate-points-group" :: _ (D3v2Selection_ SEmpty Element Line.IndexedRatePoint)
  _ <- renderTree pointsContainer (Line.lineChartPoints config props)

  pure unit
