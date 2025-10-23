module PSD3.SimpleCharts where

import Prelude

import D3.Viz.BarChart as BarChart
import D3.Viz.LineChart as LineChart
import D3.Viz.ScatterPlot as ScatterPlot
import D3.Viz.Charts.Model (monthlySales, sineWaveData, anscombesQuartet)
import D3Tagless.Instance.Selection (eval_D3M)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RHSNavigation as RHSNav
import PSD3.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | SimpleCharts page state
type State = Unit

-- | SimpleCharts page actions
data Action = Initialize

-- | Child component slots
type Slots = ( rhsNav :: forall q. H.Slot q Void Unit )

_rhsNav = Proxy :: Proxy "rhsNav"

-- | SimpleCharts page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.liftEffect $ eval_D3M $ LineChart.draw sineWaveData "div.linechart-viz"
    _ <- H.liftEffect $ eval_D3M $ BarChart.draw monthlySales "div.barchart-viz"
    _ <- H.liftEffect $ eval_D3M $ ScatterPlot.drawQuartet anscombesQuartet "div.quartet-viz"
    pure unit

render :: State -> H.ComponentHTML Action Slots Aff
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
                    [ HH.a [ HP.href "#section-1", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "1. Multi-Line Chart" ]
                    , HH.a [ HP.href "#section-2", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "2. Bar Chart" ]
                    , HH.a [ HP.href "#section-3", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "3. Anscombe's Quartet" ]
                    ]
                ]
            ]
        ]

    -- Navigation Panel (RHS)
    , HH.slot_ _rhsNav unit RHSNav.component SimpleCharts

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Simple Charts: Production-Ready Examples" ]
        , HH.p_
            [ HH.text "Polished, production-ready examples of common chart types with interesting data. These examples demonstrate best practices for building interactive visualizations with proper axes, legends, and hover interactions." ]
        , HH.p_
            [ HH.text "Each chart includes features like responsive design, smooth transitions, and clear labeling to make them suitable for real-world applications." ]
        ]

    -- Section 1: Multi-Line Chart
    , HH.section
        [ HP.id "section-1"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. Multi-Line Chart" ]
        , HH.p_
            [ HH.text "A clean line chart showing multiple data series with smooth curves. This example uses sine wave data to demonstrate how to create flowing visualizations with proper scaling and axes." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "linechart-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "This chart uses D3's line generators with linear scales for both x and y axes. The visualization includes properly positioned axes with tick marks and labels for easy data interpretation." ]
        ]

    -- Section 2: Bar Chart
    , HH.section
        [ HP.id "section-2"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "2. Bar Chart" ]
        , HH.p_
            [ HH.text "A classic bar chart with properly aligned bars and clean styling. This example shows monthly sales data with each bar representing a data point. Note the careful attention to bar positioning and width calculations." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "barchart-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "Bar charts are fundamental for comparing discrete values. This implementation includes proper spacing between bars and uses a linear scale for accurate height representation." ]
        ]

    -- Section 3: Anscombe's Quartet
    , HH.section
        [ HP.id "section-3"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "3. Anscombe's Quartet" ]
        , HH.p_
            [ HH.text "The famous Anscombe's Quartet demonstrates why visualization matters. These four datasets have nearly identical statistical properties (mean, variance, correlation) but look completely different when plotted." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "quartet-viz" ] ]
                []
            ]
        , HH.p_
            [ HH.text "This small multiples display shows all four datasets in a 2x2 grid, making it easy to compare them. Each subplot uses the same scale for valid comparison, highlighting how identical summary statistics can mask very different underlying patterns." ]
        ]
    ]
