module Component.HowTo.HowtoAxesScales where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))

type State = Unit

data Action = Initialize

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
  Initialize -> pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.QuadHowTo
        , prevNext: Nothing
        , pageTitle: Nothing
        }

    , HH.main_
        [ -- Intro
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Creating Axes and Scales" ]
            , HH.p_
                [ HH.text "PSD3 provides color scales and supports D3's scale/axis FFI for custom scales." ]
            ]

        -- Color Scales
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Color Scales" ]

            , HH.p_ [ HH.text "Built-in color interpolation (0-1 input):" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3.Internal.Scales.Scales

-- Sequential scales
fill (\\d -> d3InterpolateViridis_ (d.value / maxValue))
fill (\\d -> d3InterpolatePlasma_ (d.value / maxValue))

-- Diverging scales (good for -1 to 1 ranges)
fill (\\d -> d3InterpolateRdYlGn_ (normalize d.change))

-- Categorical by index
fill (\\d -> d3SchemeCategory10N_ (toNumber d.index))""" ]
                ]
            ]

        -- FFI Scales
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Custom D3 Scales via FFI" ]

            , HH.p_ [ HH.text "For linear/band/time scales, use FFI:" ]
            , HH.h3_ [ HH.text "JavaScript FFI" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """export const scaleLinear_ = domain => range => () =>
  d3.scaleLinear().domain(domain).range(range);

export const scaleBand_ = domain => range => padding => () =>
  d3.scaleBand().domain(domain).range(range).padding(padding);

export const applyScale_ = scale => value => scale(value);""" ]
                ]

            , HH.h3_ [ HH.text "PureScript" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """foreign import scaleLinear_ :: Array Number -> Array Number -> Effect Scale
foreign import scaleBand_ :: Array String -> Array Number -> Number -> Effect Scale
foreign import applyScale_ :: Scale -> Number -> Number

-- Usage
xScale <- liftEffect $ scaleLinear_ [0.0, maxX] [0.0, width]
let xPos d = applyScale_ xScale d.value""" ]
                ]
            ]

        -- Axes
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Creating Axes" ]

            , HH.p_ [ HH.text "Axes via FFI:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- JavaScript
export const axisBottom_ = scale => () => d3.axisBottom(scale);
export const axisLeft_ = scale => () => d3.axisLeft(scale);
export const callAxis_ = selection => axis => () => selection.call(axis);

-- PureScript: Append group then call axis
axisGroup <- append Group [transform ("translate(0," <> show height <> ")")] svg
liftEffect $ callAxis_ axisGroup xAxis""" ]
                ]
            ]

        -- Key Points
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Key Points" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Color interpolators" ], HH.text " - Take 0-1, return color string" ]
                , HH.li_ [ HH.strong_ [ HH.text "Normalize data" ], HH.text " - Scale values to 0-1 for interpolators" ]
                , HH.li_ [ HH.strong_ [ HH.text "FFI for numeric scales" ], HH.text " - scaleLinear, scaleBand, scaleTime" ]
                , HH.li_ [ HH.strong_ [ HH.text "Axis via .call()" ], HH.text " - D3 pattern for rendering axes" ]
                ]
            ]

        -- Real Example
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Real Example" ]
            , HH.p_ [ HH.text "See scales in action:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "src/lib/PSD3/Internal/Scales/Scales.purs" ]
                    , HH.text " - Color interpolators"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "src/website/Viz/Spago/Draw/Attributes.purs" ]
                    , HH.text " - Color-by-metric scales"
                    ]
                ]
            ]
        ]
    ]

renderHeader :: forall w i. String -> HH.HTML w i
renderHeader title =
  HH.header
    [ HP.classes [ HH.ClassName "example-header" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "example-header-left" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath Home
            , HP.classes [ HH.ClassName "example-logo-link" ]
            ]
            [ HH.img
                [ HP.src "assets/psd3-logo-color.svg"
                , HP.alt "PSD3 Logo"
                , HP.classes [ HH.ClassName "example-logo" ]
                ]
            ]
        , HH.a
            [ HP.href "#/howto"
            , HP.classes [ HH.ClassName "example-gallery-link" ]
            ]
            [ HH.text "How-to" ]
        , HH.div
            [ HP.classes [ HH.ClassName "example-title-container" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "example-title" ] ]
                [ HH.text title ]
            ]
        ]
    ]
