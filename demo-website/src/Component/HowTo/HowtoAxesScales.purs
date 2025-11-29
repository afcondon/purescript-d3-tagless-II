module Component.HowTo.HowtoAxesScales where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))
import D3.Viz.FPFTW.ScalesDemo as ScalesDemo

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
  Initialize -> do
    H.liftAff $ delay (Milliseconds 100.0)
    liftEffect $ ScalesDemo.drawGradientComparison "#gradient-demo"
    liftEffect $ ScalesDemo.drawScalePipeline "#pipeline-demo"
    pure unit

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
                [ HH.text "Scales: The PSD3 Way" ]
            , HH.p_
                [ HH.text "PSD3 provides a comprehensive, type-safe Scale module with full D3 parity plus functional programming idioms. Scales transform data from a domain (input) to a range (output)." ]
            ]

        -- Quick Start
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Quick Start" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3.Scale (linear, domain, range, applyScale, ticks)

-- Create a linear scale
xScale = linear
  # domain [0.0, 100.0]
  # range [0.0, 800.0]

-- Apply it
pixelX = applyScale xScale 50.0  -- Returns 400.0

-- Get tick marks for an axis
tickValues = ticks 10 xScale     -- [0, 10, 20, ..., 100]""" ]
                ]
            ]

        -- Scale Types
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Scale Types" ]
            , HH.h3_ [ HH.text "Continuous Scales" ]
            , HH.p_ [ HH.text "Map continuous domain to continuous range:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Linear (most common)
linear # domain [0.0, 100.0] # range [0.0, 500.0]

-- Logarithmic (for exponential data)
log # domain [1.0, 1000.0] # range [0.0, 300.0]

-- Power/Square root (for area scaling)
sqrt # domain [0.0, maxValue] # range [0.0, 50.0]

-- Symlog (handles negative values and zero)
symlog # constant 1.0 # domain [-100.0, 100.0] # range [0.0, 500.0]""" ]
                ]

            , HH.h3_ [ HH.text "Band Scales (for bar charts)" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Band scale with padding
xScale = band
  # domain ["Mon", "Tue", "Wed", "Thu", "Fri"]
  # range [0.0, 500.0]
  # padding 0.1

-- Get bar position and width
barX = applyScale xScale "Wed"
barWidth = bandwidth xScale""" ]
                ]

            , HH.h3_ [ HH.text "Ordinal Scales (categorical → categorical)" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Map categories to colors
colorScale = ordinal
  # domain ["A", "B", "C"]
  # range ["red", "green", "blue"]

color = applyScale colorScale "B"  -- "green" """ ]
                ]
            ]

        -- Color Interpolators
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Color Interpolators" ]
            , HH.p_ [ HH.text "Built-in color interpolators take a value in [0, 1] and return a color:" ]

            -- Gradient demo container
            , HH.div
                [ HP.id "gradient-demo"
                , HP.style "margin: 20px 0; padding: 15px; background: #f8f9fa; border-radius: 8px;"
                ]
                [ HH.div [ HP.id "gradient-demo-viridis" ] []
                , HH.div [ HP.id "gradient-demo-plasma" ] []
                , HH.div [ HP.id "gradient-demo-inferno" ] []
                , HH.div [ HP.id "gradient-demo-turbo" ] []
                , HH.div [ HP.id "gradient-demo-rdylgn" ] []
                ]

            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3.Scale (interpolateViridis, interpolatePlasma, interpolateRdYlGn)

-- Sequential (for continuous data)
fill (\\d -> interpolateViridis (d.value / maxValue))

-- Diverging (for data with meaningful midpoint)
fill (\\d -> interpolateRdYlGn ((d.change + 1.0) / 2.0))

-- Use with sample for gradients
colors = Array.range 0 99 <#> \\i -> interpolateViridis (toNumber i / 99.0)""" ]
                ]
            ]

        -- Scale Composition
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Scale Composition (FP Power!)" ]
            , HH.p_ [ HH.text "Scales compose beautifully. Here's a pipeline: raw data → normalize → color:" ]

            -- Pipeline demo
            , HH.div
                [ HP.id "pipeline-demo"
                , HP.style "margin: 20px 0;"
                ]
                []

            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3.Scale (andThen, contramap, map)
import PSD3.Scale.FP (normalize)

-- Compose scales with andThen
tempToColor = normalize (-10.0) 40.0 `andThen` interpolateRdYlGn

-- Or use contramap/map for transformations
celsiusScale = fahrenheitScale # contramap celsiusToFahrenheit
offsetScale = pixelScale # map (_ + margin)

-- Full dimap (profunctor-like)
transformed = scale # dimap preprocess postprocess""" ]
                ]
            ]

        -- Configuration
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Scale Configuration" ]
            , HH.p_ [ HH.text "All configuration returns a new scale (immutable API):" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Chain configurations with #
myScale = linear
  # domain [0.0, 100.0]
  # range [0.0, 500.0]
  # nice           -- Round domain to nice values
  # clamp true     -- Constrain output to range

-- Use modifiers from PSD3.Scale.FP
import PSD3.Scale.FP (niceModifier, clampModifier, combineModifiers)

combined = combineModifiers [niceModifier, clampModifier]
myScale = linear # combined # domain [...] # range [...]""" ]
                ]
            ]

        -- Operations
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Scale Operations" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Apply scale
y = applyScale scale x

-- Invert (range → domain)
case invert scale pixelY of
  Just dataY -> -- use dataY
  Nothing -> -- out of range

-- Generate tick values
tickVals = ticks 10 scale

-- Format ticks
formatter = tickFormat 10 ".1f" scale
labels = tickVals <#> formatter

-- Band scale specifics
barWidth = bandwidth bandScale
stepSize = step bandScale""" ]
                ]
            ]

        -- Key Points
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Key Points" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Type-safe: " ], HH.text "Phantom types track scale kind (Continuous, Band, etc.)" ]
                , HH.li_ [ HH.strong_ [ HH.text "Immutable: " ], HH.text "Configuration returns new scales, safe to share" ]
                , HH.li_ [ HH.strong_ [ HH.text "Composable: " ], HH.text "Use andThen, contramap, map, dimap" ]
                , HH.li_ [ HH.strong_ [ HH.text "Full D3 parity: " ], HH.text "All D3 scale types and methods available" ]
                , HH.li_ [ HH.strong_ [ HH.text "FP extras: " ], HH.text "PSD3.Scale.FP adds sampling, modifiers, utilities" ]
                ]
            ]

        -- See Also
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "See Also" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "PSD3.Scale" ]
                    , HH.text " - Core scale module"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "PSD3.Scale.FP" ]
                    , HH.text " - Functional programming abstractions"
                    ]
                , HH.li_
                    [ HH.a
                        [ HP.href $ "#" <> routeToPath TourFPFTW ]
                        [ HH.text "FP For The Win Tour" ]
                    , HH.text " - See scales as profunctors in action"
                    ]
                ]
            ]
        ]
    ]
