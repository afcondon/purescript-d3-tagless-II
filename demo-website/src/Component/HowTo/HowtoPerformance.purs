module Component.HowTo.HowtoPerformance where

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
                [ HH.text "Performance Optimization" ]
            , HH.p_
                [ HH.text "Tips for fast, smooth visualizations with large datasets." ]
            ]

        -- Reduce DOM Elements
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Reduce DOM Elements" ]

            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Sample data" ], HH.text " - Random sample for preview, full for export" ]
                , HH.li_ [ HH.strong_ [ HH.text "Aggregate" ], HH.text " - Bin/group data points" ]
                , HH.li_ [ HH.strong_ [ HH.text "Filter visually" ], HH.text " - Hide offscreen elements" ]
                , HH.li_ [ HH.strong_ [ HH.text "Canvas" ], HH.text " - Consider d3-canvas for 10k+ elements" ]
                ]
            ]

        -- Minimize Updates
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Minimize Updates" ]

            , HH.p_ [ HH.text "Batch DOM mutations:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Good: Set multiple attributes at once
setAttrs [cx newX, cy newY, fill newColor] circles

-- Avoid: Multiple separate setAttrs calls
-- Each call triggers layout recalculation""" ]
                ]

            , HH.p_ [ HH.text "Debounce rapid updates:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Only update after user stops resizing
window.addEventListener('resize', debounce(updateChart, 150))""" ]
                ]
            ]

        -- Efficient Tick Functions
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Force Simulation Tips" ]

            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Minimal tick attrs" ], HH.text " - Only update position, not style" ]
                , HH.li_ [ HH.strong_ [ HH.text "Alpha decay" ], HH.text " - Higher decay = faster cooldown" ]
                , HH.li_ [ HH.strong_ [ HH.text "Stop when idle" ], HH.text " - Call stop() when alpha reaches min" ]
                ]

            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Minimal tick: just positions
addTickFunction "nodes" $ Step circles [cx (_.x), cy (_.y)]

-- Don't recalculate colors on every frame!""" ]
                ]
            ]

        -- CSS Transitions
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "CSS vs D3 Transitions" ]

            , HH.p_ [ HH.text "CSS transitions are often faster for simple changes:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """.node circle {
  transition: fill 0.2s ease, opacity 0.2s ease;
}

/* PSD3 just toggles class, CSS handles animation */
.node.highlighted circle { fill: #fbbf24; }""" ]
                ]
            ]

        -- Key Points
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Key Points" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "< 1000 elements" ], HH.text " - SVG fine, no optimization needed" ]
                , HH.li_ [ HH.strong_ [ HH.text "1000-5000 elements" ], HH.text " - Optimize tick functions, batch updates" ]
                , HH.li_ [ HH.strong_ [ HH.text "> 5000 elements" ], HH.text " - Consider Canvas, sampling, or aggregation" ]
                , HH.li_ [ HH.strong_ [ HH.text "Profile first" ], HH.text " - Use DevTools Performance tab" ]
                ]
            ]

        -- Real Example
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Real Example" ]
            , HH.p_ [ HH.text "The Code Explorer handles 1400+ nodes smoothly:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "src/website/Component/CodeExplorer" ]
                    , HH.text " - Force graph with many nodes"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "src/website/Viz/Spago/Render.purs" ]
                    , HH.text " - Efficient render callbacks"
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
