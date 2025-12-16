module Component.HowTo.HowtoTooltips where

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
                [ HH.text "Adding Tooltips & Hover Behaviors" ]
            , HH.p_
                [ HH.text "PSD3 provides built-in tooltip support. Just provide a function that formats your data as HTML." ]
            ]

        -- Basic Tooltip Section
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Basic Tooltip" ]

            , HH.p_ [ HH.text "Import the tooltip module and attach behaviors:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3.Tooltip (onTooltip, onTooltipHide)

-- In your render callback, attach to elements:
_ <- on (onTooltip \\d -> d.name <> ": " <> show d.value) circles
_ <- on onTooltipHide circles""" ]
                ]

            , HH.p_ [ HH.text "That's it! The tooltip automatically:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Shows on mouse enter at cursor position" ]
                , HH.li_ [ HH.text "Hides on mouse leave" ]
                , HH.li_ [ HH.text "Stays within viewport bounds" ]
                , HH.li_ [ HH.text "Has sensible default styling" ]
                ]

            , HH.h3_ [ HH.text "Custom Styling" ]
            , HH.p_ [ HH.text "Override the default styles with CSS:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """.psd3-tooltip {
  background: #1a1a2e;
  font-family: monospace;
}""" ]
                ]

            , HH.p_ [ HH.text "Or set a custom class:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3.Tooltip (setTooltipClass)

-- In initialization:
liftEffect $ setTooltipClass "my-custom-tooltip\"""" ]
                ]
            ]

        -- Advanced: Highlight Connected
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Advanced: Highlight Connected Elements" ]

            , HH.p_ [ HH.text "Combine tooltips with highlighting related elements. This requires a small FFI for D3 class manipulation:" ]

            , HH.h3_ [ HH.text "PureScript" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3.Tooltip (onTooltip, hideTooltip)

foreign import highlightConnected_ :: MyNode -> Effect Unit
foreign import clearHighlights_ :: Effect Unit

-- Combine in hover handlers:
_ <- on (onMouseEnterWithInfo \\info -> do
  showTooltip (formatContent info.datum) info.pageX info.pageY
  highlightConnected_ info.datum
) element

_ <- on (onMouseLeave \\_ -> do
  hideTooltip
  clearHighlights_
) element""" ]
                ]

            , HH.h3_ [ HH.text "JavaScript FFI" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """export const highlightConnected_ = node => () => {
  const connectedIds = new Set([
    ...node.links.targets,
    ...node.links.sources
  ]);

  d3.selectAll('svg g').filter(d => d && d.id)
    .classed('highlighted', d => connectedIds.has(d.id))
    .classed('dimmed', d => d.id !== node.id && !connectedIds.has(d.id));
};

export const clearHighlights_ = () => {
  d3.selectAll('.highlighted').classed('highlighted', false);
  d3.selectAll('.dimmed').classed('dimmed', false);
};""" ]
                ]

            , HH.h3_ [ HH.text "CSS" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """.highlighted circle {
  stroke: #fbbf24;
  filter: drop-shadow(0 0 6px rgba(251, 191, 36, 0.6));
}

.dimmed { opacity: 0.15; }

g circle { transition: opacity 0.15s ease, stroke 0.15s ease; }""" ]
                ]
            ]

        -- Key Points
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Key Points" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Use library tooltips" ], HH.text " - onTooltip + onTooltipHide" ]
                , HH.li_ [ HH.strong_ [ HH.text "Format function" ], HH.text " - Returns HTML string from datum" ]
                , HH.li_ [ HH.strong_ [ HH.text "Combine behaviors" ], HH.text " - Tooltip + highlight in same handler" ]
                , HH.li_ [ HH.strong_ [ HH.text "CSS transitions" ], HH.text " - Smooth fade/glow effects" ]
                ]
            ]

        -- Real Example
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Real Example" ]
            , HH.p_ [ HH.text "See the Code Explorer implementation:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "src/website/Viz/Spago/Tooltip.js/.purs" ]
                    , HH.text " - Tooltips with git metrics"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "src/website/Viz/Spago/Highlight.js/.purs" ]
                    , HH.text " - Connected node highlighting"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "src/website/Viz/Spago/Render.purs:88-96" ]
                    , HH.text " - Behavior attachment"
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
