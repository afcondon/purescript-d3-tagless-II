module Component.HowTo.HowtoEvents where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Website.Types (Route(..))
import PSD3.RoutingDSL (routeToPath)

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
    [ renderHeader "Responding to User Events"

    , HH.main_
        [ -- Intro
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Responding to User Events" ]
            , HH.p_
                [ HH.text "PSD3 provides type-safe event handlers. Use the 'on' function to attach behaviors to selections." ]
            ]

        -- Click Handlers
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Click Handlers" ]

            , HH.p_ [ HH.text "Access the bound datum when an element is clicked:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Behavior.Types (onClickWithDatum)

circles <- append Circle [fill "blue", radius 20.0] container
_ <- on (onClickWithDatum \\d -> log ("Clicked: " <> d.name)) circles""" ]
                ]

            , HH.p_ [ HH.text "Without datum access:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Behavior.Types (onClick)

_ <- on (onClick (log "Button clicked!")) button""" ]
                ]
            ]

        -- Hover Effects
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Hover Effects" ]

            , HH.p_ [ HH.text "Simple enter/leave handlers:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Behavior.Types (onMouseEnter, onMouseLeave)

_ <- on (onMouseEnter \\d -> highlightNode d) circles
_ <- on (onMouseLeave \\_ -> clearHighlight) circles""" ]
                ]

            , HH.p_ [ HH.text "Built-in style changes with onHover:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Behavior.Types (onHover)

_ <- on (onHover
  { enter: [{ attr: "stroke", value: "#333" }, { attr: "stroke-width", value: "3" }]
  , leave: [{ attr: "stroke", value: "#ddd" }, { attr: "stroke-width", value: "1.5" }]
  }) lines""" ]
                ]

            , HH.p_ [ HH.text "With mouse coordinates (for tooltips):" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Behavior.Types (onMouseEnterWithInfo)

_ <- on (onMouseEnterWithInfo \\info -> do
  -- info.datum: bound data
  -- info.pageX/pageY: document coordinates
  -- info.offsetX/offsetY: element coordinates
  showTooltip info.datum info.pageX info.pageY
) circles""" ]
                ]
            ]

        -- Drag and Zoom
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Drag & Zoom" ]

            , HH.p_ [ HH.text "Simple drag behavior:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Behavior.Types (Drag(..), defaultDrag)

_ <- on (Drag defaultDrag) draggableElement""" ]
                ]

            , HH.p_ [ HH.text "Force simulation drag (reheats on drag):" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Behavior.Types (Drag(..), simulationDrag)

_ <- on (Drag (simulationDrag "my-simulation-id")) nodes""" ]
                ]

            , HH.p_ [ HH.text "Pan and zoom:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Behavior.Types (Zoom(..), defaultZoom, ScaleExtent(..))

-- Attach to outer SVG, transforms inner group
_ <- on (Zoom (defaultZoom (ScaleExtent 0.5 4.0) ".zoom-group")) svg""" ]
                ]
            ]

        -- Key Points
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Key Points" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Type-safe" ], HH.text " - Datum type flows through handlers" ]
                , HH.li_ [ HH.strong_ [ HH.text "MouseEventInfo" ], HH.text " - Contains datum + all coordinates" ]
                , HH.li_ [ HH.strong_ [ HH.text "Combine behaviors" ], HH.text " - Attach multiple with separate 'on' calls" ]
                , HH.li_ [ HH.strong_ [ HH.text "simulationDrag" ], HH.text " - Auto-reheats force simulation" ]
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
                    [ HH.code_ [ HH.text "src/website/Viz/Spago/Render.purs:81-96" ]
                    , HH.text " - Drag, click, hover behaviors"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "src/lib/PSD3v2/Behavior/Types.purs" ]
                    , HH.text " - All behavior types with docs"
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
