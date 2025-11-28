module Component.HowTo.HowtoDebugging where

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
                [ HH.text "Debugging Visualizations" ]
            , HH.p_
                [ HH.text "Techniques for finding and fixing visualization issues." ]
            ]

        -- Console Logging
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Console Logging" ]

            , HH.p_ [ HH.text "Log data in attribute functions:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)

-- Debug attribute values
fill (\\d -> unsafePerformEffect do
  log $ "Node: " <> show d.id <> ", value: " <> show d.value
  pure $ colorScale d.value
)""" ]
                ]
            ]

        -- Browser DevTools
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Browser DevTools" ]

            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Elements panel" ], HH.text " - Inspect SVG structure and attributes" ]
                , HH.li_ [ HH.strong_ [ HH.text "$0.__data__" ], HH.text " - View bound data on selected element" ]
                , HH.li_ [ HH.strong_ [ HH.text "Network tab" ], HH.text " - Verify data loading" ]
                , HH.li_ [ HH.strong_ [ HH.text "Performance tab" ], HH.text " - Profile slow renders" ]
                ]
            ]

        -- Mermaid Diagrams
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Mermaid Diagrams" ]

            , HH.p_ [ HH.text "Visualize tree structure with Mermaid interpreter:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Generate Mermaid code from TreeAPI tree
let mermaidCode = runMermaidInterpreter myTree

-- Copy to https://mermaid.live to visualize structure""" ]
                ]
            ]

        -- Common Issues
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Common Issues" ]

            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Nothing renders" ]
                    , HH.text " - Check container exists, SVG has size"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Elements at origin" ]
                    , HH.text " - Verify coordinate attributes (cx, cy, x, y)"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Wrong colors" ]
                    , HH.text " - Check scale domain/range, normalize values to 0-1"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "No data binding" ]
                    , HH.text " - Use joinData, check key function"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Type errors" ]
                    , HH.text " - Ensure datum type matches selection"
                    ]
                ]
            ]

        -- Key Points
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Key Points" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "unsafePerformEffect" ], HH.text " - Escape hatch for logging" ]
                , HH.li_ [ HH.strong_ [ HH.text "$0.__data__" ], HH.text " - Console access to bound data" ]
                , HH.li_ [ HH.strong_ [ HH.text "Mermaid interpreter" ], HH.text " - Visualize tree structure" ]
                , HH.li_ [ HH.strong_ [ HH.text "Check dimensions" ], HH.text " - SVG needs viewBox or width/height" ]
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
