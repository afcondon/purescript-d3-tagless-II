module Component.HowTo.HowtoTreeAPI where

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
                [ HH.text "Using the TreeAPI" ]
            , HH.p_
                [ HH.text "TreeAPI provides a declarative way to build visualization structures with multiple interpreters." ]
            ]

        -- When to Use
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "When to Use TreeAPI" ]

            , HH.ul_
                [ HH.li_ [ HH.text "Need multiple outputs (D3, String, Mermaid diagram)" ]
                , HH.li_ [ HH.text "Want to visualize the tree structure for debugging" ]
                , HH.li_ [ HH.text "Building reusable visualization components" ]
                , HH.li_ [ HH.text "Working with General Update Pattern" ]
                ]

            , HH.p_ [ HH.text "For simple one-off visualizations, SelectionM is simpler." ]
            ]

        -- Static Elements
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Static Elements" ]

            , HH.p_ [ HH.text "Build static DOM structure:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Tree.Types (Tree(..), Element(..), Child(..))

myTree :: Tree Unit
myTree =
  Tree
    (Element SVG [viewBox "0 0 800 600"])
    [ Child $ Tree (Element Group [class_ "chart-area"])
        [ Child $ Tree (Element Rect [width 100.0, height 50.0]) [] ]
    ]""" ]
                ]
            ]

        -- Data-Driven Elements
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Data-Driven Elements" ]

            , HH.p_ [ HH.text "Use Join for data binding:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """import PSD3v2.Tree.Types (Join(..))

myTree :: Array BarData -> Tree Unit
myTree barData =
  Tree (Element SVG [viewBox "0 0 800 600"])
    [ Join barData "bar" Group
        [ width 20.0
        , height (\\d -> d.value * 10.0)
        , x (\\d i -> toNumber i * 25.0)
        , fill "steelblue"
        ]
    ]""" ]
                ]
            ]

        -- Interpreters
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Interpreters" ]

            , HH.p_ [ HH.text "Same tree, multiple outputs:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Render to D3/DOM
runD3Interpreter container myTree

-- Generate Mermaid diagram
let mermaidCode = runMermaidInterpreter myTree

-- Generate HTML string
let htmlString = runStringInterpreter myTree""" ]
                ]
            ]

        -- Key Points
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Key Points" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "Tree" ], HH.text " - Root element with children" ]
                , HH.li_ [ HH.strong_ [ HH.text "Child/Join" ], HH.text " - Static vs data-bound children" ]
                , HH.li_ [ HH.strong_ [ HH.text "Interpreters" ], HH.text " - Same DSL, multiple backends" ]
                , HH.li_ [ HH.strong_ [ HH.text "Mermaid" ], HH.text " - Debug tree structure visually" ]
                ]
            ]

        -- Real Example
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Real Example" ]
            , HH.p_ [ HH.text "See TreeAPI in action:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "src/website/Viz/TreeAPI/LesMisTreeExample.purs" ]
                    , HH.text " - TreeAPI visualization"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "src/lib/PSD3v2/Tree/Types.purs" ]
                    , HH.text " - Tree type definitions"
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
