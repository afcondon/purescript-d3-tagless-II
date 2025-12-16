module PSD3.Tutorial.GettingStarted where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.Footer as Footer
import PSD3.Shared.SiteNav as SiteNav
import Type.Proxy (Proxy(..))

-- | Getting Started page state
type State = Unit

-- | Getting Started page actions
data Action = Initialize

-- | Child component slots
type Slots =
  ( sectionNav :: forall q. H.Slot q Void Unit
  )

_sectionNav = Proxy :: Proxy "sectionNav"

-- | Getting Started page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "docs-page" ] ]
    [ -- Site Navigation
      SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.QuadGettingStarted
        , prevNext: Nothing
        , pageTitle: Nothing
        }

    -- Hero section
    , HH.section
        [ HP.classes [ HH.ClassName "docs-hero" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "docs-hero-content" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "docs-hero-title" ] ]
                [ HH.text "Getting Started" ]
            , HH.p
                [ HP.classes [ HH.ClassName "docs-hero-description" ] ]
                [ HH.text "Build type-safe, declarative D3 visualizations in PureScript. This guide will have you rendering your first chart in minutes." ]
            ]
        ]

    -- Quick Start section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "quickstart"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Quick Start" ]

        , HH.h3
            [ HP.id "prerequisites" ]
            [ HH.text "Prerequisites" ]
        , HH.p_
            [ HH.text "You'll need:" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Node.js 18+" ]
            , HH.li_ [ HH.text "PureScript compiler and Spago" ]
            ]
        , HH.pre_
            [ HH.code_
                [ HH.text "npm install -g purescript spago" ]
            ]

        , HH.h3
            [ HP.id "installation" ]
            [ HH.text "Installation" ]
        , HH.p_
            [ HH.text "Add the PSD3 packages to your project:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """spago install psd3-selection psd3-layout""" ]
            ]
        , HH.p_
            [ HH.text "For force-directed graphs and simulations (which use the lower-level PSD3 API), also add:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text "spago install psd3-simulation" ]
            ]
        ]

    -- First Visualization section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "first-viz"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Your First Visualization" ]
        , HH.p_
            [ HH.text "PSD3 uses a declarative Tree API. You describe what you want, and the library renders it to D3. Here's a complete example:" ]

        , HH.pre_
            [ HH.code_
                [ HH.text """module Main where

import Prelude
import Effect (Effect)
import PSD3.Tree as T
import PSD3.Tree (Tree)
import PSD3.Attribute (class_, cx, cy, r, fill)
import PSD3.Interpreter.D3 (render)

-- Your data
type Point = { x :: Number, y :: Number, color :: String }

myData :: Array Point
myData =
  [ { x: 100.0, y: 100.0, color: "steelblue" }
  , { x: 200.0, y: 150.0, color: "coral" }
  , { x: 300.0, y: 100.0, color: "seagreen" }
  ]

-- The visualization
chart :: Tree Point
chart =
  T.named Svg "svg"
    [ T.width 400.0, T.height 200.0 ]
    `T.withChildren`
      [ T.joined "circles" Circle myData identity $ \\point ->
          [ cx point.x
          , cy point.y
          , r 20.0
          , fill point.color
          ]
      ]

-- Render to the DOM
main :: Effect Unit
main = render "#chart" chart""" ]
            ]

        , HH.h3_ [ HH.text "What's happening here?" ]
        , HH.ul_
            [ HH.li_
                [ HH.code_ [ HH.text "T.named Svg \"svg\"" ]
                , HH.text " - Creates a named SVG element (the name helps with transitions)"
                ]
            , HH.li_
                [ HH.code_ [ HH.text "T.joined \"circles\" Circle myData identity" ]
                , HH.text " - Data joins your array to Circle elements. Each point becomes a circle."
                ]
            , HH.li_
                [ HH.text "The lambda "
                , HH.code_ [ HH.text "\\point -> [...]" ]
                , HH.text " receives each data item with full type safety - no coercion needed"
                ]
            , HH.li_
                [ HH.code_ [ HH.text "render \"#chart\" chart" ]
                , HH.text " - Renders the tree to a DOM element with selector "
                , HH.code_ [ HH.text "#chart" ]
                ]
            ]
        ]

    -- HTML Setup section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "html-setup"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "HTML Setup" ]
        , HH.p_
            [ HH.text "Your HTML needs D3.js and a container element:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """<!DOCTYPE html>
<html>
<head>
  <script src="https://d3js.org/d3.v7.min.js"></script>
</head>
<body>
  <div id="chart"></div>
  <script src="bundle.js"></script>
</body>
</html>""" ]
            ]
        , HH.p_
            [ HH.text "Bundle your PureScript:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text "spago bundle --module Main --outfile bundle.js" ]
            ]
        ]

    -- Key Concepts section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "concepts"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Key Concepts" ]

        , HH.h3_ [ HH.text "The Tree API" ]
        , HH.p_
            [ HH.text "PSD3's Tree API is declarative - you describe the structure of your visualization as a tree of elements. The key building blocks:" ]
        , HH.ul_
            [ HH.li_
                [ HH.code_ [ HH.text "T.elem" ]
                , HH.text " - A single element with static attributes"
                ]
            , HH.li_
                [ HH.code_ [ HH.text "T.joined" ]
                , HH.text " - Data-driven elements (D3's data join)"
                ]
            , HH.li_
                [ HH.code_ [ HH.text "T.named" ]
                , HH.text " - Named container for identity-preserving updates"
                ]
            , HH.li_
                [ HH.code_ [ HH.text "T.withChild / T.withChildren" ]
                , HH.text " - Nest elements hierarchically"
                ]
            ]

        , HH.h3_ [ HH.text "Type-Safe Attributes" ]
        , HH.p_
            [ HH.text "Attributes are functions from your data to values. The compiler ensures you can't use attributes on wrong element types:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """-- This works: cx is valid for Circle
T.joined "dots" Circle data identity $ \\d ->
  [ cx d.x, cy d.y, r 5.0 ]

-- This won't compile: cx isn't valid for Rect
T.joined "bars" Rect data identity $ \\d ->
  [ cx d.x ]  -- Compile error!""" ]
            ]

        , HH.h3_ [ HH.text "Interpreters" ]
        , HH.p_
            [ HH.text "The same Tree can be rendered different ways:" ]
        , HH.ul_
            [ HH.li_
                [ HH.code_ [ HH.text "D3v2.render" ]
                , HH.text " - Renders to the DOM via D3"
                ]
            , HH.li_
                [ HH.code_ [ HH.text "MermaidTree.interpret" ]
                , HH.text " - Generates a Mermaid diagram of the tree structure"
                ]
            , HH.li_
                [ HH.code_ [ HH.text "English.interpret" ]
                , HH.text " - Produces English description (debugging)"
                ]
            ]

        , HH.h3_ [ HH.text "Two APIs" ]
        , HH.p_
            [ HH.text "PSD3 offers two levels of abstraction:" ]
        , HH.ul_
            [ HH.li_
                [ HH.strong_ [ HH.text "Tree API" ]
                , HH.text " - Declarative, ideal for static charts and layouts (bar charts, trees, Sankey diagrams). This is what we've shown above."
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "PSD3 API" ]
                , HH.text " - Lower-level but equally type-safe. Required for force simulations and highly interactive visualizations. See the "
                , HH.a [ HP.href "#/showcase" ] [ HH.text "Code Explorer" ]
                , HH.text " for an example."
                ]
            ]
        , HH.p_
            [ HH.text "Both APIs share the same type-safe foundation. The Tree API builds on top of the PSD3 API, so you can mix them when needed." ]
        ]

    -- Next Steps section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "next"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Next Steps" ]
        , HH.ul_
            [ HH.li_
                [ HH.a [ HP.href "#/tour" ] [ HH.text "Take the Tour" ]
                , HH.text " - See what's possible with PSD3"
                ]
            , HH.li_
                [ HH.a [ HP.href "#/showcase" ] [ HH.text "Explore the Showcase" ]
                , HH.text " - Interactive examples like the Code Explorer and SPLOM"
                ]
            , HH.li_
                [ HH.a [ HP.href "#/gallery" ] [ HH.text "Browse the Gallery" ]
                , HH.text " - All examples organized by category"
                ]
            , HH.li_
                [ HH.a [ HP.href "#/understanding" ] [ HH.text "Understanding PSD3" ]
                , HH.text " - Deep dive into the architecture"
                ]
            ]
        ]

    -- Footer
    , Footer.render
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
