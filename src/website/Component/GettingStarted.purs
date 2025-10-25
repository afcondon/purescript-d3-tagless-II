module PSD3.GettingStarted where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RHSNavigation as RHSNav
import PSD3.TOC (renderTOC)
import PSD3.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | Getting Started page state
type State = Unit

-- | Getting Started page actions
data Action = Initialize

-- | Child component slots
type Slots = ( rhsNav :: forall q. H.Slot q Void Unit )

_rhsNav = Proxy :: Proxy "rhsNav"

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
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ -- TOC Panel (LHS)
      renderTOC
        { title: "Contents"
        , items:
            [ { anchor: "installation", label: "Installation", level: 0 }
            , { anchor: "prerequisites", label: "Prerequisites", level: 1 }
            , { anchor: "setup", label: "Project Setup", level: 0 }
            , { anchor: "first-viz", label: "Your First Visualization", level: 0 }
            , { anchor: "next-steps", label: "Next Steps", level: 0 }
            ]
        , image: Just "images/tutorial-bookmark-balloons.jpeg"
        }

    -- Navigation Panel (RHS)
    , HH.slot_ _rhsNav unit RHSNav.component GettingStarted

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Getting Started with PS<$>D3" ]
        , HH.p_
            [ HH.text "Welcome! This guide will help you install PS<$>D3, set up your first project, and create your first data visualization. By the end, you'll have a working PureScript project with a simple D3 visualization." ]
        ]

    -- Installation section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "installation"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Installation" ]
        , HH.p_
            [ HH.text "Before we begin, ensure you have the following installed on your system:" ]

        , HH.h3
            [ HP.id "prerequisites" ]
            [ HH.text "Prerequisites" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Node.js (version 18 or higher)" ]
            , HH.li_ [ HH.text "npm or yarn package manager" ]
            , HH.li_ [ HH.text "PureScript compiler (purs)" ]
            , HH.li_ [ HH.text "Spago (PureScript package manager and build tool)" ]
            ]

        , HH.p_
            [ HH.text "If you don't have PureScript and Spago installed, you can install them globally:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text "npm install -g purescript spago" ]
            ]
        ]

    -- Project Setup section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "setup"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Project Setup" ]
        , HH.p_
            [ HH.text "Create a new PureScript project and add PS<$>D3 as a dependency:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """# Create a new directory for your project
mkdir my-d3-viz
cd my-d3-viz

# Initialize a new Spago project
spago init

# Add PS<$>D3 dependencies
# Note: Replace with actual package names once published
spago install purescript-d3-tagless""" ]
            ]
        , HH.p_
            [ HH.text "You'll also need to include D3.js in your HTML file. Create an " ]
        , HH.code_ [ HH.text "index.html" ]
        , HH.text " file:"
        , HH.pre_
            [ HH.code_
                [ HH.text """<!DOCTYPE html>
<html>
<head>
    <title>My D3 Visualization</title>
    <script src="https://d3js.org/d3.v7.min.js"></script>
</head>
<body>
    <div id="viz"></div>
    <script src="index.js"></script>
</body>
</html>""" ]
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
            [ HH.text "Let's create a simple bar chart. Create a file " ]
        , HH.code_ [ HH.text "src/Main.purs" ]
        , HH.text ":"
        , HH.pre_
            [ HH.code_
                [ HH.text """module Main where

import Prelude
import Effect (Effect)
import PSD3.Interpreter.D3 (eval_D3M)

main :: Effect Unit
main = do
  -- Your first D3 visualization code will go here
  pure unit""" ]
            ]
        , HH.p_
            [ HH.text "Build and bundle your project:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """spago build
spago bundle --to index.js""" ]
            ]
        , HH.p_
            [ HH.text "Open " ]
        , HH.code_ [ HH.text "index.html" ]
        , HH.text " in your browser to see your visualization!"
        ]

    -- Next Steps section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "next-steps"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Next Steps" ]
        , HH.p_
            [ HH.text "Now that you have PS<$>D3 installed and working, explore these resources:" ]
        , HH.ul_
            [ HH.li_
                [ HH.text "Check out the "
                , HH.a [ HP.href "#/howto" ] [ HH.text "How-to Guides" ]
                , HH.text " for step-by-step instructions on building specific visualizations"
                ]
            , HH.li_
                [ HH.text "Read the "
                , HH.a [ HP.href "#/about" ] [ HH.text "Explanation pages" ]
                , HH.text " to understand the concepts behind PS<$>D3"
                ]
            , HH.li_
                [ HH.text "Browse the "
                , HH.a [ HP.href "#/reference" ] [ HH.text "API Reference" ]
                , HH.text " for detailed documentation of all functions and types"
                ]
            ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
