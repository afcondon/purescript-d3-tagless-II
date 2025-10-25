module PSD3.Tutorial.GettingStarted where -- Tutorial

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.RHSNavigation as RHSNav
import PSD3.Understanding.TOC (renderTOC)
import PSD3.Website.Types (Route(..))
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
            , { anchor: "wizard", label: "Using the Wizard", level: 0 }
            , { anchor: "understanding", label: "Understanding the Code", level: 0 }
            , { anchor: "datum-pattern", label: "The Datum_ Pattern", level: 1 }
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
            [ HH.text "Welcome! This guide will help you install PSD3, set up your first project, and create your first data visualization using our scaffold wizard. By the end, you'll have a working visualization that you can view in your browser." ]
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
            [ HH.text "Clone the PSD3 repository which includes the visualization wizard:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """# Clone the PSD3 repository
git clone https://github.com/afcondon/PureScript-Tagless-D3.git
cd PureScript-Tagless-D3

# Install dependencies
npm install
spago build""" ]
            ]
        , HH.p_
            [ HH.text "The wizard is located in " ]
        , HH.code_ [ HH.text "scripts/init-psd3-viz.js" ]
        , HH.text " and will generate all the files you need to get started."
        ]

    -- Using the Wizard section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "wizard"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Using the Wizard" ]
        , HH.p_
            [ HH.text "The wizard will generate a complete visualization scaffold for you. Run it from the project root:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text "node scripts/init-psd3-viz.js" ]
            ]
        , HH.p_
            [ HH.text "You'll be prompted for:" ]
        , HH.ul_
            [ HH.li_
                [ HH.strong_ [ HH.text "Visualization module name" ]
                , HH.text " - e.g., "
                , HH.code_ [ HH.text "MyFirstChart" ]
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "Data record fields" ]
                , HH.text " - e.g., "
                , HH.code_ [ HH.text "x:Number,y:Number,label:String" ]
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "Output directory" ]
                , HH.text " - default: "
                , HH.code_ [ HH.text "src/viz/YourModuleName" ]
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "Generate Main.purs?" ]
                , HH.text " - Entry point for standalone viewing (y/n, default: y)"
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "Generate index.html?" ]
                , HH.text " - (y/n, default: y)"
                ]
            ]
        , HH.h3_ [ HH.text "Example Session" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """Visualization module name: ParabolaChart
Data record fields: x:Number,y:Number
Output directory: src/viz/ParabolaChart
Generate Main.purs? (y/n): y
Generate index.html? (y/n): y

✓ Created src/viz/ParabolaChart/Unsafe.purs
✓ Created src/viz/ParabolaChart/Model.purs
✓ Created src/viz/ParabolaChart/Draw.purs
✓ Created src/viz/ParabolaChart/Main.purs
✓ Created src/viz/ParabolaChart/index.html
✓ Created src/viz/ParabolaChart/README.md""" ]
            ]
        , HH.h3_ [ HH.text "Build and View" ]
        , HH.p_
            [ HH.text "Now build and bundle your visualization:" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """# Compile the PureScript
spago build

# Bundle for the browser
spago bundle --module Main --outfile src/viz/ParabolaChart/bundle.js

# Open in browser
open src/viz/ParabolaChart/index.html""" ]
            ]
        ]

    -- Understanding the Code section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "understanding"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Understanding the Generated Code" ]
        , HH.p_
            [ HH.text "The wizard generates several files following PSD3 best practices:" ]
        , HH.ul_
            [ HH.li_
                [ HH.strong_ [ HH.text "Unsafe.purs" ]
                , HH.text " - Contains type coercion functions that bridge PureScript's type system with D3's untyped JavaScript data"
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "Model.purs" ]
                , HH.text " - Defines your data type and provides a placeholder for example data"
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "Draw.purs" ]
                , HH.text " - Contains the visualization code with the "
                , HH.code_ [ HH.text "datum_" ]
                , HH.text " accessor pattern"
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "index.html" ]
                , HH.text " - Pre-configured HTML with D3.js loaded from CDN and the correct "
                , HH.code_ [ HH.text "#chart" ]
                , HH.text " div"
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "README.md" ]
                , HH.text " - Quick reference specific to your visualization"
                ]
            ]

        , HH.h3
            [ HP.id "datum-pattern" ]
            [ HH.text "The Datum_ Pattern" ]
        , HH.p_
            [ HH.text "D3.js works with untyped JavaScript data, but PureScript is strongly typed. The "
            , HH.code_ [ HH.text "datum_" ]
            , HH.text " accessor pattern solves this by isolating type coercion in the "
            , HH.code_ [ HH.text "Unsafe.purs" ]
            , HH.text " module while providing typed accessors everywhere else."
            ]
        , HH.p_
            [ HH.text "In your "
            , HH.code_ [ HH.text "Draw.purs" ]
            , HH.text ", you'll use type-annotated lambdas to work with your data:"
            ]
        , HH.pre_
            [ HH.code_
                [ HH.text """-- Use data in attributes
A.cx (\\(d :: Datum_) _ -> datum_.x d)

-- Use both data and index
A.fill (\\(d :: Datum_) (i :: Index_) ->
  if datum_.index i > 5 then "red" else "blue")

-- Scale by data value
A.radius (\\(d :: Datum_) _ -> datum_.y d * 2.0)""" ]
            ]
        , HH.p_
            [ HH.text "The type annotations "
            , HH.code_ [ HH.text "(d :: Datum_)" ]
            , HH.text " and "
            , HH.code_ [ HH.text "(i :: Index_)" ]
            , HH.text " are required to help PureScript's type checker find the correct "
            , HH.code_ [ HH.text "ToAttr" ]
            , HH.text " instance."
            ]
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
