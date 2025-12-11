module PSD3.Acknowledgements where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import D3.Viz.TreeAPI.SankeyDiagram (drawSankey)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.Footer as Footer
import PSD3.Shared.SiteNav as SiteNav

-- | Acknowledgements page state
type State = Unit

-- | Acknowledgements page actions
data Action = Initialize

-- | Acknowledgements page component
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall w. State -> HH.HTML w Action
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "acknowledgements-page" ] ]
    [ -- Header with logo and navigation
      SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Nothing
        }
    , HH.div
        [ HP.classes [ HH.ClassName "acknowledgements-content" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "acknowledgements-title" ] ]
            [ HH.text "Acknowledgements" ]

        , HH.section
            [ HP.classes [ HH.ClassName "acknowledgements-section" ] ]
            [ HH.h2_ [ HH.text "D3.js and Data Visualization" ]
            , HH.p_
                [ HH.text "This project would not exist without "
                , HH.a [ HP.href "https://d3js.org/" ] [ HH.text "D3.js" ]
                , HH.text ", created by "
                , HH.a [ HP.href "https://bost.ocks.org/mike/" ] [ HH.text "Mike Bostock" ]
                , HH.text ". D3 revolutionized web-based data visualization and established patterns that have become fundamental to the field. Many of the examples in PS<$>D3 are adapted from Mike's extensive collection of examples and tutorials."
                ]
            , HH.p_
                [ HH.text "Thanks so much to Mike for creating D3, for his clear documentation and examples, and for making the library open source. His work on Observable and continued contributions to visualization continue to inspire."
                ]
            , HH.h3_ [ HH.text "Library Dependencies" ]
            , HH.p_
                [ HH.text "This diagram shows how D3 modules flow into our library packages. Note D3's own internal dependencies on the left - modules like d3-zoom depend on d3-transition, d3-drag, etc. Unused D3 modules are tree-shaken from the bundle."
                ]
            , HH.div
                [ HP.id "d3-library-sankey"
                , HP.style "background: #fafafa; border-radius: 8px; padding: 10px;"
                ]
                []
            ]
        , HH.section
            [ HP.classes [ HH.ClassName "acknowledgements-section" ] ]
            [ HH.h2_ [ HH.text "Website Dependencies" ]
            , HH.p_
                [ HH.text "This diagram shows how the demo website depends on our libraries. The SimulationManager represents temporary FFI code that will eventually move into psd3-simulation."
                ]
            , HH.div
                [ HP.id "d3-website-sankey"
                , HP.style "background: #fafafa; border-radius: 8px; padding: 10px;"
                ]
                []
            ]

        , HH.section
            [ HP.classes [ HH.ClassName "acknowledgements-section" ] ]
            [ HH.h2_ [ HH.text "Observable Examples (ISC License)" ]
            , HH.p_
                [ HH.text "Several chart examples in PS<$>D3 are based on visualizations from "
                , HH.a [ HP.href "https://observablehq.com/" ] [ HH.text "Observable" ]
                , HH.text ", licensed under the ISC License. These include:"
                ]
            , HH.ul_
                [ HH.li_
                    [ HH.a [ HP.href "https://observablehq.com/@d3/grouped-bar-chart/2" ] [ HH.text "Grouped Bar Chart" ]
                    , HH.text " (Copyright 2018–2020 Observable, Inc.)"
                    ]
                , HH.li_
                    [ HH.a [ HP.href "https://observablehq.com/@d3/multi-line-chart/2" ] [ HH.text "Multi-Line Chart" ]
                    , HH.text " (Copyright 2018–2023 Observable, Inc.)"
                    ]
                , HH.li_
                    [ HH.a [ HP.href "https://observablehq.com/@d3/radial-stacked-bar-chart/2" ] [ HH.text "Radial Stacked Bar Chart" ]
                    , HH.text " (Copyright 2019–2023 Observable, Inc., created by Mike Bostock)"
                    ]
                ]
            , HH.p_
                [ HH.text "These examples have been reimplemented in PureScript to demonstrate the PS<$>D3 library's capabilities while faithfully reproducing the original visualizations' dimensions, scales, and styling."
                ]
            ]

        , HH.section
            [ HP.classes [ HH.ClassName "acknowledgements-section" ] ]
            [ HH.h2_ [ HH.text "Data Visualization Pioneers" ]
            , HH.p_
                [ HH.a [ HP.href "https://www.edwardtufte.com/" ] [ HH.text "Edward Tufte" ]
                , HH.text "'s work on information design and visual communication established many of the principles we follow today. His books remain essential reading for anyone interested in effective data presentation."
                ]
            , HH.p_
                [ HH.a [ HP.href "https://www.gapminder.org/about/hans-rosling/" ] [ HH.text "Hans Rosling" ]
                , HH.text "'s passionate advocacy for data literacy and his innovative animated visualizations (particularly the Gapminder World visualization) showed how data can tell compelling stories. The Wealth & Health example in this project is inspired by his work."
                ]
            ]

        , HH.section
            [ HP.classes [ HH.ClassName "acknowledgements-section" ] ]
            [ HH.h2_ [ HH.text "PureScript and Functional Programming" ]
            , HH.p_
                [ HH.a [ HP.href "http://functorial.com/" ] [ HH.text "Phil Freeman" ]
                , HH.text ", for his presentation and example of finally tagless encodings in PureScript which provided the spark for this library's architecture."
                ]
            , HH.p_
                [ HH.text "Ian Ross, for his solution to the polymorphic attribute architecture with typeclasses, so neat."
                ]
            , HH.p_
                [ HH.text "The PureScript Core Team members, past, present and future and the wider PureScript community for building this incredibly nice language and ecosystem. I'd particularly like to single out Mike Solomon, Jordan Martinez, Thomas Honeyman, Fabrizio Ferrai who listened to me drone on about this on conference calls."
                ]
            ]

        , HH.section
            [ HP.classes [ HH.ClassName "acknowledgements-section" ] ]
            [ HH.h2_ [ HH.text "AI Collaboration" ]
            , HH.p_
                [ HH.text "The 2024-2025 iteration of this project was developed in collaboration with Claude (Anthropic). This represents a significant experiment in human-AI pair programming on a complex, long-running project."
                ]
            , HH.p_
                [ HH.text "The intellectual foundations of PS<$>D3 were developed over several years: the initial PureScript D3 bindings (2019), the finally tagless encoding inspired by Phil Freeman's work, and the Attr typeclass pattern adapted from Ian Ross's Haskell code. Claude contributed substantially to the 2024-2025 implementation: the Tree API design, phantom type refinements, extensive refactoring, documentation, and tens of thousands of lines of code."
                ]
            , HH.p_
                [ HH.text "We believe transparency about AI involvement is important. This collaboration demonstrates both the potential and the nature of human-AI development: the human brings domain expertise, architectural vision, and quality judgment; the AI brings rapid implementation, pattern recognition across large codebases, and tireless iteration. Neither could have produced this result alone."
                ]
            ]

        , HH.section
            [ HP.classes [ HH.ClassName "acknowledgements-section" ] ]
            [ HH.h2_ [ HH.text "About This Project" ]
            , HH.p_
                [ HH.text "PS<$>D3 is an experimental exploration of what data visualization APIs might look like in a strongly-typed functional programming language. It aims to preserve D3's flexibility while adding type safety and composability."
                ]
            , HH.p_
                [ HH.text "This project is open source and available on GitHub. Contributions, feedback, and suggestions are always welcome."
                ]
            ]
        ]
    -- Footer
    , Footer.render
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Small delay to ensure DOM is ready
    liftAff $ delay (Milliseconds 100.0)
    -- Load and draw Library Dependencies Sankey
    libraryResult <- liftAff $ AX.get ResponseFormat.string "/data/d3-library-deps.csv"
    case libraryResult of
      Left _ -> pure unit
      Right response ->
        liftEffect $ drawSankey response.body "#d3-library-sankey" 740.0 500.0
    -- Load and draw Website Dependencies Sankey
    websiteResult <- liftAff $ AX.get ResponseFormat.string "/data/d3-website-deps.csv"
    case websiteResult of
      Left _ -> pure unit
      Right response ->
        liftEffect $ drawSankey response.body "#d3-website-sankey" 700.0 220.0
