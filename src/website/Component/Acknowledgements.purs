module PSD3.Acknowledgements where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.Footer as Footer

-- | Acknowledgements page state
type State = Unit

-- | Acknowledgements page actions
data Action = Initialize

-- | Acknowledgements page component
component :: forall q i o. H.Component q i o Aff
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
    [ HH.div
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

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> pure unit
