module PSD3.Pages.Interpreters where

import Prelude

import PSD3.Router (routeToHash)
import PSD3.Types (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Slots :: forall k. Row k
type Slots = ()

-- | Interpreters page
component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall state m. state -> H.ComponentHTML Unit Slots m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "interpreters-page" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "interpreters-page__header" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "interpreters-page__title" ] ]
            [ HH.text "Alternative Interpreters" ]
        , HH.p
            [ HP.classes [ HH.ClassName "interpreters-page__subtitle" ] ]
            [ HH.text "Demonstrating the Finally Tagless pattern" ]
        ]

    , HH.div
        [ HP.classes [ HH.ClassName "interpreters-page__content" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "interpreters-page__about" ] ]
            [ HH.h2_ [ HH.text "What is Finally Tagless?" ]
            , HH.p_ [ HH.text "Finally Tagless is an advanced functional programming pattern that allows the same code to be interpreted in multiple ways. Instead of tying your code to a specific implementation, you write against a set of type class capabilities." ]
            , HH.p_ [ HH.text "This library demonstrates this by providing multiple interpreters for the same visualization code:" ]
            ]

        , HH.div
            [ HP.classes [ HH.ClassName "interpreters-page__grid" ] ]
            [ -- MetaTree Visualizer
              HH.div
                [ HP.classes [ HH.ClassName "interpreter-card" ] ]
                [ HH.h3_ [ HH.text "MetaTree Visualizer" ]
                , HH.p_ [ HH.text "Instead of rendering to SVG, this interpreter visualizes the abstract syntax tree of the visualization itself as a tree diagram." ]
                , HH.p_ [ HH.text "This meta-visualization shows how the same code can be interpreted in completely different ways - a key advantage of the Finally Tagless approach." ]
                , HH.a
                    [ HP.href $ routeToHash (Example "meta-tree")
                    , HP.classes [ HH.ClassName "interpreter-card__link" ]
                    ]
                    [ HH.text "View MetaTree →" ]
                ]

            , -- String Generator
              HH.div
                [ HP.classes [ HH.ClassName "interpreter-card" ] ]
                [ HH.h3_ [ HH.text "String Generator" ]
                , HH.p_ [ HH.text "This interpreter takes visualization code and generates human-readable text descriptions or code snippets." ]
                , HH.p_ [ HH.text "Shows how the same high-level visualization definition can be used for documentation generation, code analysis, or teaching - all without modifying the original visualization code." ]
                , HH.a
                    [ HP.href $ routeToHash (Example "print-tree")
                    , HP.classes [ HH.ClassName "interpreter-card__link" ]
                    ]
                    [ HH.text "View String Generator →" ]
                ]
            ]

        , HH.div
            [ HP.classes [ HH.ClassName "interpreters-page__explanation" ] ]
            [ HH.h3_ [ HH.text "Why Multiple Interpreters?" ]
            , HH.p_ [ HH.text "The ability to run the same code through different interpreters provides several benefits:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Debugging: Visualize the structure of your visualization" ]
                , HH.li_ [ HH.text "Documentation: Generate descriptions automatically" ]
                , HH.li_ [ HH.text "Testing: Verify structure without rendering" ]
                , HH.li_ [ HH.text "Flexibility: Add new interpretations without changing existing code" ]
                ]
            , HH.p_
                [ HH.a
                    [ HP.href $ routeToHash Gallery ]
                    [ HH.text "← Back to Gallery" ]
                ]
            ]
        ]
    ]
