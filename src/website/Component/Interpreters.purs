module PSD3.Interpreters where

import Prelude

import PSD3.RHSNavigation as RHSNav
import PSD3.Types (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slots = ( rhsNav :: forall q. H.Slot q Void Unit )

_rhsNav = Proxy :: Proxy "rhsNav"

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
    [ -- Navigation Panel (RHS)
      HH.slot_ _rhsNav unit RHSNav.component Interpreters

    -- Page header
    , HH.div
        [ HP.classes [ HH.ClassName "interpreters-page__header" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "interpreters-page__title" ] ]
            [ HH.text "Alternative Interpreters" ]
        , HH.p
            [ HP.classes [ HH.ClassName "interpreters-page__subtitle" ] ]
            [ HH.text "Demonstrating the Finally Tagless pattern: one DSL, multiple interpretations" ]
        ]

    -- Code example section
    , HH.div
        [ HP.classes [ HH.ClassName "interpreters-page__code-section" ] ]
        [ HH.h2_ [ HH.text "The Same Visualization Code..." ]
        , HH.pre
            [ HP.classes [ HH.ClassName "code-example" ] ]
            [ HH.code_
                [ HH.text """-- Simple bar chart DSL code
barChart :: forall m. D3M m => m Unit
barChart = do
  svg { width: 400, height: 300 }
  data [10, 20, 30, 40, 50]
  bars { color: "steelblue" }
  axis { position: Bottom }"""
                ]
            ]
        ]

    -- 2x2 Grid of interpreter outputs
    , HH.div
        [ HP.classes [ HH.ClassName "interpreters-page__outputs" ] ]
        [ HH.h2_ [ HH.text "...Interpreted Four Different Ways" ]
        , HH.div
            [ HP.classes [ HH.ClassName "interpreters-grid" ] ]
            [ -- Top Left: SVG Renderer
              HH.div
                [ HP.classes [ HH.ClassName "interpreter-card" ] ]
                [ HH.h3
                    [ HP.classes [ HH.ClassName "interpreter-card__title" ] ]
                    [ HH.text "1. SVG Renderer" ]
                , HH.p
                    [ HP.classes [ HH.ClassName "interpreter-card__description" ] ]
                    [ HH.text "The standard interpreter that renders visualizations as SVG graphics." ]
                , HH.div
                    [ HP.classes [ HH.ClassName "interpreter-card__output" ] ]
                    [ HH.text "[SVG visualization will appear here]" ]
                ]

            -- Top Right: HTML Table
            , HH.div
                [ HP.classes [ HH.ClassName "interpreter-card" ] ]
                [ HH.h3
                    [ HP.classes [ HH.ClassName "interpreter-card__title" ] ]
                    [ HH.text "2. HTML Table" ]
                , HH.p
                    [ HP.classes [ HH.ClassName "interpreter-card__description" ] ]
                    [ HH.text "Interprets the same code as an accessible HTML table." ]
                , HH.div
                    [ HP.classes [ HH.ClassName "interpreter-card__output" ] ]
                    [ HH.table
                        [ HP.classes [ HH.ClassName "data-table" ] ]
                        [ HH.thead_
                            [ HH.tr_
                                [ HH.th_ [ HH.text "Index" ]
                                , HH.th_ [ HH.text "Value" ]
                                ]
                            ]
                        , HH.tbody_
                            [ HH.tr_ [ HH.td_ [ HH.text "1" ], HH.td_ [ HH.text "10" ] ]
                            , HH.tr_ [ HH.td_ [ HH.text "2" ], HH.td_ [ HH.text "20" ] ]
                            , HH.tr_ [ HH.td_ [ HH.text "3" ], HH.td_ [ HH.text "30" ] ]
                            , HH.tr_ [ HH.td_ [ HH.text "..." ], HH.td_ [ HH.text "..." ] ]
                            ]
                        ]
                    ]
                ]

            -- Bottom Left: JSON/Logger
            , HH.div
                [ HP.classes [ HH.ClassName "interpreter-card" ] ]
                [ HH.h3
                    [ HP.classes [ HH.ClassName "interpreter-card__title" ] ]
                    [ HH.text "3. JSON/Logger" ]
                , HH.p
                    [ HP.classes [ HH.ClassName "interpreter-card__description" ] ]
                    [ HH.text "Logs the visualization structure as JSON for debugging." ]
                , HH.div
                    [ HP.classes [ HH.ClassName "interpreter-card__output" ] ]
                    [ HH.pre
                        [ HP.classes [ HH.ClassName "json-output" ] ]
                        [ HH.code_
                            [ HH.text """{
  "type": "svg",
  "width": 400,
  "height": 300,
  "children": [
    { "type": "bars", "data": [...] },
    { "type": "axis", "position": "bottom" }
  ]
}"""
                            ]
                        ]
                    ]
                ]

            -- Bottom Right: MetaTree Visualizer
            , HH.div
                [ HP.classes [ HH.ClassName "interpreter-card" ] ]
                [ HH.h3
                    [ HP.classes [ HH.ClassName "interpreter-card__title" ] ]
                    [ HH.text "4. MetaTree Visualizer" ]
                , HH.p
                    [ HP.classes [ HH.ClassName "interpreter-card__description" ] ]
                    [ HH.text "Visualizes the abstract syntax tree of the visualization itself." ]
                , HH.div
                    [ HP.classes [ HH.ClassName "interpreter-card__output" ] ]
                    [ HH.text "[Tree diagram showing DSL structure will appear here]" ]
                ]
            ]
        ]

    -- Explanation section
    , HH.div
        [ HP.classes [ HH.ClassName "interpreters-page__explanation" ] ]
        [ HH.h2_ [ HH.text "Why Multiple Interpreters?" ]
        , HH.p_ [ HH.text "The ability to run the same code through different interpreters provides several benefits:" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Debugging: Visualize the structure of your visualization" ]
            , HH.li_ [ HH.text "Accessibility: Generate alternative representations (tables, text)" ]
            , HH.li_ [ HH.text "Documentation: Auto-generate descriptions and logs" ]
            , HH.li_ [ HH.text "Testing: Verify structure without rendering" ]
            , HH.li_ [ HH.text "Flexibility: Add new interpretations without changing existing code" ]
            ]
        ]
    ]
