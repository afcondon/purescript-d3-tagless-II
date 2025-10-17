module V2.Pages.ExampleDetail where

import Prelude

import V2.Types (ExampleId)
import V2.Data.Examples (getExample)
import V2.Router (routeToHash)
import V2.Types (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..))

type Input = ExampleId

type State = {
  exampleId :: ExampleId
}

type Slots :: forall k. Row k
type Slots = ()

-- | Example detail page (placeholder for now)
component :: forall q o m. H.Component q Input o m
component = H.mkComponent
  { initialState: \exampleId -> { exampleId }
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall m. State -> H.ComponentHTML Unit Slots m
render state =
  case getExample state.exampleId of
    Nothing ->
      HH.div
        [ HP.classes [ HH.ClassName "example-detail" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "example-detail__not-found" ] ]
            [ HH.h1_ [ HH.text "Example Not Found" ]
            , HH.p_ [ HH.text $ "Could not find example with ID: " <> state.exampleId ]
            , HH.a
                [ HP.href $ routeToHash Gallery
                , HP.classes [ HH.ClassName "example-detail__back-link" ]
                ]
                [ HH.text "← Back to Gallery" ]
            ]
        ]

    Just example ->
      HH.div
        [ HP.classes [ HH.ClassName "example-detail" ] ]
        [ -- Header
          HH.div
            [ HP.classes [ HH.ClassName "example-detail__header" ] ]
            [ HH.a
                [ HP.href $ routeToHash Gallery
                , HP.classes [ HH.ClassName "example-detail__back-link" ]
                ]
                [ HH.text "← Back to Gallery" ]
            , HH.h1
                [ HP.classes [ HH.ClassName "example-detail__title" ] ]
                [ HH.text example.title ]
            , HH.p
                [ HP.classes [ HH.ClassName "example-detail__description" ] ]
                [ HH.text example.description ]
            ]

        , -- Placeholder content
          HH.div
            [ HP.classes [ HH.ClassName "example-detail__content" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "example-detail__placeholder" ] ]
                [ HH.h2_ [ HH.text "Coming Soon" ]
                , HH.p_ [ HH.text "This is a placeholder for the full example detail view." ]
                , HH.p_ [ HH.text "In Phase 2, this will include:" ]
                , HH.ul_
                    [ HH.li_ [ HH.text "Split-pane layout with code and visualization" ]
                    , HH.li_ [ HH.text "Syntax-highlighted code" ]
                    , HH.li_ [ HH.text "Live, interactive visualization" ]
                    , HH.li_ [ HH.text "D3 JavaScript comparison" ]
                    , HH.li_ [ HH.text "Export and share options" ]
                    ]
                , HH.p_
                    [ HH.text "For now, please visit "
                    , HH.a
                        [ HP.href "../v1/"
                        , HP.target "_blank"
                        ]
                        [ HH.text "V1" ]
                    , HH.text " to see the working examples."
                    ]
                ]
            ]
        ]
