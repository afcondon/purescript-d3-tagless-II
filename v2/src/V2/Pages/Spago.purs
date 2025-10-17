module V2.Pages.Spago where

import Prelude

import V2.Router (routeToHash)
import V2.Types (Route(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Slots :: forall k. Row k
type Slots = ()

-- | Spago Explorer page
component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall state m. state -> H.ComponentHTML Unit Slots m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "spago-page" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "spago-page__header" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "spago-page__title" ] ]
            [ HH.text "Spago Dependency Explorer" ]
        , HH.p
            [ HP.classes [ HH.ClassName "spago-page__subtitle" ] ]
            [ HH.text "Interactive visualization of PureScript package dependencies" ]
        ]

    , HH.div
        [ HP.classes [ HH.ClassName "spago-page__content" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "spago-page__about" ] ]
            [ HH.h2_ [ HH.text "About This Application" ]
            , HH.p_ [ HH.text "The Spago Dependency Explorer is a real-world application demonstrating how D3 visualizations integrate into larger Halogen applications with bidirectional communication." ]
            , HH.p_ [ HH.text "This application fetches and visualizes PureScript package dependencies from your spago.dhall file, using a force-directed layout to show package relationships. Features include:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Force-directed graph layout showing package dependencies" ]
                , HH.li_ [ HH.text "Interactive drag to reposition nodes" ]
                , HH.li_ [ HH.text "Zoom and pan navigation" ]
                , HH.li_ [ HH.text "Package filtering and search" ]
                , HH.li_ [ HH.text "Bidirectional Halogen ↔ D3 event communication" ]
                ]
            , HH.p_ [ HH.text "This demonstrates what the library was built for: serious, interactive applications where visualization events trigger app-level actions and app state updates drive visualization changes." ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "spago-page__placeholder" ] ]
            [ HH.h3_ [ HH.text "Coming Soon" ]
            , HH.p_ [ HH.text "The full Spago Explorer application is being ported to V2." ]
            , HH.p_
                [ HH.text "For now, you can view it in "
                , HH.a
                    [ HP.href "../v1/#/spago"
                    , HP.target "_blank"
                    ]
                    [ HH.text "V1" ]
                , HH.text "."
                ]
            , HH.p_
                [ HH.a
                    [ HP.href $ routeToHash Gallery ]
                    [ HH.text "← Back to Gallery" ]
                ]
            ]
        ]
    ]
