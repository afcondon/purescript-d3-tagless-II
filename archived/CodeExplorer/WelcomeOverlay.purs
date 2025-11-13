module PSD3.CodeExplorer.WelcomeOverlay where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | Welcome overlay explaining Code Explorer controls
-- | Shows on first load, can be dismissed
render :: forall p i. i -> HH.HTML p i
render dismissAction =
  HH.div
    [ HP.classes [ HH.ClassName "welcome-overlay" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "welcome-overlay__backdrop" ]
        , HE.onClick (const dismissAction)
        ]
        []
    , HH.div
        [ HP.classes [ HH.ClassName "welcome-overlay__content" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "welcome-overlay__header" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "welcome-overlay__title" ] ]
                [ HH.text "Welcome to Code Explorer" ]
            , HH.button
                [ HP.classes [ HH.ClassName "welcome-overlay__close" ]
                , HE.onClick (const dismissAction)
                ]
                [ HH.text "×" ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "welcome-overlay__body" ] ]
            [ HH.p
                [ HP.classes [ HH.ClassName "welcome-overlay__intro" ] ]
                [ HH.text "Explore the PureScript package dependency graph with interactive force-directed layouts." ]

            , HH.div
                [ HP.classes [ HH.ClassName "welcome-overlay__section" ] ]
                [ HH.h3_ [ HH.text "🎮 Interactive Controls" ]
                , HH.p_
                    [ HH.text "Try zooming, clicking, and dragging nodes!" ]
                , HH.ul_
                    [ HH.li_ [ HH.text "Zoom: Scroll wheel or pinch to zoom in/out" ]
                    , HH.li_ [ HH.text "Pan: Click and drag the background" ]
                    , HH.li_ [ HH.text "Move nodes: Click and drag individual nodes" ]
                    ]
                ]

            , HH.div
                [ HP.classes [ HH.ClassName "welcome-overlay__section" ] ]
                [ HH.h3_ [ HH.text "◀ Left Panel: Scene Selection" ]
                , HH.p_
                    [ HH.text "Control what visualization you see:" ]
                , HH.ul_
                    [ HH.li_ [ HH.text "Package Grid: Cluster view of packages" ]
                    , HH.li_ [ HH.text "Package Graph: Force-directed package relationships" ]
                    , HH.li_ [ HH.text "Module Trees: Hierarchical module layouts (horizontal, vertical, radial)" ]
                    , HH.li_ [ HH.text "Layer Swarm: Modules organized by dependency layers" ]
                    ]
                ]

            , HH.div
                [ HP.classes [ HH.ClassName "welcome-overlay__section" ] ]
                [ HH.h3_ [ HH.text "▶ Right Panel: Force Controls" ]
                , HH.p_
                    [ HH.text "Activate and deactivate physics forces that govern the layout:" ]
                , HH.ul_
                    [ HH.li_ [ HH.text "Click force names to toggle them on/off" ]
                    , HH.li_ [ HH.text "Adjust parameters with sliders" ]
                    , HH.li_ [ HH.text "Watch the simulation respond in real-time" ]
                    ]
                ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "welcome-overlay__footer" ] ]
            [ HH.button
                [ HP.classes [ HH.ClassName "button", HH.ClassName "button--primary" ]
                , HE.onClick (const dismissAction)
                ]
                [ HH.text "Got it, let's explore!" ]
            ]
        ]
    ]
