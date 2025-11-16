module PSD3.Shared.TutorialNav
  ( renderHeader
  , allTutorialRoutes
  , getNextTutorial
  , getPrevTutorial
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

-- | All tutorial pages in order
allTutorialRoutes :: Array Route
allTutorialRoutes =
  [ TreeAPI
  ]

-- | Get the next tutorial in the sequence
getNextTutorial :: Route -> Maybe Route
getNextTutorial currentRoute =
  case Array.findIndex (\r -> r == currentRoute) allTutorialRoutes of
    Nothing -> Nothing
    Just idx -> allTutorialRoutes Array.!! (idx + 1)

-- | Get the previous tutorial in the sequence
getPrevTutorial :: Route -> Maybe Route
getPrevTutorial currentRoute =
  case Array.findIndex (\r -> r == currentRoute) allTutorialRoutes of
    Nothing -> Nothing
    Just idx -> if idx > 0 then allTutorialRoutes Array.!! (idx - 1) else Nothing

-- | Render the header with logo, navigation, and prev/next buttons
renderHeader :: forall w i. Route -> HH.HTML w i
renderHeader currentRoute =
  HH.header
    [ HP.classes [ HH.ClassName "example-header" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "example-header-left" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath Home
            , HP.classes [ HH.ClassName "example-logo-link" ]
            ]
            [ HH.img
                [ HP.src "assets/psd3-logo-color.svg"
                , HP.alt "PSD3 Logo"
                , HP.classes [ HH.ClassName "example-logo" ]
                ]
            ]
        , HH.a
            [ HP.href $ "#" <> routeToPath Home <> "tutorials"
            , HP.classes [ HH.ClassName "example-gallery-link" ]
            ]
            [ HH.text "Tour" ]
        , HH.div
            [ HP.classes [ HH.ClassName "example-title-container" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "example-title" ] ]
                [ HH.text $ show currentRoute ]
            ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "example-header-right" ] ]
        [ case getPrevTutorial currentRoute of
            Nothing ->
              HH.span
                [ HP.classes [ HH.ClassName "example-nav-button", HH.ClassName "disabled" ] ]
                [ HH.text "← Previous" ]
            Just prevRoute ->
              HH.a
                [ HP.href $ "#" <> routeToPath prevRoute
                , HP.classes [ HH.ClassName "example-nav-button" ]
                ]
                [ HH.text "← Previous" ]
        , case getNextTutorial currentRoute of
            Nothing ->
              HH.span
                [ HP.classes [ HH.ClassName "example-nav-button", HH.ClassName "disabled" ] ]
                [ HH.text "Next →" ]
            Just nextRoute ->
              HH.a
                [ HP.href $ "#" <> routeToPath nextRoute
                , HP.classes [ HH.ClassName "example-nav-button" ]
                ]
                [ HH.text "Next →" ]
        ]
    ]
