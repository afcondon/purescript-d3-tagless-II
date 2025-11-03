module PSD3.Shared.DocsCardGrid where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route)

-- | Type of documentation card
data CardType = HowTo | BestPractice | Concept | Pattern

-- | Card data
type Card =
  { title :: String
  , description :: String
  , route :: Maybe Route
  , cardType :: CardType
  }

-- | Render a grid of documentation cards
renderDocsGrid :: forall w i. Array Card -> HH.HTML w i
renderDocsGrid cards =
  HH.div
    [ HP.classes [ HH.ClassName "docs-grid" ] ]
    (map renderCard cards)

-- | Render a single documentation card
renderCard :: forall w i. Card -> HH.HTML w i
renderCard { title, description, route, cardType } =
  let
    baseClasses = [ HH.ClassName "docs-card" ]
    typeClass = case cardType of
      HowTo -> HH.ClassName "docs-card--howto"
      BestPractice -> HH.ClassName "docs-card--best-practice"
      Concept -> HH.ClassName "docs-card--concept"
      Pattern -> HH.ClassName "docs-card--pattern"
    allClasses = baseClasses <> [ typeClass ]

    typeLabel = case cardType of
      HowTo -> "How-to"
      BestPractice -> "Best Practice"
      Concept -> "Concept"
      Pattern -> "Pattern"

    element = case route of
      Just r ->
        HH.a
          [ HP.href $ "#" <> routeToPath r
          , HP.classes allClasses
          ]
      Nothing ->
        HH.div
          ([ HP.classes (allClasses <> [ HH.ClassName "docs-card--coming-soon" ]) ])
  in
    element
      [ HH.div
          [ HP.classes [ HH.ClassName "docs-card-header" ] ]
          [ HH.span
              [ HP.classes [ HH.ClassName "docs-card-type" ] ]
              [ HH.text typeLabel ]
          , case route of
              Nothing ->
                HH.span
                  [ HP.classes [ HH.ClassName "docs-card-status" ] ]
                  [ HH.text "Coming Soon" ]
              Just _ -> HH.text ""
          ]
      , HH.h3
          [ HP.classes [ HH.ClassName "docs-card-title" ] ]
          [ HH.text title ]
      , HH.p
          [ HP.classes [ HH.ClassName "docs-card-description" ] ]
          [ HH.text description ]
      ]
