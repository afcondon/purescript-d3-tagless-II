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
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))

-- | All tutorial pages in order (WealthHealth and Showcase moved to Showcase section)
allTutorialRoutes :: Array Route
allTutorialRoutes =
  [ TourFoundations
  , TourProfessional
  , TourFlow
  , TourHierarchies
  , TourMotion
  , TourInterpreters
  , TourFPFTW
  , TourGraphAlgorithms
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
  SiteNav.render
    { logoSize: SiteNav.Large
    , quadrant: SiteNav.NoQuadrant
    , prevNext: Just
        { prev: getPrevTutorial currentRoute
        , next: getNextTutorial currentRoute
        }
    , pageTitle: Just $ show currentRoute
    }
