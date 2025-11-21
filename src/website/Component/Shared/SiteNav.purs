module PSD3.Shared.SiteNav
  ( Quadrant(..)
  , LogoSize(..)
  , SiteNavConfig
  , component
  , render
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

-- | Which quadrant to highlight (NoQuadrant = don't show quadrant nav)
data Quadrant
  = NoQuadrant
  | QuadGettingStarted
  | QuadHowTo
  | QuadReference
  | QuadUnderstanding

derive instance eqQuadrant :: Eq Quadrant

-- | Logo size variant
data LogoSize = Large | Normal

derive instance eqLogoSize :: Eq LogoSize

-- | Configuration for the site navigation
type SiteNavConfig =
  { logoSize :: LogoSize
  , quadrant :: Quadrant
  , prevNext :: Maybe { prev :: Maybe Route, next :: Maybe Route }
  , pageTitle :: Maybe String
  }

-- | Default configuration for most pages
defaultConfig :: SiteNavConfig
defaultConfig =
  { logoSize: Normal
  , quadrant: NoQuadrant
  , prevNext: Nothing
  , pageTitle: Nothing
  }

-- | Halogen component wrapper (for slot usage)
component :: forall q o m. H.Component q SiteNavConfig o m
component = H.mkComponent
  { initialState: identity
  , render: \config -> render config
  , eval: H.mkEval H.defaultEval
  }

-- | Pure render function (for direct HTML embedding)
render :: forall w i. SiteNavConfig -> HH.HTML w i
render config =
  HH.header
    [ HP.classes [ HH.ClassName "site-nav" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "site-nav-content" ] ]
        [ -- Left section: Logo and optional title
          HH.div
            [ HP.classes [ HH.ClassName "site-nav-left" ] ]
            [ HH.a
                [ HP.href $ "#" <> routeToPath Home
                , HP.classes [ HH.ClassName "site-nav-logo-link" ]
                ]
                [ HH.img
                    [ HP.src "assets/psd3-logo-color.svg"
                    , HP.alt "PSD3 Logo"
                    , HP.classes
                        [ HH.ClassName "site-nav-logo"
                        , HH.ClassName $ case config.logoSize of
                            Large -> "site-nav-logo--large"
                            Normal -> "site-nav-logo--normal"
                        ]
                    ]
                ]
            , case config.pageTitle of
                Nothing -> HH.text ""
                Just title ->
                  HH.div
                    [ HP.classes [ HH.ClassName "site-nav-title-container" ] ]
                    [ HH.h1
                        [ HP.classes [ HH.ClassName "site-nav-title" ] ]
                        [ HH.text title ]
                    ]
            ]

        -- Center section: Quadrant nav (if not NoQuadrant)
        , case config.quadrant of
            NoQuadrant -> HH.text ""
            _ ->
              HH.div
                [ HP.classes [ HH.ClassName "site-nav-center" ] ]
                [ HH.div
                    [ HP.classes [ HH.ClassName "site-nav-quadrant" ] ]
                    [ renderQuadrantBox QuadGettingStarted config.quadrant
                    , renderQuadrantBox QuadHowTo config.quadrant
                    , renderQuadrantBox QuadReference config.quadrant
                    , renderQuadrantBox QuadUnderstanding config.quadrant
                    ]
                ]

        -- Right section: Nav links and prev/next
        , HH.div
            [ HP.classes [ HH.ClassName "site-nav-right" ] ]
            [ -- Standard nav links
              HH.nav
                [ HP.classes [ HH.ClassName "site-nav-links" ] ]
                [ HH.a
                    [ HP.href $ "#" <> routeToPath TourFoundations
                    , HP.classes [ HH.ClassName "site-nav-link" ]
                    ]
                    [ HH.text "Tour" ]
                , HH.a
                    [ HP.href $ "#" <> routeToPath Gallery
                    , HP.classes [ HH.ClassName "site-nav-link" ]
                    ]
                    [ HH.text "Examples" ]
                , HH.a
                    [ HP.href $ "#" <> routeToPath CodeExplorer
                    , HP.classes [ HH.ClassName "site-nav-link" ]
                    ]
                    [ HH.text "Code Explorer" ]
                , HH.a
                    [ HP.href "api/index.html"
                    , HP.classes [ HH.ClassName "site-nav-link" ]
                    ]
                    [ HH.text "API" ]
                ]
            -- Prev/Next buttons (if provided)
            , case config.prevNext of
                Nothing -> HH.text ""
                Just { prev, next } ->
                  HH.div
                    [ HP.classes [ HH.ClassName "site-nav-prevnext" ] ]
                    [ case prev of
                        Nothing ->
                          HH.span
                            [ HP.classes [ HH.ClassName "site-nav-button", HH.ClassName "site-nav-button--disabled" ] ]
                            [ HH.text "←" ]
                        Just prevRoute ->
                          HH.a
                            [ HP.href $ "#" <> routeToPath prevRoute
                            , HP.classes [ HH.ClassName "site-nav-button" ]
                            , HP.title $ show prevRoute
                            ]
                            [ HH.text "←" ]
                    , case next of
                        Nothing ->
                          HH.span
                            [ HP.classes [ HH.ClassName "site-nav-button", HH.ClassName "site-nav-button--disabled" ] ]
                            [ HH.text "→" ]
                        Just nextRoute ->
                          HH.a
                            [ HP.href $ "#" <> routeToPath nextRoute
                            , HP.classes [ HH.ClassName "site-nav-button" ]
                            , HP.title $ show nextRoute
                            ]
                            [ HH.text "→" ]
                    ]
            ]
        ]
    ]

-- | Render a single quadrant box
renderQuadrantBox :: forall w i. Quadrant -> Quadrant -> HH.HTML w i
renderQuadrantBox target current =
  HH.a
    [ HP.href $ quadrantRoute target
    , HP.classes
        [ HH.ClassName "site-nav-quadrant-box"
        , HH.ClassName $ if target == current
            then "site-nav-quadrant-box--active"
            else "site-nav-quadrant-box--inactive"
        ]
    , HP.title $ quadrantTitle target
    ]
    []

-- | Get route for a quadrant
quadrantRoute :: Quadrant -> String
quadrantRoute = case _ of
  NoQuadrant -> "#"
  QuadGettingStarted -> "#" <> routeToPath GettingStarted
  QuadHowTo -> "#" <> routeToPath HowtoIndex
  QuadReference -> "api/index.html"
  QuadUnderstanding -> "#" <> routeToPath Understanding

-- | Get display title for a quadrant
quadrantTitle :: Quadrant -> String
quadrantTitle = case _ of
  NoQuadrant -> ""
  QuadGettingStarted -> "Getting Started"
  QuadHowTo -> "How-To Guides"
  QuadReference -> "API Reference"
  QuadUnderstanding -> "Understanding"
