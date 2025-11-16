module PSD3.Understanding.TOC where -- Understanding

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties (attr)
import Halogen.HTML.Core (AttrName(..))
import PSD3.Website.Types (Route)
import PSD3.RoutingDSL (routeToPath)

-- | Link target - either an anchor on the same page or a route to another page
data TOCLinkTarget
  = AnchorLink String  -- #anchor on the current page
  | RouteLink Route    -- Link to another route

-- | A single item in the table of contents
type TOCItem =
  { target :: TOCLinkTarget  -- Where the link goes
  , label :: String          -- The display text for the link
  , level :: Int             -- Indentation level (0 = top level, 1 = level-2, 2 = level-3, etc.)
  }

-- | Configuration for the TOC panel
type TOCConfig =
  { title :: String           -- Title for the TOC (e.g., "Contents", "Interpreters")
  , items :: Array TOCItem    -- The navigation items
  , image :: Maybe String     -- Optional image to use instead of bookmark.jpeg
  }

-- | Render a table of contents panel
-- | This creates the standard LHS TOC panel with bookmark pin styling
renderTOC :: forall w i. TOCConfig -> HH.HTML w i
renderTOC config =
  HH.div
    [ HP.classes [ HH.ClassName "toc-panel" ] ]
    [ HH.img
        [ HP.src $ fromMaybe "bookmark.jpeg" config.image
        , HP.alt ""
        , HP.classes [ HH.ClassName "toc-panel__bookmark-pin" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "toc-panel__main" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "floating-panel__header" ] ]
            [ HH.h3
                [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                [ HH.text config.title ]
            , HH.button
                [ HP.classes [ HH.ClassName "floating-panel__toggle" ]
                , HP.type_ HP.ButtonButton
                ]
                [ HH.text "−" ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "floating-panel__content", HH.ClassName "toc-panel__content" ] ]
            [ HH.nav
                [ HP.classes [ HH.ClassName "toc-nav" ] ]
                (renderTOCItem <$> config.items)
            ]
        ]
    ]

-- | Render a single TOC navigation item
renderTOCItem :: forall w i. TOCItem -> HH.HTML w i
renderTOCItem item =
  let
    baseClass = HH.ClassName "toc-nav__item"
    levelClass = case item.level of
      0 -> []
      n -> [ HH.ClassName $ "toc-nav__item--level-" <> show (n + 1) ]
    routeLinkClass = case item.target of
      RouteLink _ -> [ HH.ClassName "toc-nav__item--route-link" ]
      _ -> []
    classes = [ baseClass ] <> levelClass <> routeLinkClass
  in
    case item.target of
      -- For anchor links, use data attribute for JavaScript handling
      AnchorLink anchor ->
        HH.a
          [ HP.href $ "#" <> anchor  -- Keep href for accessibility
          , HP.classes classes
          , attr (AttrName "data-anchor-link") anchor  -- Mark as anchor link
          ]
          [ HH.text item.label ]
      -- For route links, use normal navigation
      RouteLink route ->
        HH.a
          [ HP.href $ "#" <> routeToPath route
          , HP.classes classes
          ]
          [ HH.text item.label ]

-- | Helper: Create a TOC item that links to an anchor on the current page
tocAnchor :: String -> String -> Int -> TOCItem
tocAnchor anchor label level =
  { target: AnchorLink anchor
  , label
  , level
  }

-- | Helper: Create a TOC item that links to another route (e.g., How-to guide)
tocRoute :: Route -> String -> Int -> TOCItem
tocRoute route label level =
  { target: RouteLink route
  , label
  , level
  }

-- | This import ensures the JavaScript anchor handling code is loaded
-- | The JS file contains a self-executing function that sets up click handlers
foreign import initAnchorLinks :: Unit
