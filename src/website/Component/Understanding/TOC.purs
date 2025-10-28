module PSD3.Understanding.TOC where -- Understanding

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
    href = case item.target of
      AnchorLink anchor -> "#" <> anchor
      RouteLink route -> "#" <> routeToPath route
  in
    HH.a
      [ HP.href href
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
