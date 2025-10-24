module PSD3.TOC where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | A single item in the table of contents
type TOCItem =
  { anchor :: String  -- The #id to link to (without the #)
  , label :: String   -- The display text for the link
  , level :: Int      -- Indentation level (0 = top level, 1 = level-2, 2 = level-3, etc.)
  }

-- | Configuration for the TOC panel
type TOCConfig =
  { title :: String           -- Title for the TOC (e.g., "Contents", "Interpreters")
  , items :: Array TOCItem    -- The navigation items
  }

-- | Render a table of contents panel
-- | This creates the standard LHS TOC panel with bookmark pin styling
renderTOC :: forall w i. TOCConfig -> HH.HTML w i
renderTOC config =
  HH.div
    [ HP.classes [ HH.ClassName "toc-panel" ] ]
    [ HH.img
        [ HP.src "bookmark.jpeg"
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
    classes = [ baseClass ] <> levelClass
  in
    HH.a
      [ HP.href $ "#" <> item.anchor
      , HP.classes classes
      ]
      [ HH.text item.label ]
