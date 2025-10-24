module PSD3.CodeExample where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Types (Route(..))
import PSD3.Utilities (syntaxHighlightedCode)

-- | Configuration for a code example display
type CodeExampleConfig =
  { code :: String           -- The code to display
  , snippetId :: String      -- ID for the snippet (e.g., "TLCSimple")
  , title :: String          -- Optional title to show above code
  }

-- | Render a code example with syntax highlighting and an "Explore" button
renderCodeExample :: forall w i. CodeExampleConfig -> HH.HTML w i
renderCodeExample config =
  HH.div
    [ HP.classes [ HH.ClassName "code-example" ] ]
    [ -- Optional title
      if config.title == ""
        then HH.text ""
        else HH.div
          [ HP.classes [ HH.ClassName "code-example__title" ] ]
          [ HH.text config.title ]

    -- Code block with syntax highlighting
    , HH.div
        [ HP.classes [ HH.ClassName "code-example__code" ] ]
        (syntaxHighlightedCode config.code)

    -- Explore button
    , HH.div
        [ HP.classes [ HH.ClassName "code-example__actions" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath (Explore config.snippetId)
            , HP.classes [ HH.ClassName "code-example__explore-btn" ]
            ]
            [ HH.text "Explore this code →" ]
        ]
    ]

-- | Simple version without title
renderCodeExampleSimple :: forall w i. String -> String -> HH.HTML w i
renderCodeExampleSimple code snippetId =
  renderCodeExample { code, snippetId, title: "" }
