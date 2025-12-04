module PSD3v2.Tooltip
  ( -- * Core API
    showTooltip
  , hideTooltip
  , setTooltipClass
  , onTooltip
  , onTooltipHide
    -- * Configuration
  , TooltipConfig
  , configureTooltip
  , defaultConfig
    -- * Theme Presets
  , floatingPanelTheme
  , darkTheme
  , lightTheme
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import PSD3v2.Behavior.Types (Behavior(..), MouseEventInfo)

-- | Show tooltip with HTML content at screen position
-- |
-- | Automatically handles viewport bounds to keep tooltip on screen.
-- |
-- | Example:
-- | ```purescript
-- | showTooltip "<b>Node:</b> Main" 100.0 200.0
-- | ```
foreign import showTooltip_ :: String -> Number -> Number -> Effect Unit

-- | Hide the tooltip
foreign import hideTooltip_ :: Effect Unit

-- | Set custom CSS class for tooltip styling
-- |
-- | Default class is "psd3-tooltip" with built-in styles.
foreign import setTooltipClass_ :: String -> Effect Unit

-- | Show tooltip with content at position
showTooltip :: String -> Number -> Number -> Effect Unit
showTooltip = showTooltip_

-- | Hide the tooltip
hideTooltip :: Effect Unit
hideTooltip = hideTooltip_

-- | Set custom CSS class for tooltip styling
setTooltipClass :: String -> Effect Unit
setTooltipClass = setTooltipClass_

-- | Create a tooltip behavior from a content formatter
-- |
-- | This is the simplest way to add tooltips - just provide a function
-- | that converts your datum to HTML string content.
-- |
-- | Example:
-- | ```purescript
-- | _ <- on (onTooltip \d -> d.name <> ": " <> show d.value) circles
-- | ```
-- |
-- | The tooltip will automatically:
-- | - Show on mouse enter at the cursor position
-- | - Hide on mouse leave
-- | - Stay within viewport bounds
onTooltip :: forall d. (d -> String) -> Behavior d
onTooltip formatContent = MouseEnterWithInfo handler
  where
    handler :: MouseEventInfo d -> Effect Unit
    handler info = showTooltip_ (formatContent info.datum) info.pageX info.pageY

-- | Hide tooltip on mouse leave
-- |
-- | Pair with onTooltip for complete tooltip behavior:
-- |
-- | ```purescript
-- | _ <- on (onTooltip \d -> d.name) element
-- | _ <- on onTooltipHide element
-- | ```
onTooltipHide :: forall d. Behavior d
onTooltipHide = MouseLeave \_ -> hideTooltip_

-- =============================================================================
-- Configuration
-- =============================================================================

-- | Tooltip configuration for programmatic styling
-- |
-- | All fields are optional - only specified fields will override defaults.
-- | Prefer using CSS via `.psd3-tooltip` class for base styling, and use
-- | this for dynamic themes or application-specific overrides.
type TooltipConfig =
  { background :: Maybe String
  , color :: Maybe String
  , padding :: Maybe String
  , borderRadius :: Maybe String
  , fontSize :: Maybe String
  , fontFamily :: Maybe String
  , border :: Maybe String
  , boxShadow :: Maybe String
  , maxWidth :: Maybe String
  , lineHeight :: Maybe String
  , backdropFilter :: Maybe String
  }

-- | Configure tooltip appearance programmatically
-- |
-- | Example:
-- | ```purescript
-- | configureTooltip floatingPanelTheme
-- | ```
foreign import configureTooltip_ :: TooltipConfig -> Effect Unit

configureTooltip :: TooltipConfig -> Effect Unit
configureTooltip = configureTooltip_

-- | Default configuration (minimal - relies on CSS)
defaultConfig :: TooltipConfig
defaultConfig =
  { background: Nothing
  , color: Nothing
  , padding: Nothing
  , borderRadius: Nothing
  , fontSize: Nothing
  , fontFamily: Nothing
  , border: Nothing
  , boxShadow: Nothing
  , maxWidth: Nothing
  , lineHeight: Nothing
  , backdropFilter: Nothing
  }

-- =============================================================================
-- Theme Presets
-- =============================================================================

-- | Floating panel theme - matches blueprint-style floating panels
-- |
-- | Features:
-- | - Tan/beige translucent background
-- | - Backdrop blur (glassmorphism)
-- | - Monospace font
-- | - Subtle borders and shadows
floatingPanelTheme :: TooltipConfig
floatingPanelTheme =
  { background: Just "rgba(212, 201, 168, 0.95)"
  , color: Just "#2c2c2c"
  , padding: Just "12px 16px"
  , borderRadius: Just "4px"
  , fontSize: Just "12px"
  , fontFamily: Just "'Courier New', monospace"
  , border: Just "1px solid rgba(0, 0, 0, 0.1)"
  , boxShadow: Just "0 2px 8px rgba(0, 0, 0, 0.12)"
  , maxWidth: Just "300px"
  , lineHeight: Just "1.6"
  , backdropFilter: Just "blur(12px)"
  }

-- | Dark theme - traditional dark tooltip
darkTheme :: TooltipConfig
darkTheme =
  { background: Just "rgba(20, 20, 30, 0.95)"
  , color: Just "#f0f0f0"
  , padding: Just "8px 12px"
  , borderRadius: Just "6px"
  , fontSize: Just "13px"
  , fontFamily: Nothing
  , border: Nothing
  , boxShadow: Just "0 4px 12px rgba(0, 0, 0, 0.3)"
  , maxWidth: Just "300px"
  , lineHeight: Nothing
  , backdropFilter: Nothing
  }

-- | Light theme - clean, minimal light tooltip
lightTheme :: TooltipConfig
lightTheme =
  { background: Just "rgba(255, 255, 255, 0.95)"
  , color: Just "#333"
  , padding: Just "8px 12px"
  , borderRadius: Just "4px"
  , fontSize: Just "13px"
  , fontFamily: Nothing
  , border: Just "1px solid rgba(0, 0, 0, 0.1)"
  , boxShadow: Just "0 2px 8px rgba(0, 0, 0, 0.1)"
  , maxWidth: Just "300px"
  , lineHeight: Nothing
  , backdropFilter: Nothing
  }
