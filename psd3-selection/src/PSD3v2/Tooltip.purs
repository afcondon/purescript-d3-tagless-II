module PSD3v2.Tooltip
  ( showTooltip
  , hideTooltip
  , setTooltipClass
  , onTooltip
  , onTooltipHide
  ) where

import Prelude

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
