module D3.Viz.Spago.Tooltip where

import Prelude

import Effect (Effect)
import Foreign (Foreign)
import D3.Viz.Spago.Model (SpagoSimNode)

-- | Show tooltip with HTML content at screen position
foreign import showTooltip_ :: String -> Number -> Number -> Effect Unit

-- | Hide the tooltip
foreign import hideTooltip_ :: Effect Unit

-- | Format metrics content for a node
foreign import formatMetricsContent_ :: SpagoSimNode -> Foreign -> String

-- | Get the metrics data (may be null if not loaded yet)
foreign import getMetricsData_ :: Effect Foreign

-- | Show tooltip for a node with git metrics
showNodeTooltip :: SpagoSimNode -> Number -> Number -> Effect Unit
showNodeTooltip node x y = do
  metricsData <- getMetricsData_
  let content = formatMetricsContent_ node metricsData
  showTooltip_ content x y

-- | Hide the tooltip
hideNodeTooltip :: Effect Unit
hideNodeTooltip = hideTooltip_
