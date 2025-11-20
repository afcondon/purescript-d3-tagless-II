module D3.Viz.Spago.Tooltip where

import Prelude

import Effect (Effect)
import Foreign (Foreign)
import D3.Viz.Spago.Model (SpagoSimNode)
import PSD3v2.Tooltip (showTooltip, hideTooltip)

-- | Format metrics content for a node
foreign import formatMetricsContent_ :: SpagoSimNode -> Foreign -> String

-- | Get the metrics data (may be null if not loaded yet)
foreign import getMetricsData_ :: Effect Foreign

-- | Show tooltip for a node with git metrics
showNodeTooltip :: SpagoSimNode -> Number -> Number -> Effect Unit
showNodeTooltip node x y = do
  metricsData <- getMetricsData_
  let content = formatMetricsContent_ node metricsData
  showTooltip content x y

-- | Hide the tooltip
hideNodeTooltip :: Effect Unit
hideNodeTooltip = hideTooltip
