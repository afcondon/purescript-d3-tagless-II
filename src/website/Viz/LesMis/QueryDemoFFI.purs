module D3.Viz.LesMis.QueryDemoFFI where

import Prelude
import Effect (Effect)

-- | Create interactive group toggle buttons
-- | Takes: array of groups, color function, toggle callback
foreign import createGroupButtons
  :: Array Int                             -- groups
  -> (Int -> String)                       -- color function
  -> (Int -> Boolean -> Effect Unit)       -- toggle callback (group -> isLarge -> Effect Unit)
  -> Effect Unit
