-- | Shared Flare dataset for hierarchy visualizations
-- | Uses FFI to access blessed JSON data without parsing overhead
module D3.Viz.FlareData where

import Prelude
import Data.Maybe (Maybe)
import PSD3.Data.Tree (TreeJson_)

-- Use TreeJson_ from the library as our blessed JSON type
type HierData = TreeJson_

-- FFI functions to access blessed JSON data safely
foreign import getName :: HierData -> String
foreign import getValue :: HierData -> Number
foreign import getChildren :: HierData -> Maybe (Array HierData)
foreign import hasChildren :: HierData -> Boolean
