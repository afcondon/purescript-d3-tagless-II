module PSD3.Internal.Sankey.Types where

import Prelude

-- Opaque type for D3 Sankey layout state
-- This wraps the D3 sankey generator function
foreign import data SankeyLayoutState_ :: Type

-- Configuration for Sankey layout
type SankeyConfig = {
    alignment :: String  -- "justify", "left", "right", or "center"
  , linkColorMode :: String  -- "source", "target", "source-target", or "static"
  , nodeWidth :: Number
  , nodePadding :: Number
}

-- Result of applying Sankey layout to data
-- Contains nodes and links with computed positions
type SankeyLayoutResult = {
    nodes :: Array SankeyNode_
  , links :: Array SankeyLink_
}

-- Opaque types for data that has been processed by the Sankey layout
-- These will have additional properties computed by D3
foreign import data SankeyNode_ :: Type
foreign import data SankeyLink_ :: Type

-- Initial state for Sankey layout
-- This will be created by the FFI layer
foreign import initialSankeyLayoutState_ :: SankeyLayoutState_
