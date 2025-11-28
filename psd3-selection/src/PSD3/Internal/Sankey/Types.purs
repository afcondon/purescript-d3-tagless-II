module PSD3.Internal.Sankey.Types where

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

-- Property accessors for SankeyNode_
foreign import nodeX0_ :: SankeyNode_ -> Number
foreign import nodeY0_ :: SankeyNode_ -> Number
foreign import nodeX1_ :: SankeyNode_ -> Number
foreign import nodeY1_ :: SankeyNode_ -> Number
foreign import nodeValue_ :: SankeyNode_ -> Number
foreign import nodeDepth_ :: SankeyNode_ -> Int
foreign import nodeName_ :: SankeyNode_ -> String

-- Property accessors for SankeyLink_
foreign import linkValue_ :: SankeyLink_ -> Number
foreign import linkWidth_ :: SankeyLink_ -> Number
foreign import linkSourceIndex_ :: SankeyLink_ -> Int
foreign import linkTargetIndex_ :: SankeyLink_ -> Int
