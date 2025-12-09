module Component.SankeyDebug.FFI where

-- | FFI bindings for d3-sankey to enable direct comparison with our PureScript implementation

-- | D3's computed node format (subset we need)
type D3Node =
  { name :: String
  , index :: Int
  , depth :: Int
  , height :: Int
  , layer :: Int
  , value :: Number
  , x0 :: Number
  , x1 :: Number
  , y0 :: Number
  , y1 :: Number
  }

-- | D3's computed link format
type D3Link =
  { sourceIndex :: Int
  , targetIndex :: Int
  , value :: Number
  , width :: Number
  , y0 :: Number
  , y1 :: Number
  , index :: Int
  }

-- | Input node format for d3-sankey
type D3NodeInput = { name :: String }

-- | Input link format for d3-sankey
type D3LinkInput = { source :: String, target :: String, value :: Number }

-- | Result of d3-sankey computation
type D3SankeyResult =
  { nodes :: Array D3Node
  , links :: Array D3Link
  }

-- | A step in the layout process (for visualization)
type D3SankeyStep =
  { iteration :: Int
  , label :: String
  , nodes :: Array D3Node
  , links :: Array D3Link
  }

-- | Compute sankey layout using d3-sankey JavaScript library
foreign import computeD3SankeyLayout_
  :: Array D3NodeInput
  -> Array D3LinkInput
  -> Number  -- width
  -> Number  -- height
  -> Number  -- nodeWidth
  -> Number  -- nodePadding
  -> Int     -- iterations
  -> D3SankeyResult

-- | Compute sankey layout with intermediate steps captured
foreign import computeD3SankeyWithSteps_
  :: Array D3NodeInput
  -> Array D3LinkInput
  -> Number  -- width
  -> Number  -- height
  -> Number  -- nodeWidth
  -> Number  -- nodePadding
  -> Int     -- maxIterations
  -> Array D3SankeyStep

-- | Generate SVG path for a sankey link
foreign import sankeyLinkPath_
  :: D3Link
  -> Array D3Node
  -> String

-- | Convenience wrapper
computeD3SankeyLayout
  :: Array D3NodeInput
  -> Array D3LinkInput
  -> { width :: Number, height :: Number, nodeWidth :: Number, nodePadding :: Number, iterations :: Int }
  -> D3SankeyResult
computeD3SankeyLayout nodes links config =
  computeD3SankeyLayout_ nodes links config.width config.height config.nodeWidth config.nodePadding config.iterations

computeD3SankeyWithSteps
  :: Array D3NodeInput
  -> Array D3LinkInput
  -> { width :: Number, height :: Number, nodeWidth :: Number, nodePadding :: Number, maxIterations :: Int }
  -> Array D3SankeyStep
computeD3SankeyWithSteps nodes links config =
  computeD3SankeyWithSteps_ nodes links config.width config.height config.nodeWidth config.nodePadding config.maxIterations
