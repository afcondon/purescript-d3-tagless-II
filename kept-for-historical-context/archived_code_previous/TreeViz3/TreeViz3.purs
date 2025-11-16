-- | EXPERIMENTAL: Type prototype for tree visualization
-- | Tests that row polymorphism works with D3 phantom types
module D3.Viz.TreeViz3 where

import Prelude

import Data.Tree (Tree(..))
import Data.List (List(..), fromFoldable)
import Data.Array as Array
import PSD3.Internal.Attributes.Sugar (classed, fill, strokeColor, strokeWidth, viewBox, cx, cy, radius)
import PSD3.Internal.Types (D3Selection_, Element(..), Selector, Datum_, Index_)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, simpleJoin)
import PSD3.Layout.Hierarchy.Tree3 (tree, defaultTreeConfig)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)

-- | User's data type (no layout fields)
type NodeData = { name :: String, value :: Int }

-- | After layout, data type has layout fields added
type PositionedNodeData = { name :: String, value :: Int, x :: Number, y :: Number, depth :: Int, height :: Int }

-- | Sample tree
sampleTree :: Tree NodeData
sampleTree = Node
  { name: "root", value: 100 }
  (fromFoldable
    [ Node { name: "child1", value: 50 }
        (fromFoldable
          [ Node { name: "grandchild1", value: 25 } Nil
          , Node { name: "grandchild2", value: 25 } Nil
          ])
    , Node { name: "child2", value: 50 } Nil
    ])

-- | Flatten tree to array
flatten :: forall a. Tree a -> Array a
flatten tree = Array.fromFoldable tree

-- | Draw the tree
-- | This tests that:
-- | 1. Layout adds fields to the record
-- | 2. Phantom types carry through to D3Selection_
-- | 3. We can extract fields in attribute functions without unsafeCoerce
draw :: forall m.
  Bind m =>
  MonadEffect m =>
  SelectionM D3Selection_ m =>
  Selector (D3Selection_ Unit) -> m Unit
draw selector = do
  liftEffect $ log "TreeViz3: Testing row polymorphism with phantom types"

  -- Apply layout (adds x, y, depth, height to the record)
  let positioned :: Tree PositionedNodeData
      positioned = tree defaultTreeConfig sampleTree

  -- Flatten to array
  let nodes :: Array PositionedNodeData
      nodes = flatten positioned

  liftEffect $ log $ "Positioned " <> show (Array.length nodes) <> " nodes"

  -- Create SVG
  root' <- attach selector :: m (D3Selection_ Unit)
  svg <- appendTo root' Svg
    [ viewBox 0.0 0.0 800.0 600.0
    , classed "tree3-test"
    ]

  -- Create group
  nodesGroup <- appendTo svg Group [ classed "nodes" ]

  -- KEY TEST: Can we use the positioned data with D3 selections?
  -- Cast to Datum_ for D3
  let nodesData :: Array Datum_
      nodesData = unsafeCoerce nodes

  -- Create selection with key function
  -- One unsafeCoerce to bless Datum_ as our known type
  let keyFn d =
        let node = unsafeCoerce d :: PositionedNodeData
        in unsafeCoerce node.name

  nodeSelection <- simpleJoin nodesGroup Circle nodesData keyFn

  -- KEY TEST: Attribute functions with clean type annotations
  -- One unsafeCoerce per function, then clean field access
  let cxFn :: Datum_ -> Number
      cxFn d = (unsafeCoerce d :: PositionedNodeData).x

  let cyFn :: Datum_ -> Number
      cyFn d = (unsafeCoerce d :: PositionedNodeData).y

  let radiusFn :: Datum_ -> Number
      radiusFn d = if (unsafeCoerce d :: PositionedNodeData).depth == 0 then 10.0 else 5.0

  _ <- appendTo nodeSelection Circle
    [ cx cxFn
    , cy cyFn
    , radius radiusFn
    , fill "#999"
    , strokeColor "#555"
    , strokeWidth 1.5
    ]

  liftEffect $ log "TreeViz3: Type test successful!"

  pure unit
