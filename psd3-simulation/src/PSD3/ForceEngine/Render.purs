-- | Force Simulation Rendering
-- |
-- | FFI-optimized helpers for rendering force-directed graphs.
-- |
-- | IMPORTANT PATTERN: For simulations, bind data ONCE then update positions via FFI.
-- | Do NOT use Tree API or data joins in tick callbacks - they are O(n) per tick.
-- |
-- | Correct usage:
-- | ```purescript
-- | -- Define group IDs (type-safe, no typos)
-- | nodesGroup = GroupId "#my-nodes"
-- | linksGroup = GroupId "#my-links"
-- |
-- | -- At initialization (once):
-- | nodeSel <- appendData Circle nodes [cx _.x, cy _.y, ...]
-- | linkSel <- appendData Line swizzledLinks [x1 (\l -> l.source.x), ...]
-- |
-- | -- In tick callback (every frame):
-- | updateCirclePositions nodesGroup
-- | updateLinkPositions linksGroup
-- | ```
-- |
-- | The FFI updates use D3's `.attr()` method directly on selections,
-- | which is O(n) DOM updates with no data join overhead.
module PSD3.ForceEngine.Render
  ( -- * Type-safe Group IDs
    GroupId(..)
    -- * Position Updates (FFI-optimized)
  , updateCirclePositions
  , updateLinkPositions
  , updateGroupPositions
    -- * Custom Position Updates
  , updatePositions
  ) where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)

-- =============================================================================
-- Type-safe Group IDs
-- =============================================================================

-- | A type-safe wrapper for DOM selector strings.
-- |
-- | Use this to identify groups of elements for position updates.
-- | The newtype provides compile-time checking that you're using
-- | group IDs consistently, without any runtime overhead.
-- |
-- | Example:
-- | ```purescript
-- | -- Define once, use everywhere
-- | packageNodes :: GroupId
-- | packageNodes = GroupId "#package-nodes"
-- |
-- | moduleNodes :: GroupId
-- | moduleNodes = GroupId "#module-nodes"
-- | ```
newtype GroupId = GroupId String

derive instance Newtype GroupId _
derive newtype instance Eq GroupId
derive newtype instance Ord GroupId
derive newtype instance Show GroupId

-- =============================================================================
-- FFI Imports (internal - take raw strings)
-- =============================================================================

foreign import updateCirclePositions_ :: String -> Effect Unit
foreign import updateLinkPositions_ :: String -> Effect Unit
foreign import updateGroupPositions_ :: String -> Effect Unit
foreign import updatePositions_ :: forall a. String -> (a -> Effect Unit) -> Effect Unit

-- =============================================================================
-- Public API (type-safe GroupId)
-- =============================================================================

-- | Update circle positions (cx, cy) from bound data's x, y fields.
-- |
-- | Assumes data bound to circles has `x :: Number` and `y :: Number` fields.
-- | Uses D3's `selection.attr()` which reads from `__data__` on each element.
-- |
-- | Example:
-- | ```purescript
-- | nodesGroup :: GroupId
-- | nodesGroup = GroupId "#nodes"
-- |
-- | updateCirclePositions nodesGroup
-- | ```
updateCirclePositions :: GroupId -> Effect Unit
updateCirclePositions gid = updateCirclePositions_ (unwrap gid)

-- | Update line positions (x1, y1, x2, y2) from bound data's source/target nodes.
-- |
-- | Assumes data bound to lines has:
-- | - `source :: { x :: Number, y :: Number, ... }`
-- | - `target :: { x :: Number, y :: Number, ... }`
-- |
-- | This is the "swizzled link" format where source/target are node objects,
-- | not integer indices. Use `Sim.getSwizzledLinks` after adding a Link force.
-- |
-- | Example:
-- | ```purescript
-- | linksGroup :: GroupId
-- | linksGroup = GroupId "#links"
-- |
-- | updateLinkPositions linksGroup
-- | ```
updateLinkPositions :: GroupId -> Effect Unit
updateLinkPositions gid = updateLinkPositions_ (unwrap gid)

-- | Update group positions (transform) from bound data's x, y fields.
-- |
-- | Selects `g.module-pack` elements and updates their transform attribute.
-- | Assumes data bound to groups has `x :: Number` and `y :: Number` fields.
-- |
-- | Example:
-- | ```purescript
-- | nodesGroup :: GroupId
-- | nodesGroup = GroupId "#nodes"
-- |
-- | updateGroupPositions nodesGroup
-- | ```
updateGroupPositions :: GroupId -> Effect Unit
updateGroupPositions gid = updateGroupPositions_ (unwrap gid)

-- | Generic position update with custom attribute setter.
-- |
-- | The setter receives the D3 selection and should call `.attr()` methods.
-- |
-- | Example (custom node representation):
-- | ```purescript
-- | rectsGroup :: GroupId
-- | rectsGroup = GroupId "#node-rects"
-- |
-- | updatePositions rectsGroup \sel -> do
-- |   sel # setAttr "x" (_.x - 10.0)  -- center rect
-- |   sel # setAttr "y" (_.y - 10.0)
-- | ```
updatePositions :: forall a. GroupId -> (a -> Effect Unit) -> Effect Unit
updatePositions gid = updatePositions_ (unwrap gid)
