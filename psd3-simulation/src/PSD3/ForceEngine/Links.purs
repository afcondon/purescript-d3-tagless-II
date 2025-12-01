-- | Link Operations for Force Simulations
-- |
-- | Functions for working with links in force-directed graphs:
-- | - Swizzling: converting index-based links to node-reference links
-- | - Filtering: selecting links for node subsets
-- |
-- | These operations are essential for:
-- | - Rendering (need node references to read x,y positions)
-- | - GUP patterns (filtering to visible nodes)
-- | - Dynamic graph updates
module PSD3.ForceEngine.Links
  ( -- * Swizzling (index → node reference)
    swizzleLinks
  , swizzleLinksByIndex
    -- * Filtering
  , filterLinksToSubset
  ) where

import Prelude

import Data.Array as Array
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

-- =============================================================================
-- Swizzling: Convert index-based links to node-reference links
-- =============================================================================

-- | Convert raw links (integer indices) to swizzled links (node references)
-- |
-- | ASSUMES: link.source and link.target are valid array indices into nodes.
-- | Use this when your nodes array is the full set (not filtered).
-- |
-- | The transform function allows you to build the output link record,
-- | copying extra fields from the raw link as needed.
-- |
-- | Example:
-- | ```purescript
-- | let swizzled = swizzleLinks nodes rawLinks \src tgt i link ->
-- |       { source: src, target: tgt, index: i, value: link.value }
-- | ```
swizzleLinks
  :: forall node rawLink swizzled
   . Array node
  -> Array { source :: Int, target :: Int | rawLink }
  -> (node -> node -> Int -> { source :: Int, target :: Int | rawLink } -> swizzled)
  -> Array swizzled
swizzleLinks nodes links transform =
  Array.mapWithIndex swizzle links
  where
  swizzle i link =
    let src = unsafeArrayIndex nodes link.source
        tgt = unsafeArrayIndex nodes link.target
    in transform src tgt i link

-- | Safe-ish array index (crashes with helpful message if out of bounds)
unsafeArrayIndex :: forall a. Array a -> Int -> a
unsafeArrayIndex arr i = case Array.index arr i of
  Just x -> x
  Nothing -> unsafeCrashWith ("swizzleLinks: Array index out of bounds: " <> show i)

-- | Swizzle links by looking up nodes by their index field
-- |
-- | USE THIS when working with a FILTERED node subset where array positions
-- | don't match node.index values. Looks up nodes by their `.index` field
-- | and silently drops links where either endpoint is not found.
-- |
-- | This is essential for GUP patterns where you show only a subset of nodes.
-- |
-- | Example:
-- | ```purescript
-- | let visibleNodes = filter isVisible allNodes
-- |     swizzled = swizzleLinksByIndex _.index visibleNodes links \src tgt i link ->
-- |       { source: src, target: tgt, index: i, value: link.value }
-- | ```
swizzleLinksByIndex
  :: forall node rawLink swizzled
   . (node -> Int)                           -- ^ Get original index from node
  -> Array node                              -- ^ Node subset to look up in
  -> Array { source :: Int, target :: Int | rawLink }  -- ^ Links with index references
  -> (node -> node -> Int -> { source :: Int, target :: Int | rawLink } -> swizzled)
  -> Array swizzled                          -- ^ Only links where both endpoints found
swizzleLinksByIndex getIndex nodes links transform =
  Array.mapMaybe swizzle links
    # Array.mapWithIndex reindex
  where
  swizzle link = do
    srcNode <- Array.find (\n -> getIndex n == link.source) nodes
    tgtNode <- Array.find (\n -> getIndex n == link.target) nodes
    pure { src: srcNode, tgt: tgtNode, link }

  reindex i { src, tgt, link } = transform src tgt i link

-- =============================================================================
-- Filtering: Select links for node subsets
-- =============================================================================

-- | Filter links to only those where both endpoints are in the node subset
-- |
-- | Use this before swizzling when you want to show links only between
-- | visible/selected nodes. Pairs naturally with `swizzleLinksByIndex`.
-- |
-- | Example:
-- | ```purescript
-- | let visibleNodes = filter isVisible allNodes
-- |     visibleLinks = filterLinksToSubset _.index visibleNodes allLinks
-- |     swizzled = swizzleLinksByIndex _.index visibleNodes visibleLinks transform
-- | ```
filterLinksToSubset
  :: forall node linkData
   . (node -> Int)                                      -- ^ Get original index from node
  -> Array node                                         -- ^ Node subset
  -> Array { source :: Int, target :: Int | linkData }  -- ^ All links
  -> Array { source :: Int, target :: Int | linkData }  -- ^ Links between subset nodes only
filterLinksToSubset getIndex nodes links =
  let nodeIndices = map getIndex nodes
  in filter (\l -> Array.elem l.source nodeIndices && Array.elem l.target nodeIndices) links
