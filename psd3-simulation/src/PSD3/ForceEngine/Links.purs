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
-- |
-- | PERFORMANCE: Hot-path functions use FFI-backed O(1) Set/Map lookups
-- | instead of O(n) array scans to handle 1000+ node graphs at 60fps.
module PSD3.ForceEngine.Links
  ( -- * Swizzling (index → node reference)
    swizzleLinks
  , swizzleLinksByIndex
    -- * Filtering
  , filterLinksToSubset
    -- * Fast membership (FFI-backed)
  , IntSet
  , buildIntSet
  , intSetMember
  , IntMap
  , buildIntMap
  , intMapLookup
    -- * String sets (for ID-based membership)
  , StringSet
  , buildStringSet
  , stringSetMember
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

-- =============================================================================
-- FFI-backed Fast Data Structures
-- =============================================================================

-- | Opaque IntSet backed by JavaScript Set - O(1) membership
foreign import data IntSet :: Type

-- | Build an IntSet from an array of integers - O(n)
foreign import buildIntSet :: Array Int -> IntSet

-- | Check membership - O(1)
foreign import intSetMember :: Int -> IntSet -> Boolean

-- | Opaque IntMap backed by JavaScript Map - O(1) lookup
foreign import data IntMap :: Type -> Type

-- | Build an IntMap from an array of (key, value) pairs - O(n)
foreign import buildIntMap_ :: forall a. Array { key :: Int, value :: a } -> IntMap a

-- | Build an IntMap from nodes using an index accessor
buildIntMap :: forall node. (node -> Int) -> Array node -> IntMap node
buildIntMap getIndex nodes = buildIntMap_ (map (\n -> { key: getIndex n, value: n }) nodes)

-- | Lookup by key - O(1), returns null if not found
foreign import intMapLookup_ :: forall a. Int -> IntMap a -> a

-- | Safe lookup wrapper
intMapLookup :: forall a. Int -> IntMap a -> Maybe a
intMapLookup key m =
  let result = intMapLookup_ key m
  in if isNull result then Nothing else Just result

foreign import isNull :: forall a. a -> Boolean

-- | Opaque StringSet backed by JavaScript Set - O(1) membership
foreign import data StringSet :: Type

-- | Build a StringSet from an array of strings - O(n)
foreign import buildStringSet :: Array String -> StringSet

-- | Check membership - O(1)
foreign import stringSetMember :: String -> StringSet -> Boolean

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
-- | PERFORMANCE: Uses FFI-backed IntMap for O(1) lookups instead of O(n) find.
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
  -- Build O(1) lookup map once, then process all links
  let nodeMap = buildIntMap getIndex nodes
  in Array.mapMaybe (swizzle nodeMap) links
       # Array.mapWithIndex reindex
  where
  swizzle nodeMap link = do
    srcNode <- intMapLookup link.source nodeMap
    tgtNode <- intMapLookup link.target nodeMap
    pure { src: srcNode, tgt: tgtNode, link }

  reindex i { src, tgt, link } = transform src tgt i link

-- =============================================================================
-- Filtering: Select links for node subsets
-- =============================================================================

-- | Filter links to only those where both endpoints are in the node subset
-- |
-- | PERFORMANCE: Uses FFI-backed IntSet for O(1) membership instead of O(n) elem.
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
  -- Build O(1) membership set once, then filter all links
  let nodeIndices = buildIntSet (map getIndex nodes)
  in Array.filter (\l -> intSetMember l.source nodeIndices && intSetMember l.target nodeIndices) links
