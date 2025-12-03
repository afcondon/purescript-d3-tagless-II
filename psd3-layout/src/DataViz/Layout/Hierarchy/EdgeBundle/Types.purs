-- | DataViz.Layout.Hierarchy.EdgeBundle.Types
-- |
-- | Types for hierarchical edge bundling visualization.
-- | Based on Danny Holten's algorithm and D3's implementation.
-- |
-- | Edge bundling shows dependencies between nodes in a hierarchy by:
-- | 1. Arranging nodes in a radial cluster layout
-- | 2. Drawing curved edges that bundle together based on shared ancestry
-- | 3. The tighter the bundle (higher beta), the more edges follow tree structure
module DataViz.Layout.Hierarchy.EdgeBundle.Types
  ( -- * Input types
    ImportedNode
  , parseImportedNode
  -- * Hierarchy types
  , BundleTree
  , BundleNode(..)
  , getBundleData
  , getBundleChildren
  , getBundleParent
  -- * Link types
  , BundleLink
  , OutgoingLink
  , IncomingLink
  -- * Layout types
  , RadialNode
  , EdgeBundleConfig
  , defaultEdgeBundleConfig
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Index ((!))
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Data.Number as Number

-- | Input format: flat array of nodes with dot-notation names and imports
-- | Example: { "name": "flare.animate.Easing", "size": 17010, "imports": ["flare.util.Arrays"] }
type ImportedNode =
  { name :: String -- Fully qualified name (e.g. "flare.animate.Easing")
  , size :: Number -- Optional size value for visualization
  , imports :: Array String -- Array of fully qualified names this node imports
  }

-- | Parse a Foreign value into an ImportedNode
parseImportedNode :: Foreign -> Maybe ImportedNode
parseImportedNode f = case runExcept parsed of
  Right node -> Just node
  Left _ -> Nothing
  where
  parsed = do
    nameF <- f ! "name"
    name <- Foreign.readString nameF
    sizeF <- f ! "size"
    size <- Foreign.readNumber sizeF
    importsF <- f ! "imports"
    importsArr <- Foreign.readArray importsF
    imports <- traverse Foreign.readString importsArr
    pure { name, size, imports }

-- | A node in the bundle hierarchy tree
-- | Contains the data plus computed layout information
data BundleNode a = BundleNode
  { data_ :: a -- User data (name, size, etc.)
  , depth :: Int -- Distance from root (root = 0)
  , height :: Int -- Distance to deepest descendant (leaf = 0)
  , parent :: Maybe (BundleNode a) -- Parent node reference
  , children :: Array (BundleNode a) -- Child nodes
  , fullName :: String -- Full dot-notation path (e.g. "flare.animate.Easing")
  -- Layout coordinates (filled in by radial cluster)
  , x :: Number -- Angle in radians (0 to 2π)
  , y :: Number -- Radius from center
  -- Links (filled in by bilink)
  , outgoing :: Array OutgoingLink -- Edges from this node to imports
  , incoming :: Array IncomingLink -- Edges from nodes that import this
  }

-- | Type alias for the full tree
type BundleTree a = BundleNode a

-- | Get the user data from a bundle node
getBundleData :: forall a. BundleNode a -> a
getBundleData (BundleNode n) = n.data_

-- | Get children of a bundle node
getBundleChildren :: forall a. BundleNode a -> Array (BundleNode a)
getBundleChildren (BundleNode n) = n.children

-- | Get parent of a bundle node
getBundleParent :: forall a. BundleNode a -> Maybe (BundleNode a)
getBundleParent (BundleNode n) = n.parent

-- | An outgoing link (from source to target)
-- | The link itself is a pair: [source, target]
type OutgoingLink =
  { sourceFullName :: String
  , targetFullName :: String
  }

-- | An incoming link (from source to this node as target)
type IncomingLink =
  { sourceFullName :: String
  , targetFullName :: String
  }

-- | A bundle link after path computation
-- | Contains the path through the tree from source to target
type BundleLink =
  { source :: String -- Source node full name
  , target :: String -- Target node full name
  , path :: Array RadialNode -- Path through tree (for curve drawing)
  }

-- | Node position after radial layout
-- | Used for rendering and path computation
type RadialNode =
  { fullName :: String
  , x :: Number -- Angle in radians
  , y :: Number -- Radius from center
  }

-- | Configuration for edge bundle layout
type EdgeBundleConfig =
  { radius :: Number -- Radius of the radial layout
  , innerRadius :: Number -- Where leaves are positioned
  , beta :: Number -- Bundle tension (0 = straight, 1 = tight bundles)
  , nodeSpacing :: Number -- Angular spacing between nodes
  }

-- | Default configuration
defaultEdgeBundleConfig :: EdgeBundleConfig
defaultEdgeBundleConfig =
  { radius: 400.0
  , innerRadius: 380.0 -- Leaves positioned near outer edge
  , beta: 0.85 -- D3 default bundle tension
  , nodeSpacing: 1.0 -- Default node separation
  }
