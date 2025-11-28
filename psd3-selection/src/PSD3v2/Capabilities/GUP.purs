-- | PSD3v2.Capabilities.GUP
-- |
-- | General Update Pattern capability for declarative enter/update/exit handling.
-- |
-- | This module provides a clean, type-safe way to handle D3's General Update Pattern
-- | without the type gymnastics of SceneNestedJoin.
-- |
-- | ## Design Philosophy
-- |
-- | The GUP is orthogonal to Tree structure building. You:
-- | 1. Build static structure with Tree API (svg, groups, etc.)
-- | 2. Use joinWithGUP to bind data with enter/update/exit behaviors
-- | 3. Let the simulation tick function handle position updates
-- |
-- | ## Usage Pattern
-- |
-- | ```purescript
-- | drawGraph = do
-- |   -- 1. Build structure
-- |   container <- select "#viz"
-- |   let structure = svg [...] [ g [id_ "nodes"], g [id_ "links"] ]
-- |   renderTree container structure
-- |
-- |   -- 2. Join data with GUP
-- |   nodesGroup <- select "#nodes"
-- |   nodeCircles <- joinWithGUP nodesGroup nodes
-- |     { element: "circle"
-- |     , template: \n -> [cx n.x, cy n.y, radius 5, fill n.color]
-- |     , key: _.id
-- |     , enter: Just { initial: [radius 0], transition: Just (600ms) }
-- |     , exit: Just { final: [radius 0], transition: Just (500ms) }
-- |     }
-- |
-- |   -- 3. Add tick function for simulation
-- |   addTickFunction "nodes" $ Step nodeCircles [cx _.x, cy _.y]
-- |   start
-- | ```
module PSD3v2.Capabilities.GUP where

import Prelude

import Data.Maybe (Maybe)
import PSD3v2.Attribute.Types (Attribute)
import PSD3v2.Transition.Types (TransitionConfig)

-- | Specification for entering elements.
-- |
-- | Entering elements are new data items that need DOM elements created.
-- | They can optionally transition from an initial state to the template state.
type EnterSpec =
  { initial :: Array (Attribute Unit)     -- Initial attributes (static only - no datum access!)
  , transition :: Maybe TransitionConfig   -- Optional transition to template state
  }

-- | Specification for exiting elements.
-- |
-- | Exiting elements are old DOM elements being removed.
-- | They can optionally transition to a final state before removal.
type ExitSpec =
  { final :: Array (Attribute Unit)       -- Final attributes (static only - no datum access!)
  , transition :: Maybe TransitionConfig   -- Optional transition before removal
  }

-- | Complete specification for a GUP join.
-- |
-- | This describes:
-- | - What data to join
-- | - How to key the data (for object constancy)
-- | - What element type to create
-- | - How elements should look (template)
-- | - How elements should enter/exit (optional)
type JoinSpec datum =
  { element :: String                           -- Element type: "circle", "line", "rect", etc.
  , template :: datum -> Array (Attribute datum) -- How each element should look
  , key :: datum -> String                      -- Key function for object constancy
  , enter :: Maybe EnterSpec                    -- Optional enter behavior
  , exit :: Maybe ExitSpec                      -- Optional exit behavior
  }

-- | Perform a data join with full General Update Pattern support.
-- |
-- | This is the main GUP function. It:
-- | 1. Joins data to DOM elements using the key function
-- | 2. Handles entering elements (create + optional transition)
-- | 3. Handles updating elements (apply template)
-- | 4. Handles exiting elements (optional transition + remove)
-- |
-- | Returns a selection bound to the data for use in tick functions.
-- |
-- | ## Type Safety
-- |
-- | Unlike SceneNestedJoin, this has proper types:
-- | - `datum` is the actual data type (e.g., LesMisSimNode)
-- | - No wrapper types needed (no SceneData, no KeyedNode)
-- | - Template functions receive the actual datum type
-- | - Enter/exit specs use static attributes only (Attribute Unit)
-- |
-- | ## Example
-- |
-- | ```purescript
-- | nodeCircles <- joinWithGUP nodesGroup nodes
-- |   { element: "circle"
-- |   , template: \n ->
-- |       [ cx n.x, cy n.y
-- |       , radius 5.0
-- |       , fill (colorByGroup n.group)
-- |       ]
-- |   , key: _.id
-- |   , enter: Just
-- |       { initial: [cx 0, cy 0, radius 0]
-- |       , transition: Just { duration: Milliseconds 600.0, ... }
-- |       }
-- |   , exit: Just
-- |       { final: [radius 0]
-- |       , transition: Just { duration: Milliseconds 500.0, ... }
-- |       }
-- |   }
-- | ```
-- joinWithGUP
--   :: forall sel m parent parentDatum datum
--    . MonadEffect m
--   => sel SEmpty parent parentDatum
--   -> Array datum
--   -> JoinSpec datum
--   -> m (sel SBoundOwns Element datum)
-- joinWithGUP = undefined  -- TODO: Implement using existing SelectionM operations
