-- | Generic element transformation based on bound data
-- |
-- | Provides a type-safe way to update attributes on DOM elements
-- | based on their D3-bound data. Used for animations and position updates.
-- |
-- | Example:
-- | ```purescript
-- | -- Update circle positions during animation
-- | transformCircles "#viz" \node ->
-- |   { cx: lerp 0.0 node.treeX progress
-- |   , cy: lerp 0.0 node.treeY progress
-- |   }
-- |
-- | -- Update line positions
-- | transformLines "#viz" \link ->
-- |   { x1: lerp 0.0 link.sourceX progress
-- |   , y1: lerp 0.0 link.sourceY progress
-- |   , x2: lerp 0.0 link.targetX progress
-- |   , y2: lerp 0.0 link.targetY progress
-- |   }
-- | ```
module PSD3v2.Transform
  ( transformCircles
  , transformLines
  , transformPaths
  , clearContainer
  , removeElement
  , CirclePosition
  , LinePosition
  ) where

import Prelude

import Effect (Effect)

-- | Position for circle elements
type CirclePosition =
  { cx :: Number
  , cy :: Number
  }

-- | Position for line elements
type LinePosition =
  { x1 :: Number
  , y1 :: Number
  , x2 :: Number
  , y2 :: Number
  }

-- | Transform circle elements by updating cx/cy based on bound data
-- |
-- | For each circle matching the selector, calls the transformer function
-- | with the element's bound data and updates the cx/cy attributes.
-- |
-- | Parameters:
-- | - `containerSelector`: CSS selector for the container (e.g., "#viz")
-- | - `transformer`: Function from bound data to CirclePosition
-- |
-- | The phantom type `d` ensures type safety - the transformer must accept
-- | the same data type that was bound to the elements.
foreign import transformCircles_
  :: forall d
   . String                    -- Container selector
  -> (d -> CirclePosition)     -- Transformer function
  -> Effect Unit

-- | Transform line elements by updating x1/y1/x2/y2 based on bound data
-- |
-- | Parameters:
-- | - `containerSelector`: CSS selector for the container (e.g., "#viz")
-- | - `transformer`: Function from bound data to LinePosition
foreign import transformLines_
  :: forall d
   . String                    -- Container selector
  -> (d -> LinePosition)       -- Transformer function
  -> Effect Unit

-- | Transform circle elements by updating cx/cy based on bound data
transformCircles
  :: forall d
   . String                    -- Container selector
  -> (d -> CirclePosition)     -- Transformer function
  -> Effect Unit
transformCircles = transformCircles_

-- | Transform line elements by updating x1/y1/x2/y2 based on bound data
transformLines
  :: forall d
   . String                    -- Container selector
  -> (d -> LinePosition)       -- Transformer function
  -> Effect Unit
transformLines = transformLines_

-- | Transform path elements by updating d attribute based on bound data
-- |
-- | Parameters:
-- | - `containerSelector`: CSS selector for the container (e.g., "#viz")
-- | - `transformer`: Function from bound data to path d string
foreign import transformPaths_
  :: forall d
   . String                    -- Container selector
  -> (d -> String)             -- Transformer function returning path d
  -> Effect Unit

-- | Transform path elements by updating d attribute based on bound data
transformPaths
  :: forall d
   . String                    -- Container selector
  -> (d -> String)             -- Transformer function returning path d
  -> Effect Unit
transformPaths = transformPaths_

-- | Clear all child elements from a container
foreign import clearContainer_ :: String -> Effect Unit

-- | Clear all child elements from a container
clearContainer :: String -> Effect Unit
clearContainer = clearContainer_

-- | Remove an element from the DOM entirely
foreign import removeElement_ :: String -> Effect Unit

-- | Remove an element from the DOM entirely
removeElement :: String -> Effect Unit
removeElement = removeElement_
