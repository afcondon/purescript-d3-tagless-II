-- | Generic element classification based on bound data
-- |
-- | Provides a type-safe way to apply CSS classes to DOM elements
-- | based on predicates over their D3-bound data.
-- |
-- | Example:
-- | ```purescript
-- | -- Highlight connected nodes on hover
-- | let targetSet = Set.fromFoldable node.targets
-- | let sourceSet = Set.fromFoldable node.sources
-- |
-- | classifyElements "#nodes" "circle" \n ->
-- |   if n.id == hoveredId then "highlighted-source"
-- |   else if Set.member n.id targetSet then "highlighted-upstream"
-- |   else if Set.member n.id sourceSet then "highlighted-downstream"
-- |   else "dimmed"
-- |
-- | -- Clear on mouse leave
-- | clearClasses "#nodes" "circle"
-- |   ["highlighted-source", "highlighted-upstream", "highlighted-downstream", "dimmed"]
-- | ```
module PSD3v2.Classify
  ( classifyElements
  , clearClasses
  ) where

import Prelude

import Effect (Effect)

-- | Classify elements by applying CSS classes based on bound data
-- |
-- | For each element matching the selector, calls the classifier function
-- | with the element's bound data. If the classifier returns a non-empty
-- | string, that class is added to the element.
-- |
-- | Parameters:
-- | - `containerSelector`: CSS selector for the container (e.g., "#nodes")
-- | - `elementType`: Element type to classify (e.g., "circle")
-- | - `classifier`: Function from bound data to class name (empty string = skip)
-- |
-- | The phantom type `d` ensures type safety - the classifier must accept
-- | the same data type that was bound to the elements.
foreign import classifyElements_
  :: forall d
   . String        -- Container selector
  -> String        -- Element type
  -> (d -> String) -- Classifier function
  -> Effect Unit

-- | Clear specified CSS classes from all matching elements
-- |
-- | Parameters:
-- | - `containerSelector`: CSS selector for the container (e.g., "#nodes")
-- | - `elementType`: Element type to clear classes from (e.g., "circle")
-- | - `classNames`: Array of class names to remove
foreign import clearClasses_
  :: String        -- Container selector
  -> String        -- Element type
  -> Array String  -- Classes to remove
  -> Effect Unit

-- | Classify elements by applying CSS classes based on bound data
classifyElements
  :: forall d
   . String        -- Container selector
  -> String        -- Element type
  -> (d -> String) -- Classifier function
  -> Effect Unit
classifyElements = classifyElements_

-- | Clear specified CSS classes from all matching elements
clearClasses
  :: String        -- Container selector
  -> String        -- Element type
  -> Array String  -- Classes to remove
  -> Effect Unit
clearClasses = clearClasses_
