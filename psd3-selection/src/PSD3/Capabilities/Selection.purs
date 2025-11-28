-- | PSD3.Capabilities.Selection - Core D3 selection operations
-- |
-- | This module defines the `SelectionM` type class, which provides the fundamental
-- | operations for creating and manipulating D3 selections. It abstracts over D3's
-- | selection API using a finally tagless encoding, allowing multiple interpreters
-- | (D3, String, MetaTree) to implement the same interface.
-- |
-- | ## Core Concepts
-- |
-- | **Selections** are groups of DOM elements that can be manipulated together.
-- | In D3, selections are the primary way to work with the DOM - you select elements,
-- | bind data to them, and apply transformations.
-- |
-- | **Finally Tagless Encoding** means the selection type and monad are abstracted.
-- | This allows the same visualization code to run with different interpreters:
-- | - `D3M` - Actually manipulates the DOM using D3.js
-- | - `StringM` - Generates a string representation for debugging
-- | - `MetaTreeM` - Builds an AST for analysis
-- |
-- | ## Basic Usage Pattern
-- |
-- | Most visualizations follow this pattern:
-- |
-- | ```purescript
-- | import PSD3
-- | import PSD3.Attributes (width, height, fill, cx, cy, radius)
-- |
-- | myChart :: forall m. SelectionM D3Selection_ m => m Unit
-- | myChart = do
-- |   -- 1. Attach to a container element
-- |   root <- attach "#chart"
-- |
-- |   -- 2. Create SVG canvas
-- |   svg <- appendTo root Svg [width 800.0, height 600.0]
-- |
-- |   -- 3. Append elements with attributes
-- |   circle <- appendTo svg Circle [cx 100.0, cy 100.0, radius 50.0, fill "blue"]
-- |
-- |   pure unit
-- | ```
-- |
-- | ## Data Joins
-- |
-- | D3's power comes from binding data to DOM elements using the "General Update Pattern":
-- |
-- | ```purescript
-- | -- Simple join - for static data that doesn't change
-- | circles <- simpleJoin svg Circle myData keyFn
-- | setAttributes circles [radius 5.0, fill "red"]
-- |
-- | -- Update join - for dynamic data (enter/update/exit pattern)
-- | result <- updateJoin svg Circle myData keyFn
-- |
-- | -- Handle entering elements (newly added data)
-- | setAttributes result.enter [radius 5.0, fill "green"]
-- |
-- | -- Handle updating elements (existing data)
-- | setAttributes result.update [radius 7.0, fill "blue"]
-- |
-- | -- Handle exiting elements (removed data)
-- | setAttributes result.exit [remove]
-- | ```
-- |
-- | ## See Also
-- |
-- | - `PSD3.Attributes` for attribute functions like `width`, `fill`, `cx`, etc.
-- | - `PSD3.Interpreter.D3` for the main D3 interpreter
-- | - [D3 Selections](https://d3js.org/d3-selection) for the underlying D3.js concepts
module PSD3.Capabilities.Selection where

import Data.Foldable (class Foldable)
import PSD3.Internal.Types (Datum_, Element, Index_, Selector)
import PSD3.Internal.Selection.Types (Behavior, SelectionAttribute)
import Prelude (class Monad, Unit)

-- | SelectionM defines the core operations for working with D3 selections.
-- |
-- | This type class provides a monadic interface for building visualizations,
-- | where `selection` is the selection type (e.g., `D3Selection_`) and `m` is
-- | the monad (e.g., `D3M`).
-- |
-- | The operations are designed to mirror D3's selection API while maintaining
-- | type safety and composability.
class (Monad m) <= SelectionM selection m where
  -- | Append a new element to a selection and return the new element.
  -- |
  -- | This is the primary way to build up a visualization by adding elements
  -- | to a container. Each element can have attributes applied immediately.
  -- |
  -- | ```purescript
  -- | svg <- appendTo root Svg [width 800.0, height 600.0]
  -- | circle <- appendTo svg Circle [cx 50.0, cy 50.0, radius 25.0]
  -- | ```
  -- |
  -- | Maps to D3's `selection.append()` - see https://d3js.org/d3-selection#selection_append
  appendTo        :: forall d. selection d -> Element -> Array (SelectionAttribute d) -> m (selection d)

  -- | Select descendant elements matching a selector within a selection.
  -- |
  -- | This allows you to select elements that are children of the current selection,
  -- | useful for working with structured SVG groups or HTML layouts.
  -- |
  -- | ```purescript
  -- | groups <- selectUnder svg "g.data-group"
  -- | circles <- selectUnder groups "circle"
  -- | ```
  -- |
  -- | Maps to D3's `selection.selectAll()` - see https://d3js.org/d3-selection#selection_selectAll
  selectUnder     :: forall d. selection d -> Selector (selection d) -> m (selection d)

  -- | Attach to an existing DOM element using a CSS selector.
  -- |
  -- | This is typically the **first operation** in any visualization - it selects
  -- | the container element where your visualization will be rendered.
  -- |
  -- | ```purescript
  -- | root <- attach "#chart"  -- Selects <div id="chart"></div>
  -- | ```
  -- |
  -- | Maps to D3's `d3.select()` - see https://d3js.org/d3-selection#select
  attach          :: forall d. Selector (selection d) -> m (selection d)

  -- | Filter a selection to only elements matching a selector.
  -- |
  -- | Useful for narrowing down selections based on classes or other attributes.
  -- |
  -- | ```purescript
  -- | allCircles <- selectUnder svg "circle"
  -- | redCircles <- filterSelection allCircles ".red"
  -- | ```
  -- |
  -- | Maps to D3's `selection.filter()` - see https://d3js.org/d3-selection#selection_filter
  filterSelection :: forall d. selection d -> Selector (selection d) -> m (selection d)

  -- | Merge two selections into one.
  -- |
  -- | Commonly used to merge enter and update selections after a data join
  -- | so you can apply the same attributes to both.
  -- |
  -- | ```purescript
  -- | result <- updateJoin svg Circle data keyFn
  -- | merged <- mergeSelections result.enter result.update
  -- | setAttributes merged [fill "blue", radius 5.0]
  -- | ```
  -- |
  -- | Maps to D3's `selection.merge()` - see https://d3js.org/d3-selection#selection_merge
  mergeSelections :: forall d. selection d -> selection d -> m (selection d)

  -- | Apply attributes to a selection.
  -- |
  -- | This is how you style and position elements. Attributes are applied in order.
  -- |
  -- | ```purescript
  -- | setAttributes circle [fill "red", stroke "black", strokeWidth 2.0]
  -- | ```
  -- |
  -- | See `PSD3.Attributes` for the full list of available attributes.
  setAttributes   :: forall d. selection d -> Array (SelectionAttribute d) -> m Unit

  -- | Attach behavior (drag, zoom) to a selection.
  -- |
  -- | This enables interactivity by attaching event handlers.
  -- |
  -- | ```purescript
  -- | on circles (Drag DefaultDrag)
  -- | on svg (Zoom { extent: ..., scale: ..., name: "chart", target: svg })
  -- | ```
  -- |
  -- | See `PSD3.Internal.Selection.Types` for available behaviors.
  on              :: forall d. selection d -> Behavior (selection d) -> m Unit

  -- | Open a selection for data binding.
  -- |
  -- | This is an advanced operation used internally by the update join pattern.
  -- | Most users won't need to call this directly.
  -- |
  -- | **Note**: This operation may be refactored in future versions.
  openSelection   :: forall d. selection d -> Selector (selection d) -> m (selection d)

  -- | Bind data to elements using a simple join (enter-only pattern).
  -- |
  -- | Use this when you have **static data** that doesn't change. It's simpler
  -- | than `updateJoin` and only handles the "enter" case.
  -- |
  -- | ```purescript
  -- | let data = [1, 2, 3, 4, 5]
  -- | circles <- simpleJoin svg Circle data keyIsID_
  -- | setAttributes circles [cy 50.0, radius 10.0]
  -- | ```
  -- |
  -- | The key function identifies each datum uniquely for D3's internal tracking.
  -- |
  -- | Maps to D3's data join - see https://d3js.org/d3-selection#joining-data
  simpleJoin      :: forall d datum key. selection d -> Element -> (Array datum) -> (datum -> key) -> m (selection datum)

  -- | Bind data to elements using nested selections where child data is extracted from parent datum.
  -- |
  -- | Use this for **hierarchical data structures** where child elements need data derived from
  -- | their parent element's bound datum. This enables patterns like tables (rows → cells) or
  -- | nested visualizations (groups → elements).
  -- |
  -- | ```purescript
  -- | let matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  -- |
  -- | -- First join: bind rows to outer array
  -- | rows <- simpleJoin tbody Tr matrix keyIsID_
  -- |
  -- | -- Second join: bind cells to each row's inner array
  -- | cellSelection <- openSelection rows "td"
  -- | cells <- nestedJoin cellSelection Td
  -- |            (\d -> coerceDatumToArray d)  -- Extract array from parent datum
  -- |            keyIsID_
  -- | ```
  -- |
  -- | The extraction function receives the parent element's datum and returns any `Foldable`
  -- | container (Array, List, Map.values, etc.). This is automatically converted to an Array
  -- | for D3's use.
  -- |
  -- | **Type flexibility:** The `Foldable` constraint means you can extract from:
  -- | - Arrays: `\d -> coerceDatumToArray d`
  -- | - Lists: `\d -> coerceDatumToList d`
  -- | - Map values: `\d -> Map.values (coerceDatumToMap d)`
  -- | - Lenses: `\d -> view (_record <<< _children) d`
  -- | - Transformed data: `\d -> filter p >>> map f $ extractData d`
  -- |
  -- | See Mike Bostock's [Nested Selections](https://bost.ocks.org/mike/nest/) for more on this pattern.
  -- |
  -- | Maps to D3's `.data(function(d) { return d; })` pattern.
  nestedJoin      :: forall d f datum. Foldable f =>
                     selection d -> Element -> (Datum_ -> f datum) -> (Datum_ -> Index_) -> m (selection datum)

  -- | Bind data to elements using the General Update Pattern (enter/update/exit).
  -- |
  -- | Use this when you have **dynamic data** that changes over time. It returns
  -- | three selections so you can handle each case differently:
  -- |
  -- | - `enter`: New data points that need elements created
  -- | - `update`: Existing data points that need updates
  -- | - `exit`: Old elements whose data was removed
  -- |
  -- | ```purescript
  -- | result <- updateJoin svg Circle data keyFn
  -- |
  -- | -- Create new elements
  -- | newCircles <- appendTo result.enter Circle []
  -- | setAttributes newCircles [fill "green", radius 5.0]
  -- |
  -- | -- Update existing elements
  -- | setAttributes result.update [fill "blue"]
  -- |
  -- | -- Remove old elements
  -- | setAttributes result.exit [remove]
  -- | ```
  -- |
  -- | See https://d3js.org/d3-selection#joining-data for the General Update Pattern.
  updateJoin      :: forall d datum key. selection d -> Element -> (Array datum) -> (datum -> key)
    -> m { enter :: selection datum, exit :: selection d, update :: selection datum }
