module PSD3v2.Behavior.Types
  ( Behavior(..)
  , ZoomConfig(..)
  , DragConfig(..)
  , ScaleExtent(..)
  , defaultDrag
  , simulationDrag
  , defaultZoom
  , onClick
  , onClickWithDatum
  , onMouseEnter
  , onMouseLeave
  , HighlightStyle
  , onHover
  , MouseEventInfo
  , onMouseMoveWithInfo
  , onMouseEnterWithInfo
  , onMouseLeaveWithInfo
  ) where

import Prelude

import Effect (Effect)

-- | Scale extent for zoom (min and max zoom levels)
-- |
-- | Example:
-- | - `ScaleExtent 0.5 4.0` allows zooming from 50% to 400%
-- | - `ScaleExtent 1.0 1.0` disables zoom (fixed at 100%)
data ScaleExtent = ScaleExtent Number Number

-- | Style changes for hover highlight effect
-- |
-- | Specifies attribute name-value pairs to apply on enter and leave.
-- | Uses String values for simplicity (works with any attribute type).
-- |
-- | Example:
-- | ```purescript
-- | highlightStyle = { enter: [Tuple "stroke" "#333", Tuple "stroke-width" "3"]
-- |                  , leave: [Tuple "stroke" "#ddd", Tuple "stroke-width" "1.5"]
-- |                  }
-- | ```
type HighlightStyle =
  { enter :: Array { attr :: String, value :: String }
  , leave :: Array { attr :: String, value :: String }
  }

-- | Mouse event information with position data
-- |
-- | Provides both page-relative and element-relative coordinates.
-- |
-- | - `clientX`/`clientY`: Position relative to viewport
-- | - `pageX`/`pageY`: Position relative to document (for tooltip positioning)
-- | - `offsetX`/`offsetY`: Position relative to target element
type MouseEventInfo datum =
  { datum :: datum
  , clientX :: Number
  , clientY :: Number
  , pageX :: Number
  , pageY :: Number
  , offsetX :: Number
  , offsetY :: Number
  }

derive instance Eq ScaleExtent
derive instance Ord ScaleExtent

instance Show ScaleExtent where
  show (ScaleExtent min max) = "ScaleExtent " <> show min <> " " <> show max

-- | Zoom behavior configuration
-- |
-- | The `target` is the selection that will be transformed when zooming.
-- | Typically this is an inner <g> element, while the zoom behavior is
-- | attached to the outer <svg> element.
-- |
-- | Example:
-- | ```purescript
-- | zoomConfig = ZoomConfig
-- |   { scaleExtent: ScaleExtent 0.5 4.0  -- 50% to 400%
-- |   , targetSelector: ".zoom-group"      -- What to transform
-- |   }
-- | ```
newtype ZoomConfig = ZoomConfig
  { scaleExtent :: ScaleExtent
  , targetSelector :: String  -- CSS selector for the element to transform
  }

derive instance Eq ZoomConfig
derive instance Ord ZoomConfig

instance Show ZoomConfig where
  show (ZoomConfig cfg) =
    "ZoomConfig { scaleExtent: " <> show cfg.scaleExtent
    <> ", targetSelector: " <> show cfg.targetSelector <> " }"

-- | Drag behavior configuration
-- |
-- | - `SimpleDrag`: Basic dragging with transform
-- | - `SimulationDrag`: Drag with force simulation reheat (for force-directed graphs)
data DragConfig
  = SimpleDrag
  | SimulationDrag String  -- Simulation ID for accessing D3SimulationState_

derive instance Eq DragConfig
derive instance Ord DragConfig

instance Show DragConfig where
  show SimpleDrag = "SimpleDrag"
  show (SimulationDrag id) = "SimulationDrag " <> show id

-- | Behaviors that can be attached to selections
-- |
-- | Parameterized by datum type to enable typed event handlers.
-- |
-- | - `Zoom`: Pan and zoom with mouse/touch
-- | - `Drag`: Drag elements with mouse/touch (simple or simulation-aware)
-- | - `Click`: Click handler without datum access
-- | - `ClickWithDatum`: Click handler with typed datum access
-- | - `MouseEnter`: Mouse enter handler with typed datum access
-- | - `MouseLeave`: Mouse leave handler with typed datum access
data Behavior datum
  = Zoom ZoomConfig
  | Drag DragConfig
  | Click (Effect Unit)
  | ClickWithDatum (datum -> Effect Unit)
  | MouseEnter (datum -> Effect Unit)
  | MouseLeave (datum -> Effect Unit)
  | Highlight HighlightStyle  -- Hover highlighting with style changes
  | MouseMoveWithInfo (MouseEventInfo datum -> Effect Unit)
  | MouseEnterWithInfo (MouseEventInfo datum -> Effect Unit)
  | MouseLeaveWithInfo (MouseEventInfo datum -> Effect Unit)

-- Note: Can't derive Eq/Ord for function types
-- We only show structure, not function contents
instance Show (Behavior datum) where
  show (Zoom cfg) = "Zoom " <> show cfg
  show (Drag cfg) = "Drag " <> show cfg
  show (Click _) = "Click <handler>"
  show (ClickWithDatum _) = "ClickWithDatum <handler>"
  show (MouseEnter _) = "MouseEnter <handler>"
  show (MouseLeave _) = "MouseLeave <handler>"
  show (Highlight _) = "Highlight <styles>"
  show (MouseMoveWithInfo _) = "MouseMoveWithInfo <handler>"
  show (MouseEnterWithInfo _) = "MouseEnterWithInfo <handler>"
  show (MouseLeaveWithInfo _) = "MouseLeaveWithInfo <handler>"

-- | Default drag configuration
-- |
-- | Enables simple drag on the element with default D3 settings.
defaultDrag :: DragConfig
defaultDrag = SimpleDrag

-- | Simulation-aware drag configuration
-- |
-- | Enables drag with force simulation reheat.
-- | When dragging starts, simulation alpha is increased to reheat.
-- | When dragging ends, simulation cools back down.
-- |
-- | Example:
-- | ```purescript
-- | nodeCircles <- append Circle [...] nodeEnter
-- | _ <- on (Drag $ simulationDrag "lesmis") nodeCircles
-- | ```
simulationDrag :: String -> DragConfig
simulationDrag = SimulationDrag

-- | Default zoom configuration
-- |
-- | Requires you to specify:
-- | - Scale extent (min/max zoom)
-- | - Target selector (what element to transform)
-- |
-- | Example:
-- | ```purescript
-- | zoom <- on (Zoom $ defaultZoom (ScaleExtent 0.5 4.0) ".zoom-group") svg
-- | ```
defaultZoom :: ScaleExtent -> String -> ZoomConfig
defaultZoom scaleExtent targetSelector = ZoomConfig { scaleExtent, targetSelector }

-- | Click handler without datum access
-- |
-- | Use when you don't need the data bound to the clicked element.
-- |
-- | Example:
-- | ```purescript
-- | button <- append Circle [radius 20.0] container
-- | _ <- on (onClick (log "Button clicked!")) button
-- | ```
onClick :: forall datum. Effect Unit -> Behavior datum
onClick = Click

-- | Click handler with typed datum access
-- |
-- | The datum is recovered from the DOM element using D3's `__data__` property.
-- | Type safety is preserved through the Selection's phantom type parameter.
-- |
-- | Example:
-- | ```purescript
-- | type CircleData = { id :: Int, color :: String }
-- |
-- | circles <- append Circle [...] (joinData data)
-- | _ <- on (onClickWithDatum \d -> log ("Clicked circle: " <> show d.id)) circles
-- | ```
onClickWithDatum :: forall datum. (datum -> Effect Unit) -> Behavior datum
onClickWithDatum = ClickWithDatum

-- | Mouse enter handler with typed datum access
-- |
-- | Fires when mouse enters the element. Does not bubble.
-- | Useful for hover effects like highlighting.
-- |
-- | Example:
-- | ```purescript
-- | lines <- joinData "lines" "path" series (\s -> ...)
-- | _ <- on (onMouseEnter \s -> log ("Hovered: " <> s.division)) lines
-- | ```
onMouseEnter :: forall datum. (datum -> Effect Unit) -> Behavior datum
onMouseEnter = MouseEnter

-- | Mouse leave handler with typed datum access
-- |
-- | Fires when mouse leaves the element. Does not bubble.
-- | Pair with onMouseEnter for hover effects.
-- |
-- | Example:
-- | ```purescript
-- | _ <- on (onMouseLeave \_ -> resetHighlight) lines
-- | ```
onMouseLeave :: forall datum. (datum -> Effect Unit) -> Behavior datum
onMouseLeave = MouseLeave

-- | Hover highlight behavior
-- |
-- | Applies style changes on mouse enter and resets on mouse leave.
-- | Also raises the element to front on hover.
-- |
-- | Example:
-- | ```purescript
-- | _ <- on (onHover
-- |   { enter: [{ attr: "stroke", value: "#333" }, { attr: "stroke-width", value: "3" }]
-- |   , leave: [{ attr: "stroke", value: "#ddd" }, { attr: "stroke-width", value: "1.5" }]
-- |   }) lines
-- | ```
onHover :: forall datum. HighlightStyle -> Behavior datum
onHover = Highlight

-- | Mouse move handler with full event info
-- |
-- | Fires continuously as mouse moves over the element.
-- | Provides datum and mouse coordinates for tooltips, crosshairs, etc.
-- |
-- | Example:
-- | ```purescript
-- | _ <- on (onMouseMoveWithInfo \info -> do
-- |   -- info.datum is the bound data
-- |   -- info.pageX/pageY for tooltip positioning
-- |   -- info.offsetX/offsetY for data lookup
-- |   updateTooltip info.datum info.pageX info.pageY
-- | ) lines
-- | ```
onMouseMoveWithInfo :: forall datum. (MouseEventInfo datum -> Effect Unit) -> Behavior datum
onMouseMoveWithInfo = MouseMoveWithInfo

-- | Mouse enter handler with full event info
-- |
-- | Like onMouseEnter but also provides mouse coordinates.
onMouseEnterWithInfo :: forall datum. (MouseEventInfo datum -> Effect Unit) -> Behavior datum
onMouseEnterWithInfo = MouseEnterWithInfo

-- | Mouse leave handler with full event info
-- |
-- | Like onMouseLeave but also provides mouse coordinates.
onMouseLeaveWithInfo :: forall datum. (MouseEventInfo datum -> Effect Unit) -> Behavior datum
onMouseLeaveWithInfo = MouseLeaveWithInfo
