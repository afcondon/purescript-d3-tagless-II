module PSD3.Reference.Modules.AttributesModule where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.DocParser as DocParser
import PSD3.Shared.RHSNavigation as RHSNav
import PSD3.Understanding.TOC (renderTOC)
import PSD3.Website.Types (Route(..))
import Type.Proxy (Proxy(..))

type State = Unit
data Action = Initialize

type Slots = ( rhsNav :: forall q. H.Slot q Void Unit )
_rhsNav = Proxy :: Proxy "rhsNav"

component :: forall q i. H.Component q i Void Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render _ =
  let
    parsed = DocParser.parseDocumentation sourceCode
  in
  HH.div
    [ HP.classes [ HH.ClassName "reference-page" ] ]
    [ renderTOC
        { title: "Attributes"
        , items:
            [ { anchor: "overview", label: "Attributes Module", level: 0 }
            ]
        , image: Just "images/reference-bookmark-trees.jpeg"
        }

    , HH.slot_ _rhsNav unit RHSNav.component Reference

    -- Module info box
    , HH.div
        [ HP.classes [ HH.ClassName "module-info-box" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "module-info-left" ] ]
            [ HH.strong_ [ HH.text "Module: " ]
            , HH.code_ [ HH.text "PSD3.Attributes" ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "module-info-right" ] ]
            [ HH.strong_ [ HH.text "File: " ]
            , HH.code_ [ HH.text "src/lib/PSD3/Attributes.purs" ]
            ]
        ]

    -- Documentation section
    , HH.div
        [ HP.classes [ HH.ClassName "reference-content" ]
        , HP.id "overview"
        ]
        (DocParser.markdownToHtml parsed.docLines)

    -- Source code section
    , HH.div
        [ HP.classes [ HH.ClassName "reference-code-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "reference-code-title" ] ]
            [ HH.text "Source Code" ]
        , HH.pre_
            [ HH.code
                [ HP.classes [ HH.ClassName "language-haskell" ] ]
                [ HH.text (String.joinWith "\n" parsed.codeLines) ]
            ]
        ]
    ]

sourceCode :: String
sourceCode = """-- | PSD3.Attributes - All attribute functions for D3 selections
-- |
-- | This module provides functions for setting visual properties on D3 selections.
-- | Attributes control how SVG elements look and behave - their position, size,
-- | color, text content, and interactive behaviors.
-- |
-- | ## What are Attributes?
-- |
-- | In D3 and SVG, attributes are properties set on DOM elements. In PSD3, attribute
-- | functions create `SelectionAttribute` values that are applied to selections via
-- | `appendTo` or `setAttributes`.
-- |
-- | Attributes can be:
-- | - **Static**: Same value for all elements (e.g., `fill "red"`)
-- | - **Dynamic**: Computed from bound data (e.g., `fill (\\d -> d.color)`)
-- | - **Indexed**: Computed from data and index (e.g., `x (\\d i -> i * 10.0)`)
-- |
-- | The `ToAttr` type class automatically handles all three cases, so you can use
-- | literals or functions interchangeably.
-- |
-- | ## Basic Usage
-- |
-- | **Static attributes** (same for all elements):
-- | ```purescript
-- | import PSD3
-- | import PSD3.Attributes
-- |
-- | myCircle = do
-- |   circle <- appendTo svg Circle
-- |     [ cx 100.0
-- |     , cy 100.0
-- |     , radius 50.0
-- |     , fill "red"
-- |     , strokeColor "black"
-- |     , strokeWidth 2.0
-- |     ]
-- |   pure circle
-- | ```
-- |
-- | **Dynamic attributes** (computed from data):
-- | ```purescript
-- | -- Assuming data is Array { x :: Number, y :: Number, color :: String }
-- | circles <- simpleJoin svg Circle data keyFn
-- | setAttributes circles
-- |   [ cx (\\d -> d.x)           -- Function of datum
-- |   , cy (\\d -> d.y)
-- |   , radius 5.0               -- Static value
-- |   , fill (\\d -> d.color)
-- |   ]
-- | ```
-- |
-- | **Indexed attributes** (computed from data and index):
-- | ```purescript
-- | bars <- simpleJoin svg Rect data keyFn
-- | setAttributes bars
-- |   [ x (\\d i -> i * 20.0)     -- Function of datum and index
-- |   , y (\\d -> 100.0 - d.value)
-- |   , width 18.0
-- |   , height (\\d -> d.value)
-- |   ]
-- | ```
-- |
-- | ## Attribute Categories
-- |
-- | ### Position
-- | Position elements in the coordinate system:
-- | - `x`, `y` - Top-left corner (rectangles, text)
-- | - `cx`, `cy` - Center point (circles, ellipses)
-- | - `dx`, `dy` - Offset from current position (text)
-- | - `x1`, `y1`, `x2`, `y2` - Line endpoints
-- |
-- | ### Size
-- | Control element dimensions:
-- | - `width`, `height` - Rectangle dimensions
-- | - `width100`, `height100` - 100% width/height (for containers)
-- | - `radius` - Circle radius (maps to SVG's `r` attribute)
-- |
-- | ### Colors and Opacity
-- | Visual styling:
-- | - `fill` - Fill color (e.g., "red", "#ff0000", "rgb(255,0,0)")
-- | - `fillOpacity` - Fill transparency (0.0 to 1.0)
-- | - `strokeColor` - Border/line color
-- | - `strokeWidth` - Border/line width in pixels
-- | - `strokeOpacity` - Border transparency
-- | - `strokeLineJoin` - How line segments join (Arcs, Bevel, Miter, MiterClip, Round)
-- | - `opacity` - Overall element opacity
-- | - `backgroundColor` - Background color (HTML elements)
-- |
-- | ### Text
-- | Text content and styling:
-- | - `text` - Text content to display
-- | - `fontSize` - Font size in pixels
-- | - `fontFamily` - Font name (e.g., "Arial", "monospace")
-- | - `textAnchor` - Horizontal alignment ("start", "middle", "end")
-- |
-- | ### SVG Specific
-- | SVG-only attributes:
-- | - `viewBox` - Defines coordinate system (x, y, width, height)
-- | - `autoBox` - Auto-compute viewBox from element bounds
-- | - `preserveAspectRatio` - How to scale viewBox (AspectRatioSpec)
-- | - `d` - Path data for `<path>` elements
-- | - `pointXY` - Set both x and y from a point record
-- |
-- | ### Styling and Classes
-- | CSS and visual styling:
-- | - `classed` - Set CSS class (e.g., "active selected")
-- | - `cursor` - Mouse cursor style ("pointer", "crosshair", etc.)
-- |
-- | ### Transforms
-- | Position and rotation transformations:
-- | - `transform` - Array of transform functions (assembled into one)
-- | - `rotate` - Rotation angle or function
-- | - `originX`, `originY` - Transform origin point
-- | - `assembleTransforms` - Combine multiple transforms into one
-- |
-- | ### Transitions
-- | Animated attribute changes:
-- | ```purescript
-- | setAttributes circles
-- |   [ transitionWithDuration (Milliseconds 500.0) `to`
-- |     [ cx 200.0
-- |     , fill "blue"
-- |     ]
-- |   ]
-- | ```
-- |
-- | Transition functions:
-- | - `transition` - Start transition with full config (name, delay, duration, easing)
-- | - `transitionWithDuration` - Simple transition with duration only
-- | - `namedTransition` - Named transition for coordination
-- | - `defaultTransition` - Default transition settings
-- | - `to` - Chain attributes to a transition
-- | - `andThen` - Chain multiple transitions
-- |
-- | ### Events
-- | Mouse event handlers:
-- | - `onMouseEvent` - Pure event handler (datum, index, element) -> Unit
-- | - `onMouseEventEffectful` - Effectful handler (datum, index, element) -> Effect Unit
-- |
-- | Mouse events: Click, DblClick, MouseDown, MouseUp, MouseEnter, MouseLeave, MouseMove, MouseOver, MouseOut
-- |
-- | ### Special Operations
-- | - `remove` - Remove element from DOM (typically for exit selection)
-- | - `lower` - Move element to back (z-order)
-- | - `raise` - Move element to front (z-order)
-- | - `order` - Sort elements by data order
-- | - `sortSelection` - Sort by custom comparison function
-- |
-- | ## Working with Data
-- |
-- | The power of D3 comes from binding data to attributes. When you use functions
-- | as attribute values, they receive the bound datum:
-- |
-- | ```purescript
-- | type DataPoint = { name :: String, value :: Number, category :: String }
-- |
-- | let categoryColor = case _ of
-- |       "A" -> "red"
-- |       "B" -> "blue"
-- |       _   -> "gray"
-- |
-- | circles <- simpleJoin svg Circle myData keyFn
-- | setAttributes circles
-- |   [ cx (\\d -> d.value * 10.0)            -- Scale value to position
-- |   , cy (\\d i -> i * 20.0)                -- Use index for vertical spacing
-- |   , radius (\\d -> sqrt d.value)          -- Size proportional to sqrt
-- |   , fill (\\d -> categoryColor d.category) -- Color by category
-- |   ]
-- | ```
-- |
-- | ## Type Class: ToAttr
-- |
-- | The `ToAttr` type class automatically converts values to attributes:
-- | - `ToAttr Number a` - For numeric attributes (x, y, width, radius, etc.)
-- | - `ToAttr String a` - For string attributes (fill, text, class, etc.)
-- |
-- | This allows:
-- | - Literals: `fill "red"`, `radius 50.0`
-- | - Functions: `fill (\\d -> d.color)`, `radius (\\d -> d.size)`
-- | - Indexed functions: `x (\\d i -> i * 10.0)`
-- |
-- | ## See Also
-- |
-- | - `PSD3.Capabilities.Selection` - For applying attributes to selections
-- | - `PSD3.Internal.Attributes.Instances` - For the ToAttr type class
-- | - [SVG Attributes](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute) - MDN reference
module PSD3.Attributes
  ( module Sugar
  ) where

import PSD3.Internal.Attributes.Sugar (AlignAspectRatio_X(..), AlignAspectRatio_Y(..), AspectRatioPreserve(..), AspectRatioSpec(..), andThen, assembleTransforms, autoBox, backgroundColor, classed, cursor, cx, cy, d, defaultTransition, dx, dy, fill, fillOpacity, fontFamily, fontSize, height, height100, namedTransition, onMouseEvent, onMouseEventEffectful, opacity, originX, originY, pointXY, preserveAspectRatio, radius, remove, rotate, strokeColor, strokeLineJoin, strokeOpacity, strokeWidth, strength, text, textAnchor, to, transform, transform', transition, transitionWithDuration, viewBox, width, width100, x, x1, x2, y, y1, y2) as Sugar
"""

handleAction :: forall m. Action -> H.HalogenM State Action Slots Void m Unit
handleAction = case _ of
  Initialize -> pure unit
