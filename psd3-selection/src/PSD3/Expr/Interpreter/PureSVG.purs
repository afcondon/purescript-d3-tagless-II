-- | Pure SVG Interpreter
-- |
-- | Generates complete SVG documents without any D3 dependency.
-- | Use cases:
-- | - Server-side rendering (zero JS on client)
-- | - Browser-side static charts (no D3 bundle needed)
-- | - Exporting visualizations to files
-- | - Email/PDF embedding
-- |
-- | For simple visualizations, this can replace D3 entirely,
-- | dramatically reducing bundle size.
module PSD3.Expr.Interpreter.PureSVG
  ( -- Types
    SVGElement(..)
  , SVGDoc
    -- Building
  , svg
  , g
  , circle
  , rect
  , line
  , text
  , path
    -- Rendering
  , render
  , renderDoc
    -- Helpers for data-driven generation
  , forData
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))

-- =============================================================================
-- Types
-- =============================================================================

-- | An SVG element with tag, attributes, and children
data SVGElement
  = Element
      { tag :: String
      , attrs :: Array { name :: String, value :: String }
      , children :: Array SVGElement
      }
  | TextContent String

-- | A complete SVG document
type SVGDoc =
  { width :: Number
  , height :: Number
  , viewBox :: Maybe String
  , children :: Array SVGElement
  }

-- =============================================================================
-- Element Constructors
-- =============================================================================

-- | Create an SVG root element
svg :: Number -> Number -> Array SVGElement -> SVGDoc
svg w h children =
  { width: w
  , height: h
  , viewBox: Nothing
  , children
  }

-- | Group element
g :: Array { name :: String, value :: String } -> Array SVGElement -> SVGElement
g attrs children = Element { tag: "g", attrs, children }

-- | Circle element
circle
  :: { cx :: String, cy :: String, r :: String }
  -> Array { name :: String, value :: String }
  -> SVGElement
circle { cx, cy, r } extraAttrs = Element
  { tag: "circle"
  , attrs:
      [ { name: "cx", value: cx }
      , { name: "cy", value: cy }
      , { name: "r", value: r }
      ] <> extraAttrs
  , children: []
  }

-- | Rectangle element
rect
  :: { x :: String, y :: String, width :: String, height :: String }
  -> Array { name :: String, value :: String }
  -> SVGElement
rect { x, y, width, height } extraAttrs = Element
  { tag: "rect"
  , attrs:
      [ { name: "x", value: x }
      , { name: "y", value: y }
      , { name: "width", value: width }
      , { name: "height", value: height }
      ] <> extraAttrs
  , children: []
  }

-- | Line element
line
  :: { x1 :: String, y1 :: String, x2 :: String, y2 :: String }
  -> Array { name :: String, value :: String }
  -> SVGElement
line { x1, y1, x2, y2 } extraAttrs = Element
  { tag: "line"
  , attrs:
      [ { name: "x1", value: x1 }
      , { name: "y1", value: y1 }
      , { name: "x2", value: x2 }
      , { name: "y2", value: y2 }
      ] <> extraAttrs
  , children: []
  }

-- | Text element
text
  :: { x :: String, y :: String }
  -> Array { name :: String, value :: String }
  -> String
  -> SVGElement
text { x, y } extraAttrs content = Element
  { tag: "text"
  , attrs:
      [ { name: "x", value: x }
      , { name: "y", value: y }
      ] <> extraAttrs
  , children: [ TextContent content ]
  }

-- | Path element
path :: String -> Array { name :: String, value :: String } -> SVGElement
path d extraAttrs = Element
  { tag: "path"
  , attrs: [ { name: "d", value: d } ] <> extraAttrs
  , children: []
  }

-- =============================================================================
-- Data-Driven Helpers
-- =============================================================================

-- | Map over data with index, like D3's data join but pure
forData :: forall a. Array a -> (a -> Int -> SVGElement) -> Array SVGElement
forData arr f = mapWithIndex (\i a -> f a i) arr

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Render an element to SVG string
render :: SVGElement -> String
render = case _ of
  TextContent s -> escapeXml s
  Element { tag, attrs, children } ->
    let
      attrStr = foldl (\acc { name, value } ->
        acc <> " " <> name <> "=\"" <> escapeXml value <> "\"") "" attrs
      childStr = foldl (\acc child -> acc <> render child) "" children
    in
      if childStr == ""
        then "<" <> tag <> attrStr <> " />"
        else "<" <> tag <> attrStr <> ">" <> childStr <> "</" <> tag <> ">"

-- | Render a complete SVG document
renderDoc :: SVGDoc -> String
renderDoc doc =
  let
    viewBoxAttr = case doc.viewBox of
      Just vb -> " viewBox=\"" <> vb <> "\""
      Nothing -> ""
    childStr = foldl (\acc child -> acc <> "\n  " <> render child) "" doc.children
  in
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
      <> show doc.width <> "\" height=\"" <> show doc.height <> "\""
      <> viewBoxAttr <> ">"
      <> childStr
      <> "\n</svg>"

-- | Basic XML escaping
escapeXml :: String -> String
escapeXml s = s -- TODO: proper escaping of <, >, &, ", '
