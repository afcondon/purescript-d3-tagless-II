module PSD3v2.Axis.Axis
  ( Axis
  , Orientation(..)
  , AxisConfig
  , Scale
  , defaultAxisConfig
  , axisBottom
  , axisLeft
  , axisTop
  , axisRight
  , axis
  , renderAxis
  ) where

import Prelude

import Data.Array (range)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (fixed, toStringWith)
-- v3 Integration: all attributes via v3Attr/v3AttrStr (no ToAttr typeclass)
import PSD3v3.Integration (v3Attr, v3AttrStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3.AST (Tree)
import PSD3.AST as T

-- | Axis orientation
data Orientation
  = Top
  | Right
  | Bottom
  | Left

derive instance eqOrientation :: Eq Orientation

-- | Scale type (simplified for now - just linear with domain/range)
type Scale =
  { domain :: { min :: Number, max :: Number }
  , range :: { min :: Number, max :: Number }
  }

-- | Apply scale to a value
applyScale :: Scale -> Number -> Number
applyScale scale value =
  let
    domainSpan = scale.domain.max - scale.domain.min
    rangeSpan = scale.range.max - scale.range.min
    normalized = (value - scale.domain.min) / domainSpan
  in
    scale.range.min + (normalized * rangeSpan)

-- | Axis configuration
type AxisConfig =
  { tickSize :: Number        -- Length of tick marks
  , tickPadding :: Number     -- Space between tick and label
  , tickCount :: Int          -- Approximate number of ticks
  , tickFormat :: Maybe (Number -> String)  -- Custom formatter
  }

defaultAxisConfig :: AxisConfig
defaultAxisConfig =
  { tickSize: 6.0
  , tickPadding: 3.0
  , tickCount: 10
  , tickFormat: Nothing
  }

-- | Opaque axis type (holds orientation, scale, and config)
type Axis =
  { orientation :: Orientation
  , scale :: Scale
  , config :: AxisConfig
  }

-- | Create axis constructors
axisBottom :: Scale -> Axis
axisBottom scale = { orientation: Bottom, scale, config: defaultAxisConfig }

axisLeft :: Scale -> Axis
axisLeft scale = { orientation: Left, scale, config: defaultAxisConfig }

axisTop :: Scale -> Axis
axisTop scale = { orientation: Top, scale, config: defaultAxisConfig }

axisRight :: Scale -> Axis
axisRight scale = { orientation: Right, scale, config: defaultAxisConfig }

-- | Generic axis constructor
axis :: Orientation -> Scale -> Axis
axis orientation scale = { orientation, scale, config: defaultAxisConfig }

-- | Generate tick values
generateTicks :: Scale -> Int -> Array Number
generateTicks scale count =
  let
    step = (scale.domain.max - scale.domain.min) / Int.toNumber count
  in
    map (\i -> scale.domain.min + (Int.toNumber i * step)) (range 0 count)

-- | Default tick formatter
defaultTickFormat :: Number -> String
defaultTickFormat n =
  if n == 0.0
    then "0"
    else toStringWith (fixed 1) n

-- | Render axis as a Tree structure
-- |
-- | Returns a Group element containing:
-- | - A path for the domain line
-- | - Group elements for each tick (with line + text)
renderAxis :: forall datum. Axis -> Tree datum
renderAxis ax =
  let
    ticks = generateTicks ax.scale ax.config.tickCount
    formatter = fromMaybe defaultTickFormat ax.config.tickFormat

    -- Calculate positions based on orientation
    { domainPath, tickTransform, tickLine, tickText } = case ax.orientation of
      Bottom ->
        { domainPath: "M" <> show ax.scale.range.min <> ",0H" <> show ax.scale.range.max
        , tickTransform: \pos -> "translate(" <> show pos <> ",0)"
        , tickLine: { x1: 0.0, y1: 0.0, x2: 0.0, y2: ax.config.tickSize }
        , tickText: { x: 0.0, y: ax.config.tickSize + ax.config.tickPadding, anchor: "middle", dyVal: 7.0 }
        }
      Left ->
        { domainPath: "M0," <> show ax.scale.range.min <> "V" <> show ax.scale.range.max
        , tickTransform: \pos -> "translate(0," <> show pos <> ")"
        , tickLine: { x1: -ax.config.tickSize, y1: 0.0, x2: 0.0, y2: 0.0 }
        , tickText: { x: -(ax.config.tickSize + ax.config.tickPadding), y: 0.0, anchor: "end", dyVal: 3.0 }
        }
      Top ->
        { domainPath: "M" <> show ax.scale.range.min <> ",0H" <> show ax.scale.range.max
        , tickTransform: \pos -> "translate(" <> show pos <> ",0)"
        , tickLine: { x1: 0.0, y1: -ax.config.tickSize, x2: 0.0, y2: 0.0 }
        , tickText: { x: 0.0, y: -(ax.config.tickSize + ax.config.tickPadding), anchor: "middle", dyVal: 0.0 }
        }
      Right ->
        { domainPath: "M0," <> show ax.scale.range.min <> "V" <> show ax.scale.range.max
        , tickTransform: \pos -> "translate(0," <> show pos <> ")"
        , tickLine: { x1: 0.0, y1: 0.0, x2: ax.config.tickSize, y2: 0.0 }
        , tickText: { x: ax.config.tickSize + ax.config.tickPadding, y: 0.0, anchor: "start", dyVal: 3.0 }
        }

    -- Create tick elements
    tickElements :: Array (Tree datum)
    tickElements = map (\tickValue ->
      let pos = applyScale ax.scale tickValue
      in T.elem Group
          [ v3AttrStr "class" (str "tick")
          , v3AttrStr "transform" (str (tickTransform pos))
          ]
          `T.withChildren`
            [ T.elem Line
                [ v3Attr "x1" (lit tickLine.x1)
                , v3Attr "y1" (lit tickLine.y1)
                , v3Attr "x2" (lit tickLine.x2)
                , v3Attr "y2" (lit tickLine.y2)
                , v3AttrStr "stroke" (str "currentColor")
                ]
            , T.elem Text
                [ v3Attr "x" (lit tickText.x)
                , v3Attr "y" (lit tickText.y)
                , v3AttrStr "text-anchor" (str tickText.anchor)
                , v3Attr "dy" (lit tickText.dyVal)
                , v3AttrStr "fill" (str "currentColor")
                , v3Attr "font-size" (lit 10.0)
                , v3AttrStr "textContent" (str (formatter tickValue))
                ]
            ]
      ) ticks

  in
    T.elem Group
      [ v3AttrStr "class" (str "axis")
      , v3AttrStr "fill" (str "none")
      , v3Attr "font-size" (lit 10.0)
      , v3AttrStr "stroke" (str "currentColor")
      ]
      `T.withChildren`
        ([ -- Domain path
           T.elem Path
            [ v3AttrStr "class" (str "domain")
            , v3AttrStr "stroke" (str "currentColor")
            , v3AttrStr "d" (str domainPath)
            ]
         ] <> tickElements)
