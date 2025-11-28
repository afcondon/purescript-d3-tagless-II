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
import PSD3v2.Attribute.Types (class_, d, fill, stroke, textAnchor, x, y, x1, y1, x2, y2, transform, fontSize, dy, textContent)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3v2.VizTree.Tree (Tree)
import PSD3v2.VizTree.Tree as T

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
          [ class_ "tick"
          , transform (tickTransform pos)
          ]
          `T.withChildren`
            [ T.elem Line
                [ x1 tickLine.x1
                , y1 tickLine.y1
                , x2 tickLine.x2
                , y2 tickLine.y2
                , stroke "currentColor"
                ]
            , T.elem Text
                [ x tickText.x
                , y tickText.y
                , textAnchor tickText.anchor
                , dy tickText.dyVal
                , fill "currentColor"
                , fontSize 10.0
                , textContent (formatter tickValue)
                ]
            ]
      ) ticks

  in
    T.elem Group
      [ class_ "axis"
      , fill "none"
      , fontSize 10.0
      , stroke "currentColor"
      ]
      `T.withChildren`
        ([ -- Domain path
           T.elem Path
            [ class_ "domain"
            , stroke "currentColor"
            , d domainPath
            ]
         ] <> tickElements)
