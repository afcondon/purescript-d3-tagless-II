-- | DataViz.Layout.Hierarchy.EdgeBundle.BundleCurve
-- |
-- | Bundle curve generator for hierarchical edge bundling.
-- | Implements Danny Holten's algorithm with adjustable beta (tension) parameter.
-- |
-- | The bundle curve draws a smooth B-spline through a path of ancestor nodes:
-- | - beta = 0: straight line from source to target
-- | - beta = 1: curve goes through all ancestor nodes
-- | - beta = 0.85: D3's default, good balance of bundling and readability
module DataViz.Layout.Hierarchy.EdgeBundle.BundleCurve
  ( bundlePath
  , bundlePathRadial
  , bundlePathCartesian
  , BundlePoint
  , beta
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (cos, sin, pi)

-- | A point on the bundle path
type BundlePoint =
  { x :: Number
  , y :: Number
  }

-- | Default beta (tension) value - D3's default
beta :: Number
beta = 0.85

-- | Generate SVG path for a bundle curve given points
-- | Uses quadratic Bezier spline approximation with bundle tension
-- |
-- | The beta parameter controls tension:
-- | - beta = 0: control points are not adjusted (straight-ish curve)
-- | - beta = 1: control points follow the path exactly (maximum bundling)
bundlePath :: Number -> Array BundlePoint -> String
bundlePath tension points =
  case Array.length points of
    0 -> ""
    1 -> case Array.head points of
      Just p -> "M" <> show p.x <> "," <> show p.y
      Nothing -> ""
    2 -> case Array.head points, Array.last points of
      Just p1, Just p2 ->
        "M" <> show p1.x <> "," <> show p1.y
          <> "L"
          <> show p2.x
          <> ","
          <> show p2.y
      _, _ -> ""
    _ ->
      -- Apply bundle transformation and draw B-spline
      let
        bundled = applyBundleTension tension points
      in
        drawBSpline bundled

-- | Generate bundle path from radial coordinates
-- | Takes (angle, radius) pairs and converts to Cartesian for rendering
bundlePathRadial :: Number -> Array { angle :: Number, radius :: Number } -> String
bundlePathRadial tension radialPoints =
  let
    cartesian = map toCartesianPoint radialPoints
  in
    bundlePath tension cartesian

-- | Convert radial to Cartesian coordinates
toCartesianPoint :: { angle :: Number, radius :: Number } -> BundlePoint
toCartesianPoint { angle, radius } =
  { x: radius * cos angle
  , y: radius * sin angle
  }

-- | Generate bundle path from Cartesian coordinates directly
bundlePathCartesian :: Number -> Array { x :: Number, y :: Number } -> String
bundlePathCartesian tension points =
  bundlePath tension points

-- | Apply bundle tension transformation
-- | This is the core of Holten's algorithm:
-- | Each point is interpolated between its position and a straight line
-- |
-- | For point i in path of length n:
-- |   t = i / (n - 1)  (parametric position 0..1)
-- |   straightLine = lerp(start, end, t)
-- |   bundled = lerp(straightLine, actualPoint, beta)
applyBundleTension :: Number -> Array BundlePoint -> Array BundlePoint
applyBundleTension tension points =
  case Array.head points, Array.last points of
    Just start, Just end ->
      let
        n = Array.length points
      in
        Array.mapWithIndex
          ( \i point ->
              let
                -- Parametric position along the path (0 to 1)
                t = toNumber i / toNumber (n - 1)

                -- Position on straight line from start to end
                straightX = lerp start.x end.x t
                straightY = lerp start.y end.y t

                -- Interpolate between straight line and actual path based on tension
                -- tension = 0: follow straight line
                -- tension = 1: follow actual path
                bundledX = lerp straightX point.x tension
                bundledY = lerp straightY point.y tension
              in
                { x: bundledX, y: bundledY }
          )
          points
    _, _ -> points

-- | Linear interpolation
lerp :: Number -> Number -> Number -> Number
lerp a b t = a + (b - a) * t

-- | Draw a B-spline through points using SVG path commands
-- | Uses quadratic Bezier curves for smooth interpolation
drawBSpline :: Array BundlePoint -> String
drawBSpline points =
  case Array.uncons points of
    Nothing -> ""
    Just { head: first, tail: rest } ->
      case Array.uncons rest of
        Nothing ->
          -- Single point
          "M" <> show first.x <> "," <> show first.y
        Just { head: second, tail: remaining } ->
          if Array.null remaining then
            -- Two points - straight line
            "M" <> show first.x <> "," <> show first.y
              <> "L"
              <> show second.x
              <> ","
              <> show second.y
          else
            -- Three or more points - use B-spline
            let
              -- Start with move to first point
              start = "M" <> show first.x <> "," <> show first.y

              -- Use quadratic Bezier through intermediate points
              -- Each segment: Q (control point) (end point)
              -- Control point is current point, end point is midpoint to next
              curves = buildBSplineCurves first (Array.cons second remaining)
            in
              start <> curves

-- | Build B-spline curve segments through a series of points
-- | Uses the Catmull-Rom to Bezier conversion approach
buildBSplineCurves :: BundlePoint -> Array BundlePoint -> String
buildBSplineCurves prev points =
  case Array.length points of
    0 -> ""
    1 -> case Array.head points of
      Just p -> "L" <> show p.x <> "," <> show p.y
      Nothing -> ""
    _ ->
      let
        -- Build quadratic Bezier segments
        -- For B-spline, use midpoints between control points
        result = foldl
          ( \acc point ->
              let
                -- Midpoint between previous and current point
                midX = (acc.prevPoint.x + point.x) / 2.0
                midY = (acc.prevPoint.y + point.y) / 2.0

                -- Quadratic Bezier: control is previous point, end is midpoint
                segment = "Q" <> show acc.prevPoint.x <> "," <> show acc.prevPoint.y
                  <> " "
                  <> show midX
                  <> ","
                  <> show midY
              in
                { path: acc.path <> segment
                , prevPoint: point
                }
          )
          { path: "", prevPoint: prev }
          points

        -- Final segment to last point
        finalSegment = case Array.last points of
          Just lastPoint ->
            "Q" <> show result.prevPoint.x <> "," <> show result.prevPoint.y
              <> " "
              <> show lastPoint.x
              <> ","
              <> show lastPoint.y
          Nothing -> ""
      in
        result.path <> finalSegment

-- | Alternative: Use cubic Bezier for smoother curves
-- | This gives results more similar to D3's curveBundle
bundlePathCubic :: Number -> Array BundlePoint -> String
bundlePathCubic tension points =
  let
    bundled = applyBundleTension tension points
  in
    drawCubicBSpline bundled

-- | Draw cubic B-spline (smoother than quadratic)
drawCubicBSpline :: Array BundlePoint -> String
drawCubicBSpline points =
  case Array.length points of
    0 -> ""
    1 -> case Array.head points of
      Just p -> "M" <> show p.x <> "," <> show p.y
      Nothing -> ""
    2 -> case Array.head points, Array.last points of
      Just p1, Just p2 ->
        "M" <> show p1.x <> "," <> show p1.y
          <> "L"
          <> show p2.x
          <> ","
          <> show p2.y
      _, _ -> ""
    _ ->
      case Array.head points of
        Nothing -> ""
        Just first ->
          let
            start = "M" <> show first.x <> "," <> show first.y

            -- For cubic B-spline, we need at least 4 points per segment
            -- Use Catmull-Rom spline approach for smooth curves
            curves = buildCatmullRomCurves points
          in
            start <> curves

-- | Build Catmull-Rom curves (converted to Bezier for SVG)
buildCatmullRomCurves :: Array BundlePoint -> String
buildCatmullRomCurves points =
  case Array.length points of
    n | n < 3 -> ""
    _ ->
      let
        -- Process each segment (from point i to point i+1)
        -- Using points i-1, i, i+1, i+2 as control points
        segments = Array.mapWithIndex
          ( \i _ ->
              if i == 0 || i >= Array.length points - 1 then
                Nothing
              else
                let
                  p0 = fromMaybe { x: 0.0, y: 0.0 } $ Array.index points (max 0 (i - 1))
                  p1 = fromMaybe { x: 0.0, y: 0.0 } $ Array.index points i
                  p2 = fromMaybe { x: 0.0, y: 0.0 } $ Array.index points (i + 1)
                  p3 = fromMaybe { x: 0.0, y: 0.0 } $ Array.index points (min (Array.length points - 1) (i + 2))

                  -- Convert Catmull-Rom to Cubic Bezier
                  -- Control point 1 = p1 + (p2 - p0) / 6
                  -- Control point 2 = p2 - (p3 - p1) / 6
                  cp1x = p1.x + (p2.x - p0.x) / 6.0
                  cp1y = p1.y + (p2.y - p0.y) / 6.0
                  cp2x = p2.x - (p3.x - p1.x) / 6.0
                  cp2y = p2.y - (p3.y - p1.y) / 6.0
                in
                  Just $ "C" <> show cp1x <> "," <> show cp1y
                    <> " "
                    <> show cp2x
                    <> ","
                    <> show cp2y
                    <> " "
                    <> show p2.x
                    <> ","
                    <> show p2.y
          )
          points
      in
        Array.intercalate "" (Array.catMaybes segments)
