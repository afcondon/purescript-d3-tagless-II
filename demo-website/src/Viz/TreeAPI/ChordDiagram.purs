module D3.Viz.TreeAPI.ChordDiagram where

-- | Chord diagram visualization using TreeAPI
-- | Based on Baton Rouge bridge traffic analysis example
-- | Inspired by: https://www.streetlightdata.com/planning-bridges-louisiana/
-- | Uses v3 expressions for label positioning with trig functions

import Prelude hiding (add, sub, mul)

import Data.Array (index, length, mapWithIndex, (!!))
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Type.Proxy (Proxy(..))
import PSD3.Shared.Data (loadBridgesData)
import PSD3.Internal.FFI (arcGenerator_, arcPath_, chordArray_, chordGroups_, chordLayoutWithPadAngle_, ribbonGenerator_, ribbonPath_, setArcInnerRadius_, setArcOuterRadius_, setRibbonRadius_)
import PSD3.Internal.Types (Datum_)
import PSD3.Expr.Integration (v3Attr, v3AttrStr)
import PSD3.Expr.Expr (lit, str)
import PSD3.Internal.Capabilities.Selection (renderTree, select)
import PSD3.Interpreter.D3 (D3v2Selection_, reselectD3v2, runD3v2M)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)

-- v3 DSL imports
import PSD3.Expr.Expr (class NumExpr, class BoolExpr, class CompareExpr, class StringExpr, class TrigExpr, ifThenElse, add, sub, mul)
import PSD3.Expr.Expr (pi, cos, sin) as V3
import PSD3.Expr.Datum (class DatumExpr, field)
import PSD3.Expr.Sugar ((/:), (>.), s)
import PSD3.Expr.Interpreter.Eval (EvalD, runEvalD)

-- | Traffic flow matrix (11 regions of Baton Rouge)
type TrafficMatrix = Array (Array Number)

-- | Region labels for Baton Rouge traffic analysis
type RegionLabels = Array String

-- | Baton Rouge regions
batonRougeRegions :: RegionLabels
batonRougeRegions =
  [ "WBR" -- West Baton Rouge
  , "Pointe Coupee"
  , "Iberville"
  , "EBR - M" -- East Baton Rouge - Main
  , "EBR - S" -- East Baton Rouge - South
  , "Ascension"
  , "Livingston"
  , "I-10 Lafayette"
  , "US 190 Opelousas"
  , "I-10 NOLA"
  , "I-12"
  ]

-- | Color palette for regions (color-by-origin like the example)
-- | This is keyed by region name, not index, to handle alphabetical sorting
regionColorMap :: Map.Map String String
regionColorMap = Map.fromFoldable
  [ Tuple "WBR" "#d62728" -- Red (dominant in image)
  , Tuple "Pointe Coupee" "#ff9896" -- Light red
  , Tuple "Point Coupe" "#ff9896" -- Light red (alternate spelling)
  , Tuple "Iberville" "#e7969c" -- Pink red
  , Tuple "EBR-M" "#5ab4ac" -- Teal
  , Tuple "EBR - M" "#5ab4ac" -- Teal (with spaces)
  , Tuple "EBR-S" "#c7eae5" -- Light teal
  , Tuple "EBR - S" "#c7eae5" -- Light teal (with spaces)
  , Tuple "Ascension" "#80cdc1" -- Medium teal
  , Tuple "Livingston" "#d8daeb" -- Light purple
  , Tuple "I-10 Lafayette" "#b2abd2" -- Purple
  , Tuple "US-190 Opelousas" "#ff7f0e" -- Orange
  , Tuple "US 190 Opelousas" "#ff7f0e" -- Orange (alternate)
  , Tuple "I-10 NOLA" "#ffbb78" -- Light orange
  , Tuple "I-12" "#98df8a" -- Light green
  ]

-- | Get color for a region by name
getRegionColor :: RegionLabels -> Int -> String
getRegionColor labels idx = case index labels idx of
  Just label -> case Map.lookup label regionColorMap of
    Just color -> color
    Nothing -> "#999999" -- Default gray for unknown regions
  Nothing -> "#999999"

-- | Sample traffic matrix (values guesstimated from image, original data not available)
batonRougeTraffic :: TrafficMatrix
batonRougeTraffic =
  [ [ 0.0, 10.0, 5.0, 80.0, 20.0, 15.0, 8.0, 12.0, 6.0, 4.0, 3.0 ] -- WBR
  , [ 10.0, 0.0, 3.0, 15.0, 8.0, 5.0, 2.0, 4.0, 1.0, 1.0, 1.0 ] -- Pointe Coupee
  , [ 5.0, 3.0, 0.0, 20.0, 12.0, 8.0, 4.0, 3.0, 2.0, 1.0, 1.0 ] -- Iberville
  , [ 80.0, 15.0, 20.0, 0.0, 40.0, 30.0, 25.0, 18.0, 15.0, 10.0, 8.0 ] -- EBR - M (main hub)
  , [ 20.0, 8.0, 12.0, 40.0, 0.0, 35.0, 20.0, 15.0, 10.0, 8.0, 6.0 ] -- EBR - S
  , [ 15.0, 5.0, 8.0, 30.0, 35.0, 0.0, 18.0, 12.0, 8.0, 6.0, 4.0 ] -- Ascension
  , [ 8.0, 2.0, 4.0, 25.0, 20.0, 18.0, 0.0, 10.0, 6.0, 4.0, 3.0 ] -- Livingston
  , [ 12.0, 4.0, 3.0, 18.0, 15.0, 12.0, 10.0, 0.0, 5.0, 3.0, 2.0 ] -- I-10 Lafayette
  , [ 6.0, 1.0, 2.0, 15.0, 10.0, 8.0, 6.0, 5.0, 0.0, 2.0, 1.0 ] -- US 190
  , [ 4.0, 1.0, 1.0, 10.0, 8.0, 6.0, 4.0, 3.0, 2.0, 0.0, 1.0 ] -- I-10 NOLA
  , [ 3.0, 1.0, 1.0, 8.0, 6.0, 4.0, 3.0, 2.0, 1.0, 1.0, 0.0 ] -- I-12
  ]

-- | Accessor helpers for chord data
getSourceIndex :: Datum_ -> Int
getSourceIndex d = (unsafeCoerce d).source.index

getTargetIndex :: Datum_ -> Int
getTargetIndex d = (unsafeCoerce d).target.index

getSourceValue :: Datum_ -> Number
getSourceValue d = (unsafeCoerce d).source.value

getTargetValue :: Datum_ -> Number
getTargetValue d = (unsafeCoerce d).target.value

getGroupIndex :: Datum_ -> Int
getGroupIndex d = (unsafeCoerce d).index

-- | Get the index of the dominant flow direction (larger value)
-- | This colors ribbons by where the larger flow originates
getDominantFlowIndex :: Datum_ -> Int
getDominantFlowIndex d =
  let
    sourceVal = getSourceValue d
    targetVal = getTargetValue d
  in
    if sourceVal >= targetVal then getSourceIndex d
    else getTargetIndex d

-- | Indexed arc for data join
newtype IndexedArc = IndexedArc { index :: Int, datum :: Datum_ }

instance Eq IndexedArc where
  eq (IndexedArc a) (IndexedArc b) = a.index == b.index

instance Ord IndexedArc where
  compare (IndexedArc a) (IndexedArc b) = compare a.index b.index

-- | Indexed ribbon for data join
newtype IndexedRibbon = IndexedRibbon { index :: Int, datum :: Datum_ }

instance Eq IndexedRibbon where
  eq (IndexedRibbon a) (IndexedRibbon b) = a.index == b.index

instance Ord IndexedRibbon where
  compare (IndexedRibbon a) (IndexedRibbon b) = compare a.index b.index

-- =============================================================================
-- v3 Expressions for Chord Label Positioning
-- =============================================================================

-- | Flattened label data for v3 expression access
-- | Contains the angle (midpoint of arc) and radius for positioning
type ChordLabelDatum =
  { label :: String
  , startAngle :: Number
  , endAngle :: Number
  , labelRadius :: Number  -- outerR + offset
  }

-- Row type for v3 DatumExpr
type ChordLabelRow = (label :: String, startAngle :: Number, endAngle :: Number, labelRadius :: Number)

-- Field accessors
chordStartAngle :: forall repr. DatumExpr repr ChordLabelRow => repr Number
chordStartAngle = field (Proxy :: Proxy "startAngle")

chordEndAngle :: forall repr. DatumExpr repr ChordLabelRow => repr Number
chordEndAngle = field (Proxy :: Proxy "endAngle")

chordLabelRadius :: forall repr. DatumExpr repr ChordLabelRow => repr Number
chordLabelRadius = field (Proxy :: Proxy "labelRadius")

-- | v3 expression: Angle at midpoint of arc
chordMidAngle :: forall repr. NumExpr repr => DatumExpr repr ChordLabelRow => repr Number
chordMidAngle = add chordStartAngle chordEndAngle /: 2.0

-- | v3 expression: Label X position using trig
-- | X = labelRadius * cos(angle - π/2)
-- | The -π/2 rotates from D3's "12 o'clock = 0" to standard "3 o'clock = 0"
chordLabelX :: forall repr. NumExpr repr => TrigExpr repr => DatumExpr repr ChordLabelRow => repr Number
chordLabelX = mul chordLabelRadius (V3.cos (sub chordMidAngle (V3.pi /: 2.0)))

-- | v3 expression: Label Y position using trig
-- | Y = labelRadius * sin(angle - π/2)
chordLabelY :: forall repr. NumExpr repr => TrigExpr repr => DatumExpr repr ChordLabelRow => repr Number
chordLabelY = mul chordLabelRadius (V3.sin (sub chordMidAngle (V3.pi /: 2.0)))

-- | v3 expression: Text anchor based on angle
-- | Right side (angle < π): "start", Left side (angle > π): "end"
chordLabelAnchor :: forall repr. NumExpr repr => BoolExpr repr => CompareExpr repr => StringExpr repr => TrigExpr repr => DatumExpr repr ChordLabelRow => repr String
chordLabelAnchor = ifThenElse
  (chordMidAngle >. V3.pi)
  (s "end")
  (s "start")

-- | Evaluate v3 expressions
evalChordNum :: EvalD ChordLabelDatum Number -> ChordLabelDatum -> Number
evalChordNum expr datum = runEvalD expr datum 0

evalChordStr :: EvalD ChordLabelDatum String -> ChordLabelDatum -> String
evalChordStr expr datum = runEvalD expr datum 0

-- | Convert IndexedArc to ChordLabelDatum for v3 expression evaluation
toChordLabelDatum :: RegionLabels -> Number -> IndexedArc -> ChordLabelDatum
toChordLabelDatum labels outerR (IndexedArc ia) =
  let
    idx = getGroupIndex ia.datum
    label = fromMaybe "" (labels !! idx)
    groupData = unsafeCoerce ia.datum :: { startAngle :: Number, endAngle :: Number }
  in
    { label
    , startAngle: groupData.startAngle
    , endAngle: groupData.endAngle
    , labelRadius: outerR + 8.0  -- Close to the arc
    }

-- | Draw Chord diagram with TreeAPI
drawChord
  :: TrafficMatrix
  -> RegionLabels
  -> String
  -> Number
  -> Number
  -> Effect Unit
drawChord matrix labels containerSelector w h = runD3v2M do
  liftEffect $ Console.log "=== Drawing Chord Diagram with TreeAPI ==="

  -- Select container
  container <- select containerSelector :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Dimensions
  let outerR = 280.0
  let innerR = 270.0
  let centerX = w / 2.0
  let centerY = h / 2.0

  -- Create chord layout with padding between arcs (like the example)
  -- padAngle of 0.05 radians (~3 degrees) creates nice visual separation
  let chordData = chordLayoutWithPadAngle_ matrix 0.05
  let groups = chordGroups_ chordData
  let chords = chordArray_ chordData

  liftEffect $ Console.log $ "Chord layout: " <> show (length groups) <> " groups, " <> show (length chords) <> " chords"

  -- Wrap data with indices
  let indexedGroups = mapWithIndex (\i d -> IndexedArc { index: i, datum: d }) groups
  let indexedChords = mapWithIndex (\i d -> IndexedRibbon { index: i, datum: d }) chords

  -- Create generators
  let ribbonGen = setRibbonRadius_ (ribbonGenerator_ unit) innerR
  let
    arcGen = setArcOuterRadius_
      (setArcInnerRadius_ (arcGenerator_ unit) innerR)
      outerR

  -- Declarative tree structure
  let
    chordTree :: T.Tree Unit
    chordTree =
      T.named SVG "svg"
        [ v3Attr "width" (lit w)
        , v3Attr "height" (lit h)
        , v3AttrStr "viewBox" (str ("0 0 " <> show w <> " " <> show h))
        , v3AttrStr "id" (str "chord-svg")
        , v3AttrStr "class" (str "chord-diagram")
        ]
        `T.withChild`
          ( T.named Group "centerGroup"
              [ v3AttrStr "transform" (str ("translate(" <> show centerX <> "," <> show centerY <> ")"))
              , v3AttrStr "class" (str "chord-group")
              ]
              `T.withChildren`
                [ T.named Group "ribbonsGroup" [ v3AttrStr "class" (str "ribbons") ]
                , T.named Group "arcsGroup" [ v3AttrStr "class" (str "arcs") ]
                , T.named Group "labelsGroup" [ v3AttrStr "class" (str "labels") ]
                ]
          )

  -- Render structure
  selections <- renderTree container chordTree

  -- Reselect groups
  ribbonsGroupSel <- liftEffect $ reselectD3v2 "ribbonsGroup" selections
  arcsGroupSel <- liftEffect $ reselectD3v2 "arcsGroup" selections
  labelsGroupSel <- liftEffect $ reselectD3v2 "labelsGroup" selections

  -- Render ribbons (colored by dominant flow direction)
  let
    ribbonsTree :: T.Tree IndexedRibbon
    ribbonsTree =
      T.joinData "ribbonElements" "path" indexedChords $ \(IndexedRibbon ir) ->
        let
          dominantIdx = getDominantFlowIndex ir.datum
          color = getRegionColor labels dominantIdx
        in
          T.elem Path
            [ v3AttrStr "class" (str "ribbon")
            , v3AttrStr "d" (str (ribbonPath_ ribbonGen ir.datum))
            , v3AttrStr "fill" (str color)
            , v3Attr "fill-opacity" (lit 0.67)
            , v3AttrStr "stroke" (str "#000000")
            , v3Attr "stroke-width" (lit 0.5)
            ]

  _ <- renderTree ribbonsGroupSel ribbonsTree

  -- Render arcs (outer segments)
  let
    arcsTree :: T.Tree IndexedArc
    arcsTree =
      T.joinData "arcElements" "path" indexedGroups $ \(IndexedArc ia) ->
        let
          idx = getGroupIndex ia.datum
          color = getRegionColor labels idx
        in
          T.elem Path
            [ v3AttrStr "class" (str "arc")
            , v3AttrStr "d" (str (arcPath_ arcGen ia.datum))
            , v3AttrStr "fill" (str color)
            , v3AttrStr "stroke" (str "#ffffff")
            , v3Attr "stroke-width" (lit 2.0)
            ]

  _ <- renderTree arcsGroupSel arcsTree

  -- Render labels using v3 expressions for polar positioning
  -- Convert arcs to ChordLabelDatum with labelRadius baked in
  let chordLabelData = map (toChordLabelDatum labels outerR) indexedGroups

  let
    labelsTree :: T.Tree ChordLabelDatum
    labelsTree =
      T.joinData "labelElements" "text" chordLabelData $ \datum ->
        -- v3 expressions with TrigExpr calculate polar coordinates
        T.elem Text
          [ v3AttrStr "class" (str "chord-label")
          , v3Attr "x" (lit (evalChordNum chordLabelX datum))      -- v3: labelRadius * cos(midAngle - π/2)
          , v3Attr "y" (lit (evalChordNum chordLabelY datum))      -- v3: labelRadius * sin(midAngle - π/2)
          , v3AttrStr "text-anchor" (str (evalChordStr chordLabelAnchor datum))  -- v3: if midAngle > π then "end" else "start"
          , v3AttrStr "fill" (str "#000")
          , v3AttrStr "text-content" (str datum.label)
          , v3Attr "font-size" (lit 10.0)
          ]

  _ <- renderTree labelsGroupSel labelsTree

  liftEffect $ Console.log "Chord diagram rendered with TreeAPI"

  pure unit

-- | Entry point with fixed dimensions for Tour page
-- | Loads data from bridges.csv
startChord :: String -> Effect Unit
startChord containerSelector = launchAff_ do
  let w = 700.0
  let h = 700.0

  -- Load bridges data from CSV
  result <- loadBridgesData
  case result of
    Left err -> liftEffect $ Console.log $ "Failed to load bridges data: " <> err
    Right bridgeData -> liftEffect $ drawChord bridgeData.matrix bridgeData.labels containerSelector w h
