module D3.Viz.TreeAPI.ChordDiagram where

-- | Chord diagram visualization using TreeAPI
-- | Based on Baton Rouge bridge traffic analysis example
-- | Inspired by: https://www.streetlightdata.com/planning-bridges-louisiana/

import Prelude

import Data.Array (length, mapWithIndex, index, (!!), (..))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Number (pi, cos, sin)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Shared.Data (loadBridgesData)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PSD3.Internal.FFI (arcGenerator_, arcPath_, chordArray_, chordGroups_, chordLayoutWithPadAngle_, ribbonGenerator_, ribbonPath_, setArcInnerRadius_, setArcOuterRadius_, setRibbonRadius_)
import PSD3.Internal.Types (Datum_)
import PSD3v2.Attribute.Types (class_, d, fill, fillOpacity, fontSize, height, id_, stroke, strokeWidth, textAnchor, textContent, transform, viewBox, width, x, y)
import PSD3v2.Capabilities.Selection (renderTree, select)
import PSD3v2.Interpreter.D3v2 (D3v2Selection_, reselectD3v2, runD3v2M)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)

-- | Traffic flow matrix (11 regions of Baton Rouge)
type TrafficMatrix = Array (Array Number)

-- | Region labels for Baton Rouge traffic analysis
type RegionLabels = Array String

-- | Baton Rouge regions
batonRougeRegions :: RegionLabels
batonRougeRegions =
  [ "WBR"                    -- West Baton Rouge
  , "Pointe Coupee"
  , "Iberville"
  , "EBR - M"                -- East Baton Rouge - Main
  , "EBR - S"                -- East Baton Rouge - South
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
  [ Tuple "WBR" "#d62728"              -- Red (dominant in image)
  , Tuple "Pointe Coupee" "#ff9896"    -- Light red
  , Tuple "Point Coupe" "#ff9896"      -- Light red (alternate spelling)
  , Tuple "Iberville" "#e7969c"        -- Pink red
  , Tuple "EBR-M" "#5ab4ac"            -- Teal
  , Tuple "EBR - M" "#5ab4ac"          -- Teal (with spaces)
  , Tuple "EBR-S" "#c7eae5"            -- Light teal
  , Tuple "EBR - S" "#c7eae5"          -- Light teal (with spaces)
  , Tuple "Ascension" "#80cdc1"        -- Medium teal
  , Tuple "Livingston" "#d8daeb"       -- Light purple
  , Tuple "I-10 Lafayette" "#b2abd2"   -- Purple
  , Tuple "US-190 Opelousas" "#ff7f0e" -- Orange
  , Tuple "US 190 Opelousas" "#ff7f0e" -- Orange (alternate)
  , Tuple "I-10 NOLA" "#ffbb78"        -- Light orange
  , Tuple "I-12" "#98df8a"             -- Light green
  ]

-- | Get color for a region by name
getRegionColor :: RegionLabels -> Int -> String
getRegionColor labels idx = case index labels idx of
  Just label -> case Map.lookup label regionColorMap of
    Just color -> color
    Nothing -> "#999999"  -- Default gray for unknown regions
  Nothing -> "#999999"

-- | Sample traffic matrix (placeholder - to be filled with estimated values)
-- | TODO: Fill with estimated values from the image
batonRougeTraffic :: TrafficMatrix
batonRougeTraffic =
  [ [  0.0, 10.0,  5.0, 80.0, 20.0, 15.0,  8.0, 12.0,  6.0,  4.0,  3.0 ]  -- WBR
  , [ 10.0,  0.0,  3.0, 15.0,  8.0,  5.0,  2.0,  4.0,  1.0,  1.0,  1.0 ]  -- Pointe Coupee
  , [  5.0,  3.0,  0.0, 20.0, 12.0,  8.0,  4.0,  3.0,  2.0,  1.0,  1.0 ]  -- Iberville
  , [ 80.0, 15.0, 20.0,  0.0, 40.0, 30.0, 25.0, 18.0, 15.0, 10.0,  8.0 ]  -- EBR - M (main hub)
  , [ 20.0,  8.0, 12.0, 40.0,  0.0, 35.0, 20.0, 15.0, 10.0,  8.0,  6.0 ]  -- EBR - S
  , [ 15.0,  5.0,  8.0, 30.0, 35.0,  0.0, 18.0, 12.0,  8.0,  6.0,  4.0 ]  -- Ascension
  , [  8.0,  2.0,  4.0, 25.0, 20.0, 18.0,  0.0, 10.0,  6.0,  4.0,  3.0 ]  -- Livingston
  , [ 12.0,  4.0,  3.0, 18.0, 15.0, 12.0, 10.0,  0.0,  5.0,  3.0,  2.0 ]  -- I-10 Lafayette
  , [  6.0,  1.0,  2.0, 15.0, 10.0,  8.0,  6.0,  5.0,  0.0,  2.0,  1.0 ]  -- US 190
  , [  4.0,  1.0,  1.0, 10.0,  8.0,  6.0,  4.0,  3.0,  2.0,  0.0,  1.0 ]  -- I-10 NOLA
  , [  3.0,  1.0,  1.0,  8.0,  6.0,  4.0,  3.0,  2.0,  1.0,  1.0,  0.0 ]  -- I-12
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
  let sourceVal = getSourceValue d
      targetVal = getTargetValue d
  in if sourceVal >= targetVal
     then getSourceIndex d
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

-- | Draw Chord diagram with TreeAPI
drawChord ::
  TrafficMatrix ->
  RegionLabels ->
  String ->
  Number ->
  Number ->
  Effect Unit
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
  let arcGen = setArcOuterRadius_
                 (setArcInnerRadius_ (arcGenerator_ unit) innerR)
                 outerR

  -- Declarative tree structure
  let chordTree :: T.Tree Unit
      chordTree =
        T.named SVG "svg"
          [ width w
          , height h
          , viewBox ("0 0 " <> show w <> " " <> show h)
          , id_ "chord-svg"
          , class_ "chord-diagram"
          ]
          `T.withChild`
            (T.named Group "centerGroup"
              [ transform ("translate(" <> show centerX <> "," <> show centerY <> ")")
              , class_ "chord-group"
              ]
              `T.withChildren`
                [ T.named Group "ribbonsGroup" [ class_ "ribbons" ]
                , T.named Group "arcsGroup" [ class_ "arcs" ]
                , T.named Group "labelsGroup" [ class_ "labels" ]
                ])

  -- Render structure
  selections <- renderTree container chordTree

  -- Reselect groups
  ribbonsGroupSel <- liftEffect $ reselectD3v2 "ribbonsGroup" selections
  arcsGroupSel <- liftEffect $ reselectD3v2 "arcsGroup" selections
  labelsGroupSel <- liftEffect $ reselectD3v2 "labelsGroup" selections

  -- Render ribbons (colored by dominant flow direction)
  let ribbonsTree :: T.Tree IndexedRibbon
      ribbonsTree =
        T.joinData "ribbonElements" "path" indexedChords $ \(IndexedRibbon ir) ->
          let dominantIdx = getDominantFlowIndex ir.datum
              color = getRegionColor labels dominantIdx
          in T.elem Path
            [ class_ "ribbon"
            , d (ribbonPath_ ribbonGen ir.datum)
            , fill color
            , fillOpacity 0.67
            , stroke "#000000"
            , strokeWidth 0.5
            ]

  _ <- renderTree ribbonsGroupSel ribbonsTree

  -- Render arcs (outer segments)
  let arcsTree :: T.Tree IndexedArc
      arcsTree =
        T.joinData "arcElements" "path" indexedGroups $ \(IndexedArc ia) ->
          let idx = getGroupIndex ia.datum
              color = getRegionColor labels idx
          in T.elem Path
            [ class_ "arc"
            , d (arcPath_ arcGen ia.datum)
            , fill color
            , stroke "#ffffff"
            , strokeWidth 2.0
            ]

  _ <- renderTree arcsGroupSel arcsTree

  -- Render labels (positioned around the circle)
  let labelsTree :: T.Tree IndexedArc
      labelsTree =
        T.joinData "labelElements" "text" indexedGroups $ \(IndexedArc ia) ->
          let idx = getGroupIndex ia.datum
              label = fromMaybe "" (labels !! idx)
              -- Calculate angle for label positioning
              groupData = unsafeCoerce ia.datum :: { startAngle :: Number, endAngle :: Number }
              angle = (groupData.startAngle + groupData.endAngle) / 2.0
              labelR = outerR + 8.0  -- Close to the arc
              labelX = labelR * cos (angle - pi / 2.0)
              labelY = labelR * sin (angle - pi / 2.0)
              -- Anchor based on which side of the circle
              anchor = if angle > pi then "end" else "start"
          in T.elem Text
            [ class_ "chord-label"
            , x labelX
            , y labelY
            , textAnchor anchor
            , fill "#000"
            , textContent label
            , fontSize 10.0  -- Smaller font size
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
