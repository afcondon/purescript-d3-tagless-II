-- | Tree Builder Interpreter
-- |
-- | Interprets a BuilderTree with sample data to render a live preview.
-- | Uses direct D3 manipulation for simplicity.
module TreeBuilder.Interpreter
  ( renderPreview
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String as String
import Effect (Effect)
import TreeBuilder.Types (BuilderTree(..), BuilderNode, SampleDatum, AttributeChoice(..), AttributeBinding)

-- =============================================================================
-- FFI for D3 rendering
-- =============================================================================

foreign import renderPreviewFFI :: String -> Array RenderInstruction -> Effect Unit

-- =============================================================================
-- Render Instructions (simple AST for FFI)
-- =============================================================================

-- | A simplified instruction for the FFI (using newtype to avoid cycle)
newtype RenderInstruction = RenderInstruction
  { elemType :: String
  , attrs :: Array { name :: String, value :: String }
  , children :: Array RenderInstruction
  }


-- =============================================================================
-- Preview Rendering
-- =============================================================================

-- | Render a preview of the builder tree into a container
renderPreview :: String -> BuilderTree -> Array SampleDatum -> Effect Unit
renderPreview containerSelector tree sampleData = do
  let instructions = treeToInstructions tree sampleData
  renderPreviewFFI containerSelector instructions

-- | Convert a BuilderTree to render instructions
treeToInstructions :: BuilderTree -> Array SampleDatum -> Array RenderInstruction
treeToInstructions tree sampleData = case tree of
  BNode node children ->
    -- For non-join nodes, render once using first datum (or defaults)
    let datum = fromMaybe defaultDatum (Array.head sampleData)
        childInstructions = Array.concat (map (\c -> treeToInstructions c sampleData) children)
    in [ nodeToInstruction node datum childInstructions ]

  BDataJoin join ->
    -- For joins, render template once per datum
    map (\d -> nodeToInstruction join.template d []) sampleData

-- | Convert a BuilderNode to a render instruction
nodeToInstruction :: BuilderNode -> SampleDatum -> Array RenderInstruction -> RenderInstruction
nodeToInstruction node datum children =
  RenderInstruction
    { elemType: node.elementType
    , attrs: map (bindingToAttr datum) node.attributes
    , children
    }

-- | Convert an attribute binding to name/value pair
bindingToAttr :: SampleDatum -> AttributeBinding -> { name :: String, value :: String }
bindingToAttr datum binding =
  { name: binding.attrName
  , value: resolveAttributeChoice binding.choice datum
  }

-- | Resolve an AttributeChoice to a string value
resolveAttributeChoice :: AttributeChoice -> SampleDatum -> String
resolveAttributeChoice choice datum = case choice of
  FromField field -> getFieldValue field datum
  ConstantNumber n -> show n
  ConstantString s -> s
  IndexBased -> show datum.index
  Computed expr -> evalSimpleExpr expr datum

-- | Get a field value from a SampleDatum as a string
getFieldValue :: String -> SampleDatum -> String
getFieldValue field datum = case field of
  "x" -> show datum.x
  "y" -> show datum.y
  "cx" -> show datum.cx
  "cy" -> show datum.cy
  "rx" -> show datum.rx
  "ry" -> show datum.ry
  "sx" -> show datum.sx
  "sy" -> show datum.sy
  "radius" -> show datum.radius
  "width" -> show datum.width
  "height" -> show datum.height
  "color" -> datum.color
  "label" -> datum.label
  "name" -> datum.name
  "value" -> show datum.value
  "index" -> show datum.index
  _ -> ""

-- | Default datum for when array is empty
defaultDatum :: SampleDatum
defaultDatum =
  { x: 0.0
  , y: 0.0
  , cx: 25.0
  , cy: 21.0
  , rx: 0.0
  , ry: 0.0
  , sx: 0.0
  , sy: 0.0
  , radius: 10.0
  , width: 50.0
  , height: 30.0
  , color: "#999"
  , label: ""
  , name: ""
  , value: 0.0
  , index: 0
  }

-- | Evaluate a simple expression like "d.x + 40.0"
-- | Supports: d.field, d.field + N, d.field - N, d.field * N, d.field / N
-- | Also: d.field1 + d.field2, d.field1 * d.field2
evalSimpleExpr :: String -> SampleDatum -> String
evalSimpleExpr expr datum =
  -- Handle "d.field1 + d.field2" pattern (field + field)
  case parseFieldAddField expr of
    Just { field1, field2 } ->
      show (getNumericField field1 datum + getNumericField field2 datum)
    Nothing ->
      -- Handle "d.field + N" pattern
      case parseAddExpr expr of
        Just { field, offset } ->
          show (getNumericField field datum + offset)
        Nothing ->
          -- Handle "d.field - N" pattern
          case parseSubExpr expr of
            Just { field, offset } ->
              show (getNumericField field datum - offset)
            Nothing ->
              -- Handle "d.field * N" or "d.field1 * d.field2"
              case parseMulExpr expr of
                Just result -> show result
                Nothing ->
                  -- Handle "d.field / N"
                  case parseDivExpr expr of
                    Just { field, divisor } ->
                      show (getNumericField field datum / divisor)
                    Nothing ->
                      -- Just a field reference
                      case parseFieldRef expr of
                        Just field -> getFieldValue field datum
                        Nothing -> expr
  where
  -- Parse "d.field1 + d.field2"
  parseFieldAddField :: String -> Maybe { field1 :: String, field2 :: String }
  parseFieldAddField s =
    let parts = String.split (String.Pattern " + ") s
    in case Array.length parts of
      2 -> do
        part1 <- Array.head parts
        part2 <- Array.last parts
        field1 <- String.stripPrefix (String.Pattern "d.") part1
        field2 <- String.stripPrefix (String.Pattern "d.") part2
        Just { field1, field2 }
      _ -> Nothing

  -- Parse "d.field + N"
  parseAddExpr :: String -> Maybe { field :: String, offset :: Number }
  parseAddExpr s =
    let parts = String.split (String.Pattern " + ") s
    in case Array.length parts of
      2 -> do
        fieldPart <- Array.head parts
        offsetPart <- Array.last parts
        field <- String.stripPrefix (String.Pattern "d.") fieldPart
        offset <- Number.fromString offsetPart
        Just { field, offset }
      _ -> Nothing

  -- Parse "d.field - N"
  parseSubExpr :: String -> Maybe { field :: String, offset :: Number }
  parseSubExpr s =
    let parts = String.split (String.Pattern " - ") s
    in case Array.length parts of
      2 -> do
        fieldPart <- Array.head parts
        offsetPart <- Array.last parts
        field <- String.stripPrefix (String.Pattern "d.") fieldPart
        offset <- Number.fromString offsetPart
        Just { field, offset }
      _ -> Nothing

  -- Parse "d.field * N" or "d.field1 * d.field2"
  parseMulExpr :: String -> Maybe Number
  parseMulExpr s =
    let parts = String.split (String.Pattern " * ") s
    in case Array.length parts of
      2 -> do
        part1 <- Array.head parts
        part2 <- Array.last parts
        field1 <- String.stripPrefix (String.Pattern "d.") part1
        -- Try as field * number first
        case Number.fromString part2 of
          Just n -> Just (getNumericField field1 datum * n)
          Nothing -> do
            -- Try as field * field
            field2 <- String.stripPrefix (String.Pattern "d.") part2
            Just (getNumericField field1 datum * getNumericField field2 datum)
      _ -> Nothing

  -- Parse "d.field / N"
  parseDivExpr :: String -> Maybe { field :: String, divisor :: Number }
  parseDivExpr s =
    let parts = String.split (String.Pattern " / ") s
    in case Array.length parts of
      2 -> do
        fieldPart <- Array.head parts
        divisorPart <- Array.last parts
        field <- String.stripPrefix (String.Pattern "d.") fieldPart
        divisor <- Number.fromString divisorPart
        Just { field, divisor }
      _ -> Nothing

  -- Parse "d.field"
  parseFieldRef :: String -> Maybe String
  parseFieldRef s = String.stripPrefix (String.Pattern "d.") s

-- | Get a numeric field value from a SampleDatum
getNumericField :: String -> SampleDatum -> Number
getNumericField field datum = case field of
  "x" -> datum.x
  "y" -> datum.y
  "cx" -> datum.cx
  "cy" -> datum.cy
  "rx" -> datum.rx
  "ry" -> datum.ry
  "sx" -> datum.sx
  "sy" -> datum.sy
  "radius" -> datum.radius
  "width" -> datum.width
  "height" -> datum.height
  "value" -> datum.value
  "index" -> toNumber datum.index
  _ -> 0.0
