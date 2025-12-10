-- | Tree Builder Interpreter
-- |
-- | Interprets a BuilderTree with sample data to render a live preview.
-- | Uses direct D3 manipulation for simplicity.
module TreeBuilder.Interpreter
  ( renderPreview
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
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

unwrapInstruction :: RenderInstruction -> { elemType :: String, attrs :: Array { name :: String, value :: String }, children :: Array RenderInstruction }
unwrapInstruction (RenderInstruction r) = r

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
  Computed _ -> "computed"

-- | Get a field value from a SampleDatum as a string
getFieldValue :: String -> SampleDatum -> String
getFieldValue field datum = case field of
  "x" -> show datum.x
  "y" -> show datum.y
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
  , radius: 10.0
  , width: 50.0
  , height: 30.0
  , color: "#999"
  , label: ""
  , name: ""
  , value: 0.0
  , index: 0
  }
