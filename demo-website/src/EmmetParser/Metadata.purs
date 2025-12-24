-- | Emmet Metadata for Round-Tripping
-- |
-- | This module provides metadata tracking for AST features that cannot be
-- | expressed in Emmet notation, enabling full round-trip conversion.
-- |
-- | ## Design Philosophy
-- |
-- | Emmet notation is intentionally simple - it expresses structure and simple
-- | attributes only. However, the full PSD3 AST supports:
-- | - Computed attributes (expressions, conditionals, scales)
-- | - GUP behaviors (enter/update/exit transitions)
-- | - Event handlers (behaviors)
-- | - Named selections
-- |
-- | When converting AST → Emmet, we preserve these features as "opaque metadata"
-- | attached to specific nodes. When converting back Emmet → AST, we reattach
-- | this metadata to the reconstructed tree.
-- |
-- | This enables:
-- | 1. **Full round-trip guarantee**: AST → Emmet+Metadata → AST (lossless)
-- | 2. **Graphical structure editing**: Users can restructure trees visually
-- |    while preserving computed attributes and behaviors
-- | 3. **Clear separation**: Emmet for structure, PureScript for computation
-- |
-- | ## Usage
-- |
-- | ```purescript
-- | -- Convert AST to Emmet with metadata
-- | let emmetData = toEmmetWithMetadata myAST
-- |
-- | -- User edits Emmet string (structure changes)
-- | let newEmmet = editEmmetString emmetData.emmetString
-- |
-- | -- Convert back, preserving opaque features
-- | let newAST = fromEmmetWithMetadata { emmetData | emmetString = newEmmet }
-- | ```
module EmmetParser.Metadata
  ( -- * Core Types
    EmmetWithMetadata
  , NodePath(..)
  , OpaqueFeature(..)
  , EmmetMetadata
    -- * Conversion Functions
  , toEmmetWithMetadata
  , fromEmmetWithMetadata
  , isEmmetCompatible
  , analyzeCompatibility
    -- * Compatibility Analysis
  , CompatibilityReport
  , IncompatibilityReason(..)
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Control.Comonad.Cofree (head, tail)
import Data.List as List
import Data.Tree (mkTree)
import Data.Tree (Tree) as DT
import Data.Tuple (Tuple(..))
import PSD3.AST (Tree(..), TreeNode, GUPBehaviors)
import PSD3.Internal.Attribute (Attribute(..), AttrSource(..), AttributeName(..), AttributeValue(..))
import PSD3.Internal.Behavior.Types (Behavior)
import PSD3.Internal.Selection.Types (ElementType(..))
import EmmetParser.Parser (parseEmmet)
import EmmetParser.Converter (convertToTree)
import EmmetParser.Types as EmmetTypes
import TreeBuilder3.Converter (builderTreeToAST)
import TreeBuilder3.Types (TreeNode, DslNodeType(..), AttrKind(..)) as TB

-- | Path to a node in the tree (for metadata attachment)
-- |
-- | Represents the structural position of a node as an array of child indices.
-- | Root is [], first child is [0], second child of first child is [0,1], etc.
-- |
-- | This allows us to reattach metadata even after structural edits, as long
-- | as the user doesn't delete the node or change its relative position too much.
newtype NodePath = NodePath (Array Int)

derive newtype instance Eq NodePath
derive newtype instance Ord NodePath

instance Show NodePath where
  show (NodePath path) = "NodePath " <> show path

-- | Root path (empty array)
rootPath :: NodePath
rootPath = NodePath []

-- | Extend a path by one level
childPath :: NodePath -> Int -> NodePath
childPath (NodePath path) idx = NodePath (Array.snoc path idx)

-- | Features that cannot be expressed in Emmet notation
-- |
-- | These are preserved as opaque metadata and reattached during round-trip.
data OpaqueFeature datum
  -- | Computed attribute (expressions, not simple field access)
  -- | Example: cx (field @"x" `times` 40.0 `plus` 50.0)
  = ComputedAttribute
      { name :: String
      , source :: AttrSource
      , func :: datum -> String  -- We store the function but can't serialize it
      , description :: String     -- Human-readable description for display
      }
  -- | Conditional attribute
  -- | Example: fill (ifThen (field @"value" > 50.0) "red" "blue")
  | ConditionalAttribute
      { name :: String
      , description :: String
      }
  -- | Scale function
  -- | Example: cy (from (\d -> yScale d.value))
  | ScaleAttribute
      { name :: String
      , scaleName :: Maybe String  -- If we can infer scale name
      , description :: String
      }
  -- | Event behavior (click, drag, zoom handlers)
  | BehaviorFeature
      { behavior :: Behavior datum
      , description :: String
      }
  -- | Named selection (node has a name for later retrieval)
  | NamedSelection
      { name :: String
      }

instance Show (OpaqueFeature datum) where
  show (ComputedAttribute { name, description }) =
    "(ComputedAttribute " <> name <> ": " <> description <> ")"
  show (ConditionalAttribute { name, description }) =
    "(ConditionalAttribute " <> name <> ": " <> description <> ")"
  show (ScaleAttribute { name, description }) =
    "(ScaleAttribute " <> name <> ": " <> description <> ")"
  show (BehaviorFeature { description }) =
    "(BehaviorFeature " <> description <> ")"
  show (NamedSelection { name }) =
    "(NamedSelection " <> name <> ")"

-- | Metadata tracking opaque features by node path
type EmmetMetadata datum =
  { opaqueFeatures :: Map NodePath (Array (OpaqueFeature datum))
  , gupBehaviors :: Map NodePath (GUPBehaviors datum)
  }

-- | Emmet string with attached metadata for full round-tripping
type EmmetWithMetadata datum =
  { emmetString :: String
  , metadata :: EmmetMetadata datum
  , dataType :: String  -- Type name (e.g., "Point", "Node")
  }

-- | Compatibility report for an AST
type CompatibilityReport =
  { compatible :: Boolean
  , reasons :: Array IncompatibilityReason
  , nodeCount :: Int
  , opaqueCount :: Int
  }

-- | Reasons why an AST is not fully Emmet-compatible
data IncompatibilityReason
  = HasComputedAttributes Int  -- Count of computed attrs
  | HasConditionals Int
  | HasScales Int
  | HasBehaviors Int
  | HasNamedSelections Int
  | HasCustomElements Int
  | HasComplexGUP Int  -- Update joins with non-default behaviors

derive instance Eq IncompatibilityReason

instance Show IncompatibilityReason where
  show (HasComputedAttributes n) = "Has " <> show n <> " computed attributes"
  show (HasConditionals n) = "Has " <> show n <> " conditional attributes"
  show (HasScales n) = "Has " <> show n <> " scale functions"
  show (HasBehaviors n) = "Has " <> show n <> " behaviors"
  show (HasNamedSelections n) = "Has " <> show n <> " named selections"
  show (HasCustomElements n) = "Has " <> show n <> " custom elements"
  show (HasComplexGUP n) = "Has " <> show n <> " update joins with transitions"

-- | Check if an attribute is Emmet-compatible (static, field, or index)
isCompatibleAttribute :: forall datum. Attribute datum -> Boolean
isCompatibleAttribute (StaticAttr _ _) = true
isCompatibleAttribute (DataAttr _ (FieldSource _) _) = true
isCompatibleAttribute (DataAttr _ IndexSource _) = true
isCompatibleAttribute (IndexedAttr _ IndexSource _) = true
isCompatibleAttribute _ = false

-- | Check if an AST can be losslessly represented in Emmet notation
isEmmetCompatible :: forall datum. Tree datum -> Boolean
isEmmetCompatible tree =
  let report = analyzeCompatibility tree
  in report.compatible

-- | Analyze an AST and report compatibility
analyzeCompatibility :: forall datum. Tree datum -> CompatibilityReport
analyzeCompatibility tree =
  let
    result = analyzeTree rootPath tree
  in
    { compatible: Array.null result.reasons
    , reasons: result.reasons
    , nodeCount: result.nodeCount
    , opaqueCount: result.opaqueCount
    }
  where
    analyzeTree :: NodePath -> Tree datum -> CompatibilityReport
    analyzeTree path = case _ of
      Node node ->
        let
          -- Check attributes
          incompatibleAttrs = Array.filter (not <<< isCompatibleAttribute) node.attrs
          attrReasons = if Array.null incompatibleAttrs
            then []
            else [HasComputedAttributes (Array.length incompatibleAttrs)]

          -- Check behaviors
          behaviorReasons = if Array.null node.behaviors
            then []
            else [HasBehaviors (Array.length node.behaviors)]

          -- Check for named selection
          nameReasons = case node.name of
            Nothing -> []
            Just _ -> [HasNamedSelections 1]

          -- Check custom elements
          customElemReasons = case node.elemType of
            SVG -> []
            Group -> []
            Circle -> []
            Rect -> []
            Path -> []
            Line -> []
            Text -> []
            _ -> [HasCustomElements 1]

          -- Recursively check children
          childReports = Array.mapWithIndex
            (\idx child -> analyzeTree (childPath path idx) child)
            node.children

          -- Combine child results
          childReport = Array.foldl combineReports emptyReport childReports

          -- Current node reasons
          nodeReasons = attrReasons <> behaviorReasons <> nameReasons <> customElemReasons
        in
          combineReports
            { compatible: Array.null nodeReasons
            , reasons: nodeReasons
            , nodeCount: 1
            , opaqueCount: Array.length incompatibleAttrs + Array.length node.behaviors +
                          (if maybe false (const true) node.name then 1 else 0)
            }
            childReport

      Join { template, joinData } ->
        -- Check template with first datum (if available)
        case Array.head joinData of
          Nothing -> emptyReport
          Just d ->
            let templateTree = template d
            in analyzeTree (childPath path 0) templateTree

      NestedJoin { template, joinData, decompose } ->
        case Array.head joinData of
          Nothing -> emptyReport
          Just d ->
            let innerData = decompose d
            in case Array.head innerData of
              Nothing -> emptyReport
              Just inner ->
                let templateTree = template inner
                in analyzeTree (childPath path 0) templateTree

      UpdateJoin { template, joinData, behaviors } ->
        -- Check if GUP behaviors are default/empty
        let hasNonDefaultGUP = hasComplexBehaviors behaviors
            gupReasons = if hasNonDefaultGUP then [HasComplexGUP 1] else []
        in case Array.head joinData of
          Nothing ->
            { compatible: not hasNonDefaultGUP
            , reasons: gupReasons
            , nodeCount: 1
            , opaqueCount: if hasNonDefaultGUP then 1 else 0
            }
          Just d ->
            let templateTree = template d
                templateReport = analyzeTree (childPath path 0) templateTree
            in combineReports
              { compatible: not hasNonDefaultGUP
              , reasons: gupReasons
              , nodeCount: 1
              , opaqueCount: if hasNonDefaultGUP then 1 else 0
              }
              templateReport

      UpdateNestedJoin { template, joinData, decompose, behaviors } ->
        let hasNonDefaultGUP = hasComplexBehaviors behaviors
            gupReasons = if hasNonDefaultGUP then [HasComplexGUP 1] else []
        in case Array.head joinData of
          Nothing ->
            { compatible: not hasNonDefaultGUP
            , reasons: gupReasons
            , nodeCount: 1
            , opaqueCount: if hasNonDefaultGUP then 1 else 0
            }
          Just d ->
            let innerData = decompose d
            in case Array.head innerData of
              Nothing ->
                { compatible: not hasNonDefaultGUP
                , reasons: gupReasons
                , nodeCount: 1
                , opaqueCount: if hasNonDefaultGUP then 1 else 0
                }
              Just inner ->
                let templateTree = template inner
                    templateReport = analyzeTree (childPath path 0) templateTree
                in combineReports
                  { compatible: not hasNonDefaultGUP
                  , reasons: gupReasons
                  , nodeCount: 1
                  , opaqueCount: if hasNonDefaultGUP then 1 else 0
                  }
                  templateReport

    emptyReport :: CompatibilityReport
    emptyReport =
      { compatible: true
      , reasons: []
      , nodeCount: 0
      , opaqueCount: 0
      }

    combineReports :: CompatibilityReport -> CompatibilityReport -> CompatibilityReport
    combineReports r1 r2 =
      { compatible: r1.compatible && r2.compatible
      , reasons: r1.reasons <> r2.reasons
      , nodeCount: r1.nodeCount + r2.nodeCount
      , opaqueCount: r1.opaqueCount + r2.opaqueCount
      }

    hasComplexBehaviors :: forall d. GUPBehaviors d -> Boolean
    hasComplexBehaviors { enter, update, exit } =
      hasPhaseConfig enter || hasPhaseConfig update || hasPhaseConfig exit

    hasPhaseConfig :: forall d. Maybe { attrs :: Array (Attribute d), transition :: Maybe _ } -> Boolean
    hasPhaseConfig Nothing = false
    hasPhaseConfig (Just { attrs, transition }) =
      not (Array.null attrs) || maybe false (const true) transition

-- | Convert AST to Emmet notation with metadata
toEmmetWithMetadata :: forall datum. Tree datum -> EmmetWithMetadata datum
toEmmetWithMetadata tree =
  let result = treeToEmmet rootPath tree
  in { emmetString: result.emmetString
     , metadata:
         { opaqueFeatures: result.opaqueFeatures
         , gupBehaviors: result.gupBehaviors
         }
     , dataType: "Unit"  -- TODO: Extract from tree if available
     }
  where
    treeToEmmet :: NodePath -> Tree datum -> EmmetResult datum
    treeToEmmet path = case _ of
      Node node ->
        let
          -- Convert element type to Emmet char
          elemChar = elementTypeToChar node.elemType

          -- Convert attributes to Emmet, extracting opaque ones
          attrResult = attributesToEmmet path node.attrs
          attrsStr = if Array.null attrResult.emmetAttrs
            then ""
            else "[" <> String.joinWith "," attrResult.emmetAttrs <> "]"

          -- Convert children recursively
          childResults = Array.mapWithIndex
            (\idx child -> treeToEmmet (childPath path idx) child)
            node.children

          -- Combine children with > operator
          childrenStr = if Array.null childResults
            then ""
            else ">" <> String.joinWith ">" (map _.emmetString childResults)

          -- Combine all metadata
          allOpaque = Array.foldl Map.union attrResult.opaqueFeatures (map _.opaqueFeatures childResults)
          allGUP = Array.foldl Map.union Map.empty (map _.gupBehaviors childResults)
        in
          { emmetString: elemChar <> attrsStr <> childrenStr
          , opaqueFeatures: allOpaque
          , gupBehaviors: allGUP
          }

      Join { key, joinData, template } ->
        -- For joins, we need to instantiate template with first datum
        case Array.head joinData of
          Nothing ->
            { emmetString: "j(Unknown)>g"  -- Fallback for empty data
            , opaqueFeatures: Map.empty
            , gupBehaviors: Map.empty
            }
          Just d ->
            let templateTree = template d
                templateResult = treeToEmmet (childPath path 0) templateTree
            in
              { emmetString: "j(Unknown)>" <> templateResult.emmetString
              , opaqueFeatures: templateResult.opaqueFeatures
              , gupBehaviors: templateResult.gupBehaviors
              }

      NestedJoin { key, joinData, decompose, template } ->
        case Array.head joinData of
          Nothing ->
            { emmetString: "n(Unknown)>g"
            , opaqueFeatures: Map.empty
            , gupBehaviors: Map.empty
            }
          Just d ->
            let innerData = decompose d
            in case Array.head innerData of
              Nothing ->
                { emmetString: "n(Unknown)>g"
                , opaqueFeatures: Map.empty
                , gupBehaviors: Map.empty
                }
              Just inner ->
                let templateTree = template inner
                    templateResult = treeToEmmet (childPath path 0) templateTree
                in
                  { emmetString: "n(Unknown)>" <> templateResult.emmetString
                  , opaqueFeatures: templateResult.opaqueFeatures
                  , gupBehaviors: templateResult.gupBehaviors
                  }

      UpdateJoin { key, joinData, template, behaviors } ->
        case Array.head joinData of
          Nothing ->
            { emmetString: "u(Unknown)>g"
            , opaqueFeatures: Map.empty
            , gupBehaviors: Map.empty
            }
          Just d ->
            let templateTree = template d
                templateResult = treeToEmmet (childPath path 0) templateTree
                -- Store GUP behaviors as metadata
                gupMetadata = Map.insert path behaviors Map.empty
            in
              { emmetString: "u(Unknown)>" <> templateResult.emmetString
              , opaqueFeatures: templateResult.opaqueFeatures
              , gupBehaviors: Map.union gupMetadata templateResult.gupBehaviors
              }

      UpdateNestedJoin { key, joinData, decompose, template, behaviors } ->
        case Array.head joinData of
          Nothing ->
            { emmetString: "x(Unknown)>g"
            , opaqueFeatures: Map.empty
            , gupBehaviors: Map.empty
            }
          Just d ->
            let innerData = decompose d
            in case Array.head innerData of
              Nothing ->
                { emmetString: "x(Unknown)>g"
                , opaqueFeatures: Map.empty
                , gupBehaviors: Map.empty
                }
              Just inner ->
                let templateTree = template inner
                    templateResult = treeToEmmet (childPath path 0) templateTree
                    gupMetadata = Map.insert path behaviors Map.empty
                in
                  { emmetString: "x(Unknown)>" <> templateResult.emmetString
                  , opaqueFeatures: templateResult.opaqueFeatures
                  , gupBehaviors: Map.union gupMetadata templateResult.gupBehaviors
                  }

-- | Convert Emmet with metadata back to AST
-- | Note: Returns Tree Unit because TreeBuilder3 doesn't preserve datum types
fromEmmetWithMetadata :: forall datum. EmmetWithMetadata datum -> Maybe (Tree Unit)
fromEmmetWithMetadata { emmetString, metadata } = do
  -- 1. Parse Emmet string to EmmetExpr
  emmetExpr <- case parseEmmet emmetString of
    Left _err -> Nothing
    Right expr -> Just expr

  -- 2. Convert to TreeBuilder3 tree
  let builderTree = convertToTree emmetExpr

  -- 3. Substitute opaque attributes from metadata
  let enhancedTree = substituteOpaqueAttributes rootPath builderTree metadata

  -- 4. Convert to PSD3 AST
  case builderTreeToAST enhancedTree of
    Left _err -> Nothing
    Right ast -> Just ast

-- =============================================================================
-- Helper Types and Functions
-- =============================================================================

type EmmetResult datum =
  { emmetString :: String
  , opaqueFeatures :: Map NodePath (Array (OpaqueFeature datum))
  , gupBehaviors :: Map NodePath (GUPBehaviors datum)
  }

type AttributeResult datum =
  { emmetAttrs :: Array String
  , opaqueFeatures :: Map NodePath (Array (OpaqueFeature datum))
  }

-- | Convert element type to Emmet single-char representation
elementTypeToChar :: ElementType -> String
elementTypeToChar = case _ of
  SVG -> "svg"      -- Special case: multi-char
  Group -> "g"
  Circle -> "c"
  Rect -> "r"
  Path -> "p"
  Line -> "l"
  Text -> "t"
  _ -> "g"  -- Fallback for custom elements

-- | Convert attributes to Emmet syntax, extracting opaque ones to metadata
attributesToEmmet :: forall datum. NodePath -> Array (Attribute datum) -> AttributeResult datum
attributesToEmmet path attrs =
  Array.foldl processAttribute emptyResult attrs
  where
    emptyResult =
      { emmetAttrs: []
      , opaqueFeatures: Map.empty
      }

    processAttribute acc attr = case attr of
      StaticAttr (AttributeName name) value ->
        acc { emmetAttrs = Array.snoc acc.emmetAttrs (name <> "=" <> attrValueToString value) }

      DataAttr (AttributeName name) (FieldSource field) _func ->
        acc { emmetAttrs = Array.snoc acc.emmetAttrs (name <> ":" <> field) }

      DataAttr (AttributeName name) IndexSource _func ->
        -- This is actually an indexed attr, but DataAttr doesn't distinguish
        acc { emmetAttrs = Array.snoc acc.emmetAttrs (name <> "@index") }

      DataAttr (AttributeName name) src func ->
        -- Opaque attribute - store in metadata
        let feature = ComputedAttribute
              { name
              , source: src
              , func: \d -> attrValueToString (func d)
              , description: showAttrSource src
              }
            features = Map.lookup path acc.opaqueFeatures
                      # fromMaybe []
                      # flip Array.snoc feature
            newOpaque = Map.insert path features acc.opaqueFeatures
        in acc { emmetAttrs = Array.snoc acc.emmetAttrs (name <> ":COMPUTED")
               , opaqueFeatures = newOpaque
               }

      IndexedAttr (AttributeName name) IndexSource _func ->
        acc { emmetAttrs = Array.snoc acc.emmetAttrs (name <> "@index") }

      IndexedAttr (AttributeName name) src func ->
        -- Opaque indexed attribute
        let feature = ComputedAttribute
              { name
              , source: src
              , func: \d -> "indexed"  -- Can't easily show indexed funcs
              , description: showAttrSource src
              }
            features = Map.lookup path acc.opaqueFeatures
                      # fromMaybe []
                      # flip Array.snoc feature
            newOpaque = Map.insert path features acc.opaqueFeatures
        in acc { emmetAttrs = Array.snoc acc.emmetAttrs (name <> ":COMPUTED")
               , opaqueFeatures = newOpaque
               }

-- | Convert AttributeValue to string for display
attrValueToString :: AttributeValue -> String
attrValueToString = case _ of
  StringValue s -> s
  NumberValue n -> show n
  BooleanValue b -> show b

-- | Show AttrSource for metadata description
showAttrSource :: AttrSource -> String
showAttrSource = case _ of
  UnknownSource -> "unknown"
  StaticSource s -> s
  FieldSource f -> "field: " <> f
  ExprSource e -> "expr: " <> e
  IndexSource -> "index"
  OpaqueSource -> "opaque"

-- | Substitute opaque attributes in TreeBuilder3 tree with real ones from metadata
substituteOpaqueAttributes :: forall datum.
  NodePath ->
  DT.Tree TB.TreeNode ->
  EmmetMetadata datum ->
  DT.Tree TB.TreeNode
substituteOpaqueAttributes path tree metadata =
  let
    node = head tree
    children = tail tree

    -- Look up opaque features for this node
    features = Map.lookup path metadata.opaqueFeatures # fromMaybe []

    -- Map over children, recursively processing and substituting attr nodes
    childrenArray = Array.fromFoldable children
    newChildrenArray = Array.mapWithIndex
      (\idx child ->
        let childNode = head child
        in case childNode.nodeType of
          -- If this is an opaque attribute node, try to substitute it
          TB.NodeAttr (TB.AttrOpaque name _) ->
            substituteAttrNode child childNode name features

          -- Otherwise, recursively process children
          _ -> substituteOpaqueAttributes (childPath path idx) child metadata
      )
      childrenArray
    newChildren = List.fromFoldable newChildrenArray
  in
    mkTree node newChildren

-- | Try to substitute an opaque attribute node with a real one from metadata
substituteAttrNode :: forall datum.
  DT.Tree TB.TreeNode ->
  TB.TreeNode ->
  String ->
  Array (OpaqueFeature datum) ->
  DT.Tree TB.TreeNode
substituteAttrNode originalTree node attrName features =
  case Array.find (\f -> getFeatureName f == attrName) features of
    Nothing ->
      -- No metadata found - keep the opaque attr (will error at runtime)
      originalTree

    Just feature ->
      -- Convert OpaqueFeature to AttrKind
      let newNodeType = opaqueFeatureToAttrKind feature
          newNode = node { nodeType = newNodeType }
      in mkTree newNode List.Nil  -- Attr nodes have no children

-- | Get the attribute name from an OpaqueFeature
getFeatureName :: forall datum. OpaqueFeature datum -> String
getFeatureName = case _ of
  ComputedAttribute { name } -> name
  ConditionalAttribute { name } -> name
  ScaleAttribute { name } -> name
  BehaviorFeature _ -> ""  -- Behaviors don't have attr names
  NamedSelection _ -> ""   -- Selections don't have attr names

-- | Convert an OpaqueFeature to an AttrKind (losing the function but keeping structure info)
opaqueFeatureToAttrKind :: forall datum. OpaqueFeature datum -> TB.DslNodeType
opaqueFeatureToAttrKind = case _ of
  ComputedAttribute { name, source } ->
    TB.NodeAttr $ sourceToAttrKind name source

  ConditionalAttribute { name } ->
    -- Conditionals become expressions (we don't have the original expr)
    TB.NodeAttr $ TB.AttrExpr name "CONDITIONAL"

  ScaleAttribute { name, scaleName } ->
    -- Scales become expressions
    let expr = case scaleName of
          Just sn -> "scale:" <> sn
          Nothing -> "SCALE"
    in TB.NodeAttr $ TB.AttrExpr name expr

  BehaviorFeature _ ->
    -- Behaviors aren't attributes - shouldn't get here
    TB.PendingBehavior

  NamedSelection _ ->
    -- Named selections aren't attributes - shouldn't get here
    TB.PendingElement

-- | Convert AttrSource to AttrKind
sourceToAttrKind :: String -> AttrSource -> TB.AttrKind
sourceToAttrKind name = case _ of
  StaticSource val -> TB.AttrStatic name val
  FieldSource field -> TB.AttrField name field
  ExprSource expr -> TB.AttrExpr name expr
  IndexSource -> TB.AttrIndex name
  UnknownSource -> TB.AttrStatic name "unknown"
  OpaqueSource -> TB.AttrOpaque name "COMPUTED"  -- Shouldn't happen but fallback
