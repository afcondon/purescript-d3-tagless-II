-- | BubblePackData - Load and transform declarations into pack hierarchies
-- |
-- | Creates a 3-level hierarchy for bubble pack visualization:
-- | Level 1: Module (outer circle)
-- | Level 2: Category (value, data, typeSynonym, typeClass, externData)
-- | Level 3: Individual declarations
module Component.CodeExplorerV2.BubblePackData where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import Data.Array (filter, groupBy, head, length, sortWith)
import Data.Array.NonEmpty (toArray)
import Data.Either (hush)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import PSD3.Layout.Hierarchy.Pack (HierarchyData(..), PackConfig, PackNode(..), hierarchy, pack)

-- | Raw declaration from JSON
type Declaration =
  { title :: String
  , kind :: String
  }

-- | Module with its declarations
type ModuleDeclarations =
  { name :: String
  , declarations :: Array Declaration
  }

-- | Data for pack visualization - what we attach to each circle
data PackDatum
  = ModuleNode String           -- Module name
  | CategoryNode String         -- Category name (value, data, etc.)
  | DeclarationNode String      -- Declaration title

derive instance Eq PackDatum

instance Show PackDatum where
  show (ModuleNode name) = "Module: " <> name
  show (CategoryNode name) = "Category: " <> name
  show (DeclarationNode name) = "Decl: " <> name

-- | FFI to parse declarations JSON
foreign import parseDeclarationsJSON_ :: String -> Array ModuleDeclarations

-- | Load declarations from JSON file
loadDeclarations :: Aff (Maybe (Array ModuleDeclarations))
loadDeclarations = do
  let datadir = "./data/spago-data/"
  response <- AJAX.get ResponseFormat.string $ datadir <> "declarations.json"
  pure $ hush response <#> \body -> parseDeclarationsJSON_ body.body

-- | Filter to only my-project modules
filterMyProject :: Array ModuleDeclarations -> Array ModuleDeclarations
filterMyProject = filter isMyProject
  where
    -- My-project modules have paths starting with "src/"
    -- Their names don't have a package prefix like "Data.Maybe"
    -- For now, filter by checking if it's in our known namespaces
    isMyProject m =
      contains (Pattern "PSD3") m.name ||
      contains (Pattern "Component") m.name ||
      contains (Pattern "D3.Viz") m.name ||
      contains (Pattern "Utility") m.name ||
      contains (Pattern "HTML") m.name

-- | Group declarations by kind
groupByKind :: Array Declaration -> Array { kind :: String, decls :: Array Declaration }
groupByKind decls =
  let
    sorted = sortWith _.kind decls
    grouped = groupBy (\a b -> a.kind == b.kind) sorted
  in
    map (\group ->
      let arr = toArray group
          kind = case head arr of
            Just x -> x.kind
            Nothing -> "unknown"
      in { kind, decls: arr }
    ) grouped

-- | Build pack hierarchy for a single module
moduleToHierarchy :: ModuleDeclarations -> HierarchyData PackDatum
moduleToHierarchy mod =
  let
    -- Group declarations by kind
    byKind = groupByKind mod.declarations

    -- Create category nodes with declaration children
    categoryNodes = map (\{ kind, decls } ->
      HierarchyData
        { data_: CategoryNode kind
        , value: Nothing  -- Will sum from children
        , children: Just $ map (\d ->
            HierarchyData
              { data_: DeclarationNode d.title
              , value: Just 1.0  -- Each declaration has value 1
              , children: Nothing
              }
          ) decls
        }
    ) byKind
  in
    HierarchyData
      { data_: ModuleNode mod.name
      , value: Nothing  -- Will sum from children
      , children: Just categoryNodes
      }

-- | Pack config for module bubble packs
modulePackConfig :: PackConfig PackDatum
modulePackConfig =
  { size: { width: 100.0, height: 100.0 }  -- Will be scaled based on module size
  , padding: 2.0
  , radius: Nothing  -- Use sqrt of value
  }

-- | Create packed circle for a module
packModule :: ModuleDeclarations -> PackNode PackDatum
packModule mod =
  let
    hier = moduleToHierarchy mod
    tree = hierarchy hier
  in
    pack modulePackConfig tree

-- | Result: array of packed modules ready for force simulation
type PackedModule =
  { name :: String
  , declCount :: Int
  , packed :: PackNode PackDatum
  }

-- | Load and pack all my-project modules
loadAndPackModules :: Aff (Maybe (Array PackedModule))
loadAndPackModules = do
  maybeDecls <- loadDeclarations
  pure $ maybeDecls <#> \allDecls ->
    let
      myProject = filterMyProject allDecls
    in
      map (\mod ->
        { name: mod.name
        , declCount: length mod.declarations
        , packed: packModule mod
        }
      ) myProject

-- | Get module names to IDs mapping for linking with existing graph
moduleNameToId :: Array PackedModule -> Map.Map String Int
moduleNameToId modules =
  Map.fromFoldable $ mapWithIndex (\i m -> Tuple m.name i) modules
  where
    mapWithIndex f arr =
      let indexed = foldl (\acc x -> acc <> [Tuple (length acc) x]) [] arr
      in map (\(Tuple i x) -> f i x) indexed

-- | Circle data for rendering (flattened from pack hierarchy)
type RenderCircle =
  { x :: Number      -- Position relative to module center
  , y :: Number
  , r :: Number      -- Radius
  , depth :: Int     -- 0 = module, 1 = category, 2 = declaration
  , label :: String  -- Name/title for tooltip
  }

-- | Get the root radius from a packed module
getPackedRadius :: PackedModule -> Number
getPackedRadius pm = case pm.packed of
  PackNode node -> node.r

-- | Flatten PackNode into renderable circles
-- | Returns circles with positions relative to the root center
flattenPackNode :: PackNode PackDatum -> Array RenderCircle
flattenPackNode root =
  let
    -- Root position for offsetting
    PackNode rootNode = root
    rootX = rootNode.x
    rootY = rootNode.y

    go :: Number -> Number -> PackNode PackDatum -> Array RenderCircle
    go offsetX offsetY (PackNode node) =
      let
        -- This circle's absolute position (relative to root)
        x = node.x - rootX + offsetX
        y = node.y - rootY + offsetY

        -- Get label from datum
        label = case node.data_ of
          ModuleNode name -> name
          CategoryNode name -> name
          DeclarationNode name -> name

        -- This circle
        thisCircle =
          { x
          , y
          , r: node.r
          , depth: node.depth
          , label
          }

        -- Recursively flatten children
        childCircles = node.children >>= go x y
      in
        [thisCircle] <> childCircles
  in
    go 0.0 0.0 root

-- | Get all circles for a packed module, centered at origin
getModuleCircles :: PackedModule -> Array RenderCircle
getModuleCircles pm = flattenPackNode pm.packed
