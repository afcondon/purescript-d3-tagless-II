-- | Scene Configurations
-- |
-- | Declarative definitions for each scene using NodeRule system.
-- |
-- | Each scene has three phases:
-- | 1. initRules: Applied before transition starts
-- | 2. layout: Where DumbEngine moves nodes
-- | 3. finalRules: Applied after transition completes
module Engine.Scenes
  ( -- Form scenes (transition to positions, then Static)
    gridFormScene
  , orbitFormScene
  , treeFormScene
  -- Run scenes (enable physics/forces)
  , gridRunScene
  , orbitRunScene
  , treeRunScene
  -- Rule helpers
  , isPackage
  , isModule
  , isTreeModule
  , isNonTreeModule
  , pinAtCurrent
  , pinAtOrbit
  , unpinNode
  , setGridXYToPackageOrbit
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber, ceil)
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin, sqrt)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Engine.Scene (SceneConfig, EngineMode(..), NodeRule, PositionMap)
import Types (SimNode, NodeType(..))
import Viz.SpagoGridTest.GridLayout as GridLayout
import Viz.SpagoGridTest.OrbitLayout as OrbitLayout

-- =============================================================================
-- Node Predicates (Selectors)
-- =============================================================================

isPackage :: SimNode -> Boolean
isPackage n = n.nodeType == PackageNode

isModule :: SimNode -> Boolean
isModule n = n.nodeType == ModuleNode

-- | Is this module part of the spanning tree (reachable from root)?
isTreeModule :: SimNode -> Boolean
isTreeModule n = n.nodeType == ModuleNode && n.isInTree

-- | Is this module NOT part of the spanning tree?
isNonTreeModule :: SimNode -> Boolean
isNonTreeModule n = n.nodeType == ModuleNode && not n.isInTree

-- =============================================================================
-- Node Transforms
-- =============================================================================

-- | Pin a node at its current position
pinAtCurrent :: SimNode -> SimNode
pinAtCurrent n = n
  { fx = Nullable.notNull n.x
  , fy = Nullable.notNull n.y
  }

-- | Pin a node at its orbit position
pinAtOrbit :: SimNode -> SimNode
pinAtOrbit n =
  let px = cos n.orbitAngle * OrbitLayout.orbitRadius
      py = sin n.orbitAngle * OrbitLayout.orbitRadius
  in n
    { fx = Nullable.notNull px
    , fy = Nullable.notNull py
    , gridX = px
    , gridY = py
    }

-- | Unpin a node (clear fx/fy)
unpinNode :: SimNode -> SimNode
unpinNode n = n
  { fx = Nullable.null
  , fy = Nullable.null
  }

-- | Set gridX/gridY to parent package's orbit position
-- | (Requires packageOrbitMap to be built externally)
setGridXYToPackageOrbit :: Object.Object { x :: Number, y :: Number } -> SimNode -> SimNode
setGridXYToPackageOrbit pkgMap n =
  case Object.lookup (show n.cluster) pkgMap of
    Just { x: px, y: py } -> n { gridX = px, gridY = py }
    Nothing -> n

-- =============================================================================
-- Common Rule Sets
-- =============================================================================

-- | Standard init: pin all nodes at current positions
pinAllRule :: NodeRule SimNode
pinAllRule =
  { name: "pinAll"
  , select: const true
  , apply: pinAtCurrent
  }

-- =============================================================================
-- Grid Scenes (Form + Run)
-- =============================================================================

-- | Form Grid: Transition to grid positions, then stop (Static)
gridFormScene :: SceneConfig SimNode
gridFormScene =
  { name: "GridForm"
  , initRules: [ pinAllRule ]
  , layout: GridLayout.calculateGridPositions
  , finalRules: gridFormFinalRules
  , stableMode: Static
  , cssTransition: Nothing
  }

-- | Grid Run: Enable physics with grid forces
gridRunScene :: SceneConfig SimNode
gridRunScene =
  { name: "GridRun"
  , initRules: [ pinAllRule ]
  , layout: GridLayout.calculateGridPositions
  , finalRules: gridRunFinalRules
  , stableMode: Physics {}
  , cssTransition: Nothing
  }

-- | Form Grid final rules - pin everything at grid positions
gridFormFinalRules :: Array SimNode -> Array (NodeRule SimNode)
gridFormFinalRules nodes =
  let calc = gridCalc nodes
  in
    [ { name: "pinPackagesAtGrid"
      , select: isPackage
      , apply: \n ->
          let pos = calc.pkgGridPos n.id
          in n { fx = Nullable.notNull pos.x
               , fy = Nullable.notNull pos.y
               , gridX = pos.x
               , gridY = pos.y
               }
      }
    , { name: "pinModulesAtGrid"
      , select: isModule
      , apply: \n ->
          let pos = calc.pkgGridPos n.cluster
          in n { fx = Nullable.notNull pos.x
               , fy = Nullable.notNull pos.y
               , gridX = pos.x
               , gridY = pos.y
               }
      }
    ]

-- | Grid Run final rules - pin packages, unpin modules (forces will spread them)
gridRunFinalRules :: Array SimNode -> Array (NodeRule SimNode)
gridRunFinalRules nodes =
  let calc = gridCalc nodes
  in
    [ { name: "pinPackagesAtGrid"
      , select: isPackage
      , apply: \n ->
          let pos = calc.pkgGridPos n.id
          in n { fx = Nullable.notNull pos.x
               , fy = Nullable.notNull pos.y
               , gridX = pos.x
               , gridY = pos.y
               }
      }
    , { name: "unpinModulesSetGrid"
      , select: isModule
      , apply: \n ->
          let pos = calc.pkgGridPos n.cluster
          in n { fx = Nullable.null
               , fy = Nullable.null
               , gridX = pos.x
               , gridY = pos.y
               }
      }
    ]

-- | Grid calculation helper
gridCalc :: Array SimNode -> { pkgGridPos :: Int -> { x :: Number, y :: Number } }
gridCalc nodes =
  let
    packageCount = Array.length $ Array.filter isPackage nodes
    aspect = GridLayout.viewBoxWidth / GridLayout.viewBoxHeight
    gridCols = ceil (sqrt (toNumber packageCount * aspect))
    gridRows = ceil (toNumber packageCount / toNumber gridCols)
    spacingX = GridLayout.viewBoxWidth * 0.8 / toNumber gridCols
    spacingY = GridLayout.viewBoxHeight * 0.8 / toNumber gridRows
    gridColsN = toNumber gridCols
    gridRowsN = toNumber gridRows
  in
    { pkgGridPos: \pkgId ->
        let row = pkgId / gridCols
            col = pkgId `mod` gridCols
            gx = (toNumber col - gridColsN / 2.0 + 0.5) * spacingX
            gy = (toNumber row - gridRowsN / 2.0 + 0.5) * spacingY
        in { x: gx, y: gy }
    }

-- =============================================================================
-- Orbit Scenes (Form + Run)
-- =============================================================================

-- | Form Orbit: Transition to orbit positions, then stop (Static)
orbitFormScene :: SceneConfig SimNode
orbitFormScene =
  { name: "OrbitForm"
  , initRules: [ pinAllRule ]
  , layout: orbitLayout
  , finalRules: orbitFormFinalRules
  , stableMode: Static
  , cssTransition: Nothing
  }

-- | Orbit Run: Enable physics with orbit forces
orbitRunScene :: SceneConfig SimNode
orbitRunScene =
  { name: "OrbitRun"
  , initRules: [ pinAllRule ]
  , layout: orbitLayout
  , finalRules: orbitRunFinalRules
  , stableMode: Physics {}
  , cssTransition: Nothing
  }

-- | Layout: All nodes to their package's orbit position
orbitLayout :: Array SimNode -> PositionMap
orbitLayout nodes =
  let
    packages = Array.filter isPackage nodes
    packagePositions = Object.fromFoldable $ map getPackageOrbitPos packages

    getPackageOrbitPos pkg =
      let px = cos pkg.orbitAngle * OrbitLayout.orbitRadius
          py = sin pkg.orbitAngle * OrbitLayout.orbitRadius
      in Tuple (show pkg.id) { x: px, y: py }

    getNodeOrbitPos pkgMap node = case node.nodeType of
      PackageNode ->
        let px = cos node.orbitAngle * OrbitLayout.orbitRadius
            py = sin node.orbitAngle * OrbitLayout.orbitRadius
        in Tuple (show node.id) { x: px, y: py }
      ModuleNode ->
        case Object.lookup (show node.cluster) pkgMap of
          Just pos -> Tuple (show node.id) pos
          Nothing -> Tuple (show node.id) { x: node.x, y: node.y }
  in
    Object.fromFoldable $ map (getNodeOrbitPos packagePositions) nodes

-- | Form Orbit final rules - pin everything at orbit positions
orbitFormFinalRules :: Array SimNode -> Array (NodeRule SimNode)
orbitFormFinalRules nodes =
  let
    packages = Array.filter isPackage nodes
    pkgOrbitMap = Object.fromFoldable $ map getPackageOrbitPos packages

    getPackageOrbitPos pkg =
      let px = cos pkg.orbitAngle * OrbitLayout.orbitRadius
          py = sin pkg.orbitAngle * OrbitLayout.orbitRadius
      in Tuple (show pkg.id) { x: px, y: py }
  in
    [ { name: "pinPackagesAtOrbit"
      , select: isPackage
      , apply: \n ->
          let px = cos n.orbitAngle * OrbitLayout.orbitRadius
              py = sin n.orbitAngle * OrbitLayout.orbitRadius
          in n { fx = Nullable.notNull px
               , fy = Nullable.notNull py
               , gridX = px
               , gridY = py
               }
      }
    , { name: "pinModulesAtOrbit"
      , select: isModule
      , apply: \n ->
          case Object.lookup (show n.cluster) pkgOrbitMap of
            Just { x: px, y: py } -> n
              { fx = Nullable.notNull px
              , fy = Nullable.notNull py
              , gridX = px
              , gridY = py
              }
            Nothing -> pinAtCurrent n
      }
    ]

-- | Orbit Run final rules - pin packages, unpin modules (forces will spread them)
orbitRunFinalRules :: Array SimNode -> Array (NodeRule SimNode)
orbitRunFinalRules nodes =
  let
    packages = Array.filter isPackage nodes
    pkgOrbitMap = Object.fromFoldable $ map getPackageOrbitPos packages

    getPackageOrbitPos pkg =
      let px = cos pkg.orbitAngle * OrbitLayout.orbitRadius
          py = sin pkg.orbitAngle * OrbitLayout.orbitRadius
      in Tuple (show pkg.id) { x: px, y: py }
  in
    [ { name: "pinPackagesAtOrbit"
      , select: isPackage
      , apply: pinAtOrbit
      }
    , { name: "unpinModulesSetGridToOrbit"
      , select: isModule
      , apply: \n ->
          case Object.lookup (show n.cluster) pkgOrbitMap of
            Just { x: px, y: py } -> n
              { fx = Nullable.null
              , fy = Nullable.null
              , gridX = px
              , gridY = py
              }
            Nothing -> unpinNode n
      }
    ]

-- =============================================================================
-- Tree Scenes (Form + Run)
-- =============================================================================

-- | Form Tree: Transition to tree positions, then stop (Static)
-- | Tree modules go to tree positions, packages/non-tree stay where they are
treeFormScene :: SceneConfig SimNode
treeFormScene =
  { name: "TreeForm"
  , initRules: [ pinAllRule ]
  , layout: treeFormLayout
  , finalRules: \_ ->
      [ { name: "pinTreeModulesAtTree"
        , select: isTreeModule
        , apply: \n -> n
            { fx = Nullable.notNull n.treeX
            , fy = Nullable.notNull n.treeY
            }
        }
      , { name: "pinOthersAtCurrent"
        , select: \n -> not (isTreeModule n)
        , apply: pinAtCurrent
        }
      ]
  , stableMode: Static
  , cssTransition: Nothing
  }

-- | Tree Run: Force-directed tree with link forces
-- | Tree modules are unpinned and connected by link forces
treeRunScene :: SceneConfig SimNode
treeRunScene =
  { name: "TreeRun"
  , initRules: [ pinAllRule ]
  , layout: treeRunLayout
  , finalRules: \_ ->
      [ { name: "unpinTreeModules"
        , select: isTreeModule
        , apply: unpinNode
        }
      , { name: "pinOthersAtCurrent"
        , select: \n -> not (isTreeModule n)
        , apply: pinAtCurrent
        }
      ]
  , stableMode: Physics {}
  , cssTransition: Nothing
  }

-- | Layout for Form Tree: tree modules to tree positions, others stay put
treeFormLayout :: Array SimNode -> PositionMap
treeFormLayout nodes =
  Object.fromFoldable $ map getPosition nodes
  where
  getPosition node =
    if isTreeModule node
    then Tuple (show node.id) { x: node.treeX, y: node.treeY }
    else Tuple (show node.id) { x: node.x, y: node.y }

-- | Layout for Tree Run: keep current positions (forces will arrange)
treeRunLayout :: Array SimNode -> PositionMap
treeRunLayout nodes =
  Object.fromFoldable $ map (\n -> Tuple (show n.id) { x: n.x, y: n.y }) nodes
