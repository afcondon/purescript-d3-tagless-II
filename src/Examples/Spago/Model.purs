module D3.Examples.Spago.Model where

import D3.Examples.Spago.Files

import D3.Data.Types (Datum_, Index_)
import D3.Node (D3SimulationRow, D3TreeRow, D3_FocusXY, D3_Link(..), D3_SimulationNode(..), EmbeddedData, NodeID, D3_Radius)
import Data.Array (catMaybes, cons, foldl, length, range, zip, (!!), (:))
import Data.Foldable (sum)
import Data.Generic.Rep (Sum)
import Data.Graph (Graph, fromMap)
import Data.Map (toUnfoldable)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, null) as N
import Data.Nullable (notNull)
import Data.Set as S
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Math (sqrt)
import Math as Math
import Prelude (class Eq, class Show, append, bind, mempty, pure, show, ($), (*), (+), (<$>), (<>), (==))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

moduleRadius = 5.0 :: Number 
packageRadius = 50.0 :: Number
packageForceRadius = 50.0 :: Number

-- Model data types specialized with inital data


type SpagoTreeNode    = D3TreeRow       (EmbeddedData SpagoNodeData + ())
type SpagoSimNode     = D3SimulationRow (             SpagoNodeRow  + D3_FocusXY + D3_Radius + ()) -- note we've woven in focusXY so that we can cluster the nodes

type SpagoGraphLinkObj = D3_Link SpagoNodeData SpagoLinkData 

type SpagoModel = { 
    links           :: Array SpagoGraphLinkID  -- each ID will get swizzled for a SpagoGraphLinkObj_ when simulation initialized
  , prunedTreeLinks :: Array SpagoGraphLinkID  -- there are so many of these that we only use them when hovering enabled
  , nodes           :: Array SpagoSimNode      -- already upgraded to simnode as a result of positioning when building the model
  , graph           :: Graph NodeID SpagoNodeData
  , tree            :: Maybe (Tuple NodeID SpagoTreeNode)
  , maps            :: { name2ID    :: M.Map String NodeID
                       , id2Name    :: M.Map NodeID String
                       , id2Node    :: M.Map NodeID SpagoNodeData
                       , id2Package :: M.Map NodeID NodeID
                       , id2LOC     :: M.Map NodeID Number
                       , id2XYLeaf  :: M.Map NodeID { x :: Number, y :: Number, isLeaf :: Boolean }
                       }
}

setXY :: SpagoSimNode -> { x :: Number, y :: Number, isLeaf :: Boolean } -> SpagoSimNode
setXY (D3SimNode node) { x, y, isLeaf: true }  = D3SimNode (node { x = x, y = y, pinned = Floating })
setXY (D3SimNode node) { x, y, isLeaf: false } = D3SimNode (node { fx = notNull x, fy = notNull y, pinned = Pinned })

upgradeSpagoNodeData :: SpagoNodeData -> SpagoSimNode
upgradeSpagoNodeData node = D3SimNode { 
    depends    : node.depends
  , id         : node.id
  , name       : node.name
  , nodetype   : node.nodetype
  , packageID  : node.packageID
  , packageName: node.packageName
  , index      : node.id
  , pinned     : Floating
  , loc        : node.loc
  , cluster    : node.packageID
  , focusX     : 0.0
  , focusY     : 0.0
  , fx         : (N.null :: N.Nullable Number)
  , fy         : (N.null :: N.Nullable Number)
  , vx         : 0.0
  , vy         : 0.0
  , x          : 0.0
  , y          : 0.0
  , r          : sqrt node.loc
  }

datumIsSpagoSimNode :: Datum_ -> SpagoSimNode
datumIsSpagoSimNode = unsafeCoerce

datumIsSpagoLink :: Datum_ -> D3_Link SpagoNodeData SpagoLinkData
datumIsSpagoLink = unsafeCoerce

datumIsGraphNode :: Datum_ -> SpagoSimNode
datumIsGraphNode = unsafeCoerce

getIdFromSpagoSimNode :: Datum_ -> Int
getIdFromSpagoSimNode datum = d.id
  where
    (D3SimNode d) = unsafeCoerce datum

getRadiusFromSpagoSimNode :: Datum_ -> Index_ -> Number
getRadiusFromSpagoSimNode datum i = Math.sqrt d.loc
  where
    (D3SimNode d) = unsafeCoerce datum

getNameFromSpagoSimNode :: Datum_ -> String
getNameFromSpagoSimNode datum = d.name
  where
    (D3SimNode d) = unsafeCoerce datum

convertFilesToGraphModel :: forall r. 
  { body :: String | r } -> 
  { body :: String | r } -> 
  { body :: String | r } -> 
  { body :: String | r } -> SpagoModel
convertFilesToGraphModel moduleJSON packageJSON lsdepJSON locJSON = 
  makeSpagoGraphModel $ readSpago_Raw_JSON_ moduleJSON.body packageJSON.body lsdepJSON.body locJSON.body


makeSpagoGraphModel :: Spago_Raw_JSON_ -> SpagoModel
makeSpagoGraphModel json = do
  let { nodes, links, name2ID, id2Name, id2Node, id2Package, id2LOC } 
        = getGraphJSONData json

  { links
  , nodes    : upgradeSpagoNodeData <$> nodes
  , graph    : makeGraph nodes
  , tree     : Nothing  -- not present in the JSON, has to be calculated, if possible
  , prunedTreeLinks: [] -- not present in the JSON, has to be calculated, if possible
  , maps     : { name2ID
               , id2Name
               , id2Node
               , id2Package
               , id2LOC
               , id2XYLeaf: M.empty
               }
  }

makeGraph :: Array SpagoNodeData -> Graph NodeID SpagoNodeData
makeGraph nodes = do
  let
    graphMap = foldl addNode M.empty nodes
    addNode :: M.Map NodeID (Tuple SpagoNodeData (S.Set NodeID)) -> SpagoNodeData -> M.Map NodeID (Tuple SpagoNodeData (S.Set NodeID))
    addNode acc node = M.insert node.id (Tuple node depends) acc
      where
        depends :: S.Set Int
        depends = S.fromFoldable node.depends.full
  fromMap graphMap



