module D3.Viz.Spago.Draw.Attributes where

import Prelude

import D3.Viz.Spago.Model (SpagoSimNode)
import D3.Viz.Spago.Files (NodeType(..))
import PSD3.Data.Node (NodeID)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_, d3SchemeSequential10N_)
import PSD3v2.Attribute.Types (Attribute, class_, fill, height, opacity, radius, stroke, strokeWidth, textContent, textAnchor, transform, viewBox, width, x, y)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Nullable (toMaybe)
import Data.Int (toNumber)
import Data.Number (sqrt) as Number

-- Helper functions for node attributes (phantom type friendly)
nodeRadius :: SpagoSimNode -> Number
nodeRadius d = d.r

nodeName :: SpagoSimNode -> String
nodeName d = d.name

nodeClass :: SpagoSimNode -> String
nodeClass d = show d.nodetype <> " " <> d.containerName <> " " <> d.name <> (if d.connected then " connected" else "")

translateNode :: SpagoSimNode -> String
translateNode d = "translate(" <> show d.x <> "," <> show d.y <> ")"

positionLabel :: SpagoSimNode -> Number
positionLabel d = case d.nodetype of
  (IsModule _)  -> negate d.r  -- position below center
  (IsPackage _) -> 0.0  -- position at center

fillByUsage :: SpagoSimNode -> String
fillByUsage d = if d.connected then colorByGroup d else "none"

strokeByUsage :: SpagoSimNode -> String
strokeByUsage d = if d.connected then "none" else colorByGroup d

colorByGroup :: SpagoSimNode -> String
colorByGroup d = d3SchemeCategory10N_ (toNumber d.cluster)

colorByDepth :: SpagoSimNode -> String
colorByDepth d = case toMaybe d.treeDepth of
  Nothing -> "none"
  Just depth -> d3SchemeSequential10N_ (toNumber depth)

opacityByType :: SpagoSimNode -> Number
opacityByType d = case d.nodetype of
  (IsPackage _) -> 0.4
  (IsModule _) -> 0.7

-- | Attributes for entering node groups (applied when new nodes are added to DOM)
enterAttrs :: Array (Attribute SpagoSimNode)
enterAttrs =
  [ class_ \(d :: SpagoSimNode) -> nodeClass d
  , transform \(d :: SpagoSimNode) -> translateNode d
  ]

-- | Attributes for updating existing node groups (reapplied on data updates)
updateAttrs :: Array (Attribute SpagoSimNode)
updateAttrs =
  [ class_ \(d :: SpagoSimNode) -> nodeClass d
  , transform \(d :: SpagoSimNode) -> translateNode d
  ]

-- | Visual attributes for a scene - split into circle and label attributes
-- | so they can be applied to the appropriate child elements
-- | Tags automatically propagate to CSS classes on node groups
type SpagoSceneAttributes = {
    circles :: Array (Attribute SpagoSimNode)
  , labels  :: Array (Attribute SpagoSimNode)
  , tagMap  :: Maybe (Map NodeID (Set String))  -- Optional tag map for automatic CSS class propagation
}

-- | Attributes for the "cluster" scene - emphasizes package groupings with
-- | fill/stroke based on usage and semi-transparent packages
clusterSceneAttributes :: SpagoSceneAttributes
clusterSceneAttributes = {
    circles: [ radius \(d :: SpagoSimNode) -> nodeRadius d
            , fill \(d :: SpagoSimNode) -> fillByUsage d
            , stroke \(d :: SpagoSimNode) -> strokeByUsage d
            , strokeWidth 3.0
            , opacity \(d :: SpagoSimNode) -> opacityByType d
          ]
  , labels: [ class_ "label"
            , x 0.2
            , y \(d :: SpagoSimNode) -> positionLabel d
            , textAnchor "middle"
            , textContent \(d :: SpagoSimNode) -> nodeName d
          ]
  , tagMap: Nothing
}

-- | Attributes for the "graph" scene - colors nodes by group/package with
-- | opacity differentiating packages from modules
graphSceneAttributes :: SpagoSceneAttributes
graphSceneAttributes = {
    circles: [ radius \(d :: SpagoSimNode) -> nodeRadius d
            , fill \(d :: SpagoSimNode) -> colorByGroup d
            , opacity \(d :: SpagoSimNode) -> opacityByType d
           ]
  , labels: [ class_ "label"
            , x 0.2
            , y \(d :: SpagoSimNode) -> positionLabel d
            , textAnchor "middle"
            , textContent \(d :: SpagoSimNode) -> nodeName d
          ]
  , tagMap: Nothing
}

-- | Attributes for the "tree" scene - uses depth-based color gradient for fill
-- | and group-based colors for stroke to show hierarchy
treeSceneAttributes :: SpagoSceneAttributes
treeSceneAttributes = {
    circles: [ radius \(d :: SpagoSimNode) -> nodeRadius d
            , fill \(d :: SpagoSimNode) -> colorByDepth d
            , stroke \(d :: SpagoSimNode) -> colorByGroup d
            , strokeWidth 3.0
            ]
  , labels: [ class_ "label"
            , x 4.0
            , y 2.0
            , textContent\(d :: SpagoSimNode) -> nodeName d
            ]
  , tagMap: Nothing
}

svgAttrs :: forall d. Number -> Number -> Array (Attribute d)
svgAttrs w h = [ viewBox (show (-w / 2.1) <> " " <> show (-h / 2.05) <> " " <> show w <> " " <> show h)
                    , class_ "overlay"
                    , width w, height h
]
