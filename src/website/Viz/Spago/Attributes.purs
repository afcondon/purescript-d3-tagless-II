module D3.Viz.Spago.Draw.Attributes where

import Prelude

import PSD3.Internal.Attributes.Sugar (classed, cursor, fill, height, opacity, radius, strokeColor, strokeWidth, text, textAnchor, transform', viewBox, width, x, y)
import PSD3.Internal.Selection.Types (SelectionAttribute)
import D3.Viz.Spago.Model (SpagoSimNode)
import D3.Viz.Spago.Files (NodeType(..))
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_, d3SchemeSequential10N_)
import PSD3.Data.Node (NodeID)
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
enterAttrs :: Array (SelectionAttribute SpagoSimNode)
enterAttrs =
  [ classed \(d :: SpagoSimNode) -> nodeClass d
  , transform' \(d :: SpagoSimNode) -> translateNode d
  ]

-- | Attributes for updating existing node groups (reapplied on data updates)
updateAttrs :: Array (SelectionAttribute SpagoSimNode)
updateAttrs =
  [ classed \(d :: SpagoSimNode) -> nodeClass d
  , transform' \(d :: SpagoSimNode) -> translateNode d
  ]

-- | Visual attributes for a scene - split into circle and label attributes
-- | so they can be applied to the appropriate child elements
-- | Tags automatically propagate to CSS classes on node groups
type SpagoSceneAttributes = {
    circles :: Array (SelectionAttribute SpagoSimNode)
  , labels  :: Array (SelectionAttribute SpagoSimNode)
  , tagMap  :: Maybe (Map NodeID (Set String))  -- Optional tag map for automatic CSS class propagation
}

-- | Attributes for the "cluster" scene - emphasizes package groupings with
-- | fill/stroke based on usage and semi-transparent packages
clusterSceneAttributes :: SpagoSceneAttributes
clusterSceneAttributes = {
    circles: [ radius \(d :: SpagoSimNode) -> nodeRadius d
            , fill \(d :: SpagoSimNode) -> fillByUsage d
            , strokeColor \(d :: SpagoSimNode) -> strokeByUsage d
            , strokeWidth 3.0
            , opacity \(d :: SpagoSimNode) -> opacityByType d
          ]
  , labels: [ classed "label"
            , x 0.2
            , y \(d :: SpagoSimNode) -> positionLabel d
            , textAnchor "middle"
            , text \(d :: SpagoSimNode) -> nodeName d
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
  , labels: [ classed "label"
            , x 0.2
            , y \(d :: SpagoSimNode) -> positionLabel d
            , textAnchor "middle"
            , text \(d :: SpagoSimNode) -> nodeName d
          ]
  , tagMap: Nothing
}

-- | Attributes for the "tree" scene - uses depth-based color gradient for fill
-- | and group-based colors for stroke to show hierarchy
treeSceneAttributes :: SpagoSceneAttributes
treeSceneAttributes = {
    circles: [ radius \(d :: SpagoSimNode) -> nodeRadius d
            , fill \(d :: SpagoSimNode) -> colorByDepth d
            , strokeColor \(d :: SpagoSimNode) -> colorByGroup d
            , strokeWidth 3.0
            ]
  , labels: [ classed "label"
            , x 4.0
            , y 2.0
            , text \(d :: SpagoSimNode) -> nodeName d
            ]
  , tagMap: Nothing
}

svgAttrs :: forall d. Number -> Number -> Array (SelectionAttribute d)
svgAttrs w h = [ viewBox (-w / 2.1) (-h / 2.05) w h
                    , classed "overlay"
                    , width w, height h
                    , cursor "grab"
]
