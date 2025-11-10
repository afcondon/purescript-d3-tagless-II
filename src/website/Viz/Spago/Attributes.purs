module D3.Viz.Spago.Draw.Attributes where

import PSD3.Internal.Attributes.Sugar (classed, cursor, fill, height, onMouseEvent, opacity, radius, rotate, strokeColor, strokeWidth, text, textAnchor, transform, transform', viewBox, width, x, y)
import PSD3.Data.Tree (TreeLayout(..))
import PSD3.Internal.Types (D3Simulation_, Datum_, MouseEvent(..))
import D3.Viz.Spago.Model (datum_)
import PSD3.Internal.Selection.Types (SelectionAttribute)
import PSD3.Data.Node (NodeID)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Prelude (negate, (/), (<>))

-- | Attributes for entering node groups (applied when new nodes are added to DOM)
enterAttrs :: Array (SelectionAttribute Datum_)
enterAttrs =
  [ classed datum_.nodeClass
  , transform' datum_.translateNode
  ]

-- | Attributes for updating existing node groups (reapplied on data updates)
updateAttrs :: Array (SelectionAttribute Datum_)
updateAttrs =
  [ classed datum_.nodeClass
  , transform' datum_.translateNode
  ]

-- | Visual attributes for a scene - split into circle and label attributes
-- | so they can be applied to the appropriate child elements
-- | Tags automatically propagate to CSS classes on node groups
type SpagoSceneAttributes d = {
    circles :: Array (SelectionAttribute Datum_)
  , labels  :: Array (SelectionAttribute Datum_)
  , tagMap  :: Maybe (Map NodeID (Set String))  -- Optional tag map for automatic CSS class propagation
}

-- | Attributes for the "cluster" scene - emphasizes package groupings with
-- | fill/stroke based on usage and semi-transparent packages
clusterSceneAttributes :: SpagoSceneAttributes Datum_
clusterSceneAttributes = {
    circles: [ radius datum_.radius
            , fill datum_.fillByUsage
            , strokeColor datum_.strokeByUsage
            , strokeWidth 3.0
            , opacity datum_.opacityByType
          ]
  , labels: [ classed "label"
            , x 0.2
            , y datum_.positionLabel
            , textAnchor "middle"
            , text datum_.name
            -- , text datum_.nameAndID
          ]
  , tagMap: Nothing
}

-- | Attributes for the "graph" scene - colors nodes by group/package with
-- | opacity differentiating packages from modules
graphSceneAttributes :: SpagoSceneAttributes Datum_
graphSceneAttributes = {
    circles: [ radius datum_.radius
            , fill datum_.colorByGroup
            , opacity datum_.opacityByType
            -- , callback
           ]
  , labels: [ classed "label"
            , x 0.2
            , y datum_.positionLabel
            , textAnchor "middle"
            , text datum_.name
            -- , text datum_.indexAndID
          ]
  , tagMap: Nothing
}

-- | Attributes for the "tree" scene - uses depth-based color gradient for fill
-- | and group-based colors for stroke to show hierarchy
treeSceneAttributes :: SpagoSceneAttributes Datum_
treeSceneAttributes = {
    circles: [ radius datum_.radius
            , fill datum_.colorByDepth
            , strokeColor datum_.colorByGroup
            , strokeWidth 3.0
            ]
  , labels: [ classed "label"
            , x 4.0
            , y 2.0
            -- the following attribute is suspended until we can revisit the tree_datum_ concept, see if it's really needed etc
            -- , textAnchor (tree_datum_.textAnchor Horizontal)
            , text datum_.name
            ]
  , tagMap: Nothing
}

svgAttrs :: Number -> Number -> Array (SelectionAttribute Datum_)
svgAttrs w h = [ viewBox (-w / 2.1) (-h / 2.05) w h 
                    -- , preserveAspectRatio $ AspectRatio XMid YMid Meet 
                    , classed "overlay"
                    , width w, height h
                    , cursor "grab"
]
