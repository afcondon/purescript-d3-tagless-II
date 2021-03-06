module D3.Examples.Spago.Draw.Attributes where

import D3.Attributes.Sugar (classed, cursor, fill, height, onMouseEvent, opacity, radius, rotate, strokeColor, strokeWidth, text, textAnchor, transform, transform', viewBox, width, x, y)
import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (D3Simulation_, MouseEvent(..))
import D3.Examples.Spago.Model (datum_)
import D3.Selection (SelectionAttribute)
import Data.Maybe (Maybe)
import Prelude (negate, (/), (<>))

-- TODO this is a problem once extracted from "script", leads to undefined in D3.js
enterLinks :: forall t339. Array t339
enterLinks = [] -- [ classed link_.linkClass ] -- default invisible in CSS unless marked "visible"

-- explodePackageOnClick :: D3Simulation_ -> SelectionAttribute
-- explodePackageOnClick simulation_ = onMouseEvent MouseClick (\e d _ -> explodePackages e simulation_ d) -- here we need to raise a Halogen Event

-- toggleSpotlightOnClick :: D3Simulation_ -> SelectionAttribute
-- toggleSpotlightOnClick simulation_ = onMouseEvent MouseClick (\e d _ -> toggleSpotlight e simulation_ d)

-- unused right now but could be used to make, for example, click on Package or background cause unSpotlighting of node
-- undoSpotlightOnClick :: D3Simulation_ -> SelectionAttribute
-- undoSpotlightOnClick simulation_ = onMouseEvent MouseClick (\e d t -> cancelSpotlight_ simulation_)

enterAttrs :: Array SelectionAttribute
enterAttrs = 
  [ classed datum_.nodeClass
  , transform' datum_.translateNode
  ]

updateAttrs :: Array SelectionAttribute
updateAttrs = 
  [ classed datum_.nodeClass
  , transform' datum_.translateNode
  ]

type SpagoSceneAttributes = { 
    circles :: Array SelectionAttribute
  , labels  :: Array SelectionAttribute
}

clusterSceneAttributes :: SpagoSceneAttributes
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
}

graphSceneAttributes :: SpagoSceneAttributes
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
}

treeSceneAttributes :: SpagoSceneAttributes
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
}

svgAttrs :: Number -> Number -> Array SelectionAttribute
svgAttrs w h = [ viewBox (-w / 2.1) (-h / 2.05) w h 
                    -- , preserveAspectRatio $ AspectRatio XMid YMid Meet 
                    , classed "overlay"
                    , width w, height h
                    , cursor "grab"
]
