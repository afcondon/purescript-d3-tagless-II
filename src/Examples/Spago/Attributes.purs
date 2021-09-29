module D3.Examples.Spago.Draw.Attributes where

import D3.Attributes.Sugar (classed, cursor, fill, height, onMouseEvent, opacity, radius, strokeColor, strokeWidth, text, textAnchor, transform', viewBox, width, x, y)
import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (D3Simulation_, MouseEvent(..))
import D3.Examples.Spago.Model (cancelSpotlight_, datum_, explodePackages, toggleSpotlight, tree_datum_)
import D3.Selection (SelectionAttribute)
import Prelude (negate, (/))

-- TODO this is a problem once extracted from "script", leads to undefined in D3.js
enterLinks :: forall t339. Array t339
enterLinks = [] -- [ classed link_.linkClass ] -- default invisible in CSS unless marked "visible"

explodePackageOnClick :: D3Simulation_ -> SelectionAttribute
explodePackageOnClick simulation_ = onMouseEvent MouseClick (\e d _ -> explodePackages e simulation_ d)

toggleSpotlightOnClick :: D3Simulation_ -> SelectionAttribute
toggleSpotlightOnClick simulation_ = onMouseEvent MouseClick (\e d _ -> toggleSpotlight e simulation_ d)

-- unused right now but could be used to make, for example, click on Package or background cause unSpotlighting of node
undoSpotlightOnClick :: D3Simulation_ -> SelectionAttribute
undoSpotlightOnClick simulation_ = onMouseEvent MouseClick (\e d t -> cancelSpotlight_ simulation_)

enterAttrs :: Array SelectionAttribute
enterAttrs = 
  [ classed datum_.nodeClass
  , transform' datum_.translateNode
  ]

updateAttrs :: forall t1. t1 -> Array SelectionAttribute
updateAttrs _ = 
  [ classed datum_.nodeClass
  , transform' datum_.translateNode
  ]

clusterSceneAttributes :: { circle :: Array SelectionAttribute , labels :: Array SelectionAttribute }
clusterSceneAttributes = { 
    circle: [ radius datum_.radius
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
            -- , text datum_.indexAndID
          ] 
}

graphSceneAttributes :: { circle :: Array SelectionAttribute , labels :: Array SelectionAttribute }
graphSceneAttributes = { 
    circle: [ radius datum_.radius
            , fill datum_.colorByGroup
            , opacity datum_.opacityByType
           ]
  , labels: [ classed "label"
            , x 0.2
            , y datum_.positionLabel
            , textAnchor "middle"
            , text datum_.name
            -- , text datum_.indexAndID
          ] 
}

treeSceneAttributes :: { circle :: Array SelectionAttribute, labels :: Array SelectionAttribute }
treeSceneAttributes  = {
    circle: [ radius datum_.radius
            , fill datum_.colorByDepth
            , strokeColor datum_.colorByGroup
            , strokeWidth 3.0
            ]
  , labels: [ classed "label"
            , x 4.0
            , y 2.0
            , textAnchor (tree_datum_.textAnchor Horizontal)
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
