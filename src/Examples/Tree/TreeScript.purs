module D3.Examples.Tree.Script where

import D3.Layouts.Hierarchical

import D3.Attributes.Sugar (backgroundColor, classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, x)
import D3.Examples.Tree.Types (ScriptConfig, labelName)
import D3.Interpreter (class D3InterpreterM, append, attach, attachZoom, (<+>))
import D3.Layouts.Hierarchical as H
import D3.Selection (Element(..), Join(..), Keys(..), ScaleExtent(..), ZoomExtent(..), node)
import Prelude (class Bind, bind, negate, pure)

-- | The eDSL script that renders tree layouts
-- | it has been parameterized rather heavily using the ScriptConfig record so that it can draw
-- | all six variations of [Radial, Horizontal, Vertical] * [Dendrogram, TidyTree] 
-- | NB there would be nothing wrong, per se, with individual examples, this just shows 
-- | some more composability, at the price of some direct legibility
treeScript :: forall m v selection. Bind m => D3InterpreterM selection m => 
  ScriptConfig -> H.Model String v -> m selection
treeScript config model = do
  root      <- attach config.selector
  svg       <- root      `append` (node Svg config.viewbox)
  container <- svg       `append` (node Group [ fontFamily "sans-serif"
                                              , fontSize   10.0
                                              , backgroundColor "beige"
                                              ])
  links     <- container `append` (node Group [ classed "links"])
  nodes     <- container `append` (node Group [ classed "nodes"])

  theLinks_ <- links <+> Join {
      element   : Path
    , key       : UseDatumAsKey
    , "data"    : H.links_ model.root_
    , behaviour : [ strokeWidth   1.5
                  , strokeColor   config.color
                  , strokeOpacity 0.4
                  , fill          "none"
                  , config.linkPath
                  ]
  }

  nodeJoin_ <- nodes <+> Join {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : H.descendants_ model.root_
    -- there could be other stylistic stuff here but the transform is key structuring component
    , behaviour : config.nodeTransform -- <- the key positioning calculation for the tree!!!
  }

  theNodes <- nodeJoin_ `append` 
                (node Circle  [ fill (\d -> if hasChildren_ d then "#999" else "#555")
                              , radius 2.5
                              , strokeColor "white"
                              ])

  theLabels <- nodeJoin_ `append`
                (node Text  [ dy         0.31
                            , x          (\d -> if config.textDirection d then 6.0 else (-6.0))
                            , textAnchor (\d -> if config.textDirection d then "start" else "end")
                            , text       labelName
                            , fill config.color
                            ])

  svgZ <- container `attachZoom`   
                    { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: 500.0, right: 500.0 }
                    , scale     : ScaleExtent 1 8 -- wonder if ScaleExtent ctor could be range operator `..`
                    , qualifier : "tree"
                    }

  pure svg


