module D3.Examples.Tree.Script where

import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, x)
import D3.Data.Tree (labelName)
import D3.Data.Types (Element(..), TreeModel)
import D3.Examples.Tree.Types (ScriptConfig)
import D3.FFI (descendants_, hasChildren_, links_)
import D3.Interpreter (class D3InterpreterM, append, attach, (<+>))
import D3.Selection (Join(..), Keys(..), node)
import Prelude (class Bind, bind, negate, pure)

-- | The eDSL script that renders tree layouts
-- | it has been parameterized rather heavily using the ScriptConfig record so that it can draw
-- | all six variations of [Radial, Horizontal, Vertical] * [Dendrogram, TidyTree] 
-- | NB there would be nothing wrong, per se, with individual examples, this just shows 
-- | some more composability, at the price of some direct legibility
treeScript :: forall m v selection. Bind m => D3InterpreterM selection m => 
  ScriptConfig -> TreeModel String v -> m selection
treeScript config model = do
  root       <- attach config.selector                           
  svg        <- root `append` (node Svg config.viewbox)          
  container  <- svg  `append` (node Group [ fontFamily      "sans-serif"
                                          , fontSize        10.0
                                          ])
  links      <- container `append` (node Group [ classed "links"])
  nodes      <- container `append` (node Group [ classed "nodes"])

  theLinks_  <- links <+> Join {
      element   : Path
    , key       : UseDatumAsKey
    , "data"    : links_ model.root_
    , behaviour : [ strokeWidth   1.5
                  , strokeColor   config.color
                  , strokeOpacity 0.4
                  , fill          "none"
                  , config.linkPath
                  ]
  }

  nodeJoin_  <- nodes <+> Join {
      element   : Group
    , key       : UseDatumAsKey
    , "data"    : descendants_ model.root_
    -- there could be other stylistic stuff here but the transform is key structuring component
    , behaviour : config.nodeTransform -- <- the key positioning calculation for the tree!!!
  }

  theNodes <- nodeJoin_ `append` 
                (node Circle  [ fill         (\datum -> if hasChildren_ datum then "#999" else "#555")
                              , radius       2.5
                              , strokeColor "white"
                              ])

  theLabels <- nodeJoin_ `append`
                (node Text  [ dy         0.31
                            , x          (\datum -> if config.textDirection datum then 6.0 else (-6.0))
                            , textAnchor (\datum -> if config.textDirection datum then "start" else "end")
                            , text       labelName
                            , fill       config.color
                            ])
                            
  pure svg


