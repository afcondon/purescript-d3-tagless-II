module D3.Examples.Tree.Script where

import D3.Node (D3SimulationRow, D3TreeRow, D3_Link, EmbeddedData, NodeID)
import Prelude (class Bind, bind, negate, pure)

import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, x)
import D3.Data.Types (Datum_, Element(..), Selector)
import D3.FFI (descendants_, hasChildren_, links_)
import D3.Interpreter (class D3InterpreterM, append, attach, (<+>))
import D3.Selection (ChainableS, Join(..), Keys(..), node)
import Unsafe.Coerce (unsafeCoerce)
import Type.Row (type (+))

-- Model data types specialized with inital data
type FlareNodeRow row = ( name :: String | row )
type FlareNodeData    = { | FlareNodeRow () }

type FlareTreeNode    = D3TreeRow       (EmbeddedData FlareNodeData + ())
type FlareSimNode     = D3SimulationRow (             FlareNodeRow  + ())

type FlareLinkData = ( value :: Number )
type FlareModel    = { links :: Array (D3_Link NodeID FlareLinkData)
                      , nodes :: Array FlareNodeData }

-- a record that packages up all the customizations that are needed to render the 6 variations on Tree
type ScriptConfig = { 
    linkPath      :: ChainableS
  , selector      :: Selector
  , spacing       :: { interChild :: Number, interLevel :: Number }
  , viewbox       :: Array ChainableS
  , nodeTransform :: Array ChainableS
  , color         :: String
  , textDirection :: Datum_ -> Boolean
  , svg           :: { width :: Number, height :: Number }
}

-- | The eDSL script that renders tree layouts
-- | it has been parameterized rather heavily using the ScriptConfig record so that it can draw
-- | all six variations of [Radial, Horizontal, Vertical] * [Dendrogram, TidyTree] 
-- | NB there would be nothing wrong, per se, with individual examples, this just shows 
-- | some more composability, at the price of some direct legibility
script :: forall m selection. Bind m => D3InterpreterM selection m => 
  ScriptConfig -> FlareTreeNode ->  m selection
script config tree = do
  root       <- attach config.selector   
  svg        <- root `append` (node Svg config.viewbox)          
  container  <- svg  `append` (node Group [ fontFamily      "sans-serif"
                                          , fontSize        10.0
                                          ])
  links      <- container `append` (node Group [ classed "links"] )
  nodes      <- container `append` (node Group [ classed "nodes"] )

  theLinks_  <- links <+> Join {
      element   : Path
    , key       : UseDatumAsKey
    , "data"    : links_ tree
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
    , "data"    : descendants_ tree
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

-- datumIsTreeNode :: forall d v. Datum_ -> D3_Hierarchy_Node_ d v
-- datumIsTreeNode = unsafeCoerce

labelName :: Datum_ -> String
labelName d = node."data".name
  where node = unsafeCoerce d

