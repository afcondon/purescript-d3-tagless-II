module D3.Examples.Tree.Script where

import Prelude

import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, x)
import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (Datum_, Element(..), Selector)
import D3.Examples.MetaTree.Unsafe (unboxD3TreeNode)
import D3.Examples.Tree.Model (FlareTreeNode)
import D3.FFI (descendants_, hasChildren_, links_)
import D3.Interpreter (class D3InterpreterM, append, attach, (<+>))
import D3.Selection (ChainableS, Join(..), Keys(..), node)
import Data.Nullable (Nullable)
import Math (pi)

datum_ :: 
  { depth :: Datum_ -> Int
  , hasChildren :: Datum_ -> Boolean
  , height :: Datum_ -> Int
  , id :: Datum_ -> Int
  , name :: Datum_ -> String
  , textAnchor :: TreeLayout -> Datum_ -> String
  , textX :: TreeLayout -> Datum_ -> Number
  , value :: Datum_ -> Nullable Number
  , x :: Datum_ -> Number
  , y :: Datum_ -> Number
  }
datum_ = {
-- simple accessors first
    depth : (\d -> (unboxD3TreeNode d).depth)
  , height: (\d -> (unboxD3TreeNode d).height)
  , id    : (\d -> (unboxD3TreeNode d).id)
  , value : (\d -> (unboxD3TreeNode d).value)
  , x     : (\d -> (unboxD3TreeNode d).x)
  , y     : (\d -> (unboxD3TreeNode d).y)
-- now accessors that use the embedded "data" object within the Tree node
  , name   : (\d -> (unboxD3TreeNode d).data.name)
-- now more semanticly complicated accessors
  , hasChildren: (\d -> hasChildren_ d) -- this particular one has to be done by FFI
-- TODO these next two should be rewritten to use some sort of choice operator
  , textAnchor : (\l d -> case l of
                            Radial ->
                              if (hasChildren_ d) == (datum_.x d < pi)
                              then "start"
                              else "end"
                            _ -> 
                              if (hasChildren_ d)
                              then "start"
                              else "end"
                        )
  , textX : (\l d -> case l of
                      Radial ->
                        if (hasChildren_ d) == (datum_.x d < pi) -- d.x < pi => node is on the RHS of Radial tree
                        then 6.0
                        else (-6.0)
                      _ -> 
                        if (hasChildren_ d)
                        then 6.0
                        else (-6.0)
                  )
}


-- a record that packages up all the customizations that are needed to render the 6 variations on Tree
type ScriptConfig = { 
    layout        :: TreeLayout
  , selector      :: Selector
  , linkPath      :: ChainableS
  , spacing       :: { interChild :: Number, interLevel :: Number }
  , viewbox       :: Array ChainableS
  , nodeTransform :: Array ChainableS
  , color         :: String
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
  svg        <- root `append` (node Svg (config.viewbox <> [ classed "tree"]))          
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
                (node Circle  [ fill         (\d -> if datum_.hasChildren d then "#999" else "#555")
                              , radius       2.5
                              , strokeColor "white"
                              ])

  theLabels <- nodeJoin_ `append`
                (node Text  [ dy         0.31
                            , x          (datum_.textX config.layout)
                            , textAnchor (\d -> datum_.textAnchor config.layout d)
                            , text       datum_.name
                            , fill       config.color
                            ])               
  pure svg
