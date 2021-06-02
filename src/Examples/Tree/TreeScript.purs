module D3.Examples.Tree.Script where

import Prelude

import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, x)
import D3.Data.Tree (TreeLayout(..), TreeType)
import D3.Data.Types (Datum_, Element(..), Selector)
import D3.FFI (descendants_, hasChildren_, links_)
import D3.Interpreter (class D3InterpreterM, append, attach, (<+>))
import D3.Node (D3SimulationRow, D3TreeRow, D3_ID, D3_Link(..), D3_TreeNode(..), D3_TreeRow, D3_XY, EmbeddedData, NodeID)
import D3.Selection (ChainableS, Join(..), Keys(..), node)
import Data.Nullable (Nullable)
import Math (pi)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- Model data types specialized with inital data
type FlareNodeRow row = ( name :: String | row )
type FlareNodeData    = { | FlareNodeRow () }

type FlareTreeNode    = D3TreeRow       (EmbeddedData FlareNodeData + ())
type FlareSimNode     = D3SimulationRow (             FlareNodeRow  + ())

type FlareLinkData  = ( value :: Number )
type FlareSimRecord = Record (FlareNodeRow  + ()) 
type FlareLinkObj   =  { source :: FlareSimRecord, target :: FlareSimRecord | FlareLinkData }

type FlareRawModel = { 
    links :: Array (D3_Link NodeID FlareLinkData)
  , nodes :: Array FlareNodeData
}

type FlareCookedModel = { 
    links :: Array (D3_Link NodeID FlareLinkData)
  , nodes :: Array FlareNodeData
}

unboxD3SimLink :: Datum_ -> FlareLinkObj
unboxD3SimLink datum = do
  let (D3_Link l) = unsafeCoerce datum
  l

unboxD3TreeNode :: Datum_ -> { children :: Array
                     (D3_TreeNode
                        ( data :: { name :: String
                                  }
                        , depth :: Int
                        , height :: Int
                        , id :: Int
                        , value :: Nullable Number
                        , x :: Number
                        , y :: Number
                        )
                     )
     , data :: { name :: String
               }
     , depth :: Int
     , height :: Int
     , id :: Int
     , parent :: Nullable
                   (D3_TreeNode
                      ( data :: { name :: String
                                }
                      , depth :: Int
                      , height :: Int
                      , id :: Int
                      , value :: Nullable Number
                      , x :: Number
                      , y :: Number
                      )
                   )
     , value :: Nullable Number
     , x :: Number
     , y :: Number
     }
unboxD3TreeNode datum = do
  let (t' :: D3_TreeNode (D3_ID + D3_TreeRow + D3_XY + (EmbeddedData { | FlareNodeRow () }) + () ) )  = unsafeCoerce datum
      (D3TreeNode t) = t'
  t

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
  , "type" : (\t d -> t)
  , layout : (\l d -> l)
-- now more semanticly complicated accessors
  , hasChildren: (\d -> hasChildren_ d) -- this particular one has to be done by FFI
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
  , linkPath      :: ChainableS
  , selector      :: Selector
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
                (node Circle  [ fill         (\d -> if datum_.hasChildren d then "#999" else "#555")
                              , radius       2.5
                              , strokeColor "white"
                              ])

  theLabels <- nodeJoin_ `append`
                (node Text  [ dy         0.31
                            , x          (datum_.textX config.layout)
                            , textAnchor (datum_.textAnchor config.layout)
                            , text       datum_.name
                            , fill       config.color
                            ])
                            
  pure svg
