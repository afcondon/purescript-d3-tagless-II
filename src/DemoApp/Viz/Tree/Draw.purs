module D3.Examples.Tree.Draw where

import Prelude

import D3.Attributes.Sugar (classed, dy, fill, fontFamily, fontSize, height, radius, strokeColor, strokeOpacity, strokeWidth, text, textAnchor, width, x)
import D3.Data.Tree (TreeLayout(..))
import D3.Data.Types (Datum_, Element(..), Selector)
import D3.Examples.MetaTree.Unsafe (coerceToTreeNode, unboxD3TreeNode)
import D3.Examples.Tree.Model (FlareTreeNode)
import D3.FFI (descendants_, getHierarchyValue_, hasChildren_, keyIsID_, links_)
import D3.Selection (SelectionAttribute)
import D3Tagless.Capabilities (class SelectionM, appendTo, attach, setAttributes, simpleJoin)
import Data.Nullable (Nullable)
import Data.Number (pi)

treeDatum_ :: 
  { hasChildren :: Datum_ -> Boolean
  , name :: Datum_ -> String
  , onRHS :: TreeLayout -> Datum_ -> Boolean
  , textAnchor :: TreeLayout -> Datum_ -> String
  , textX :: TreeLayout -> Datum_ -> Number
  , x :: Datum_ -> Number
  , y :: Datum_ -> Number
  , depth :: Datum_ -> Int
  , height :: Datum_ -> Int
  , value :: Datum_ -> Nullable Number
  }
treeDatum_ = {
-- simple accessors first
    depth : _.depth <<< unboxD3TreeNode <<< coerceToTreeNode
  , height: _.height <<< unboxD3TreeNode <<< coerceToTreeNode
  , x     : _.x <<< unboxD3TreeNode <<< coerceToTreeNode
  , y     : _.y <<< unboxD3TreeNode <<< coerceToTreeNode
-- now accessors that use the embedded "data" object within the Tree node
  , name   : _.data.name <<< unboxD3TreeNode <<< coerceToTreeNode
-- now more semanticly complicated accessors
  -- value_ returns null rather than undefined if there is no value field
  , value : getHierarchyValue_ <<< coerceToTreeNode
  -- hasChildren_ returns empty array rather than undefined if there is no children field
  , hasChildren: hasChildren_ <<< coerceToTreeNode
-- -- TODO these next two should be rewritten to use some sort of choice operator
  , textAnchor : (\l d -> case l of
                            Radial ->
                              if (treeDatum_.hasChildren d) == (treeDatum_.x d < pi)
                              then "start"
                              else "end"
                            _ -> 
                              if (treeDatum_.hasChildren d)
                              then "start"
                              else "end"
                        )
  , textX : (\l d -> case l of
                      Radial ->
                        if (treeDatum_.hasChildren d) == (treeDatum_.x d < pi) -- d.x < pi => node is on the RHS of Radial tree
                        then 6.0
                        else (-6.0)
                      _ -> 
                        if (treeDatum_.hasChildren d)
                        then 6.0
                        else (-6.0)
                  )
  , onRHS       : \l d -> if l == Radial && (treeDatum_.x d >= pi)
                        then true
                        else false
}


-- a record that packages up all the customizations that are needed to render the 6 variations on Tree
type ScriptConfig = { 
    layout        :: TreeLayout
  , selector      :: Selector String
  , linkPath      :: SelectionAttribute
  , spacing       :: { interChild :: Number, interLevel :: Number }
  , viewbox       :: Array SelectionAttribute
  , nodeTransform :: Array SelectionAttribute
  , color         :: String
  , svg           :: { width :: Number, height :: Number }
}

-- | The eDSL script that renders tree layouts
-- | it has been parameterized rather heavily using the ScriptConfig record so that it can draw
-- | all six variations of [Radial, Horizontal, Vertical] * [Dendrogram, TidyTree] 
-- | NB there would be nothing wrong, per se, with individual examples, this just shows 
-- | some more composability, at the price of some direct legibility

-- Snippet_Start
-- Name: TreeDraw
draw :: forall m selection. Bind m => SelectionM selection m => 
  ScriptConfig -> FlareTreeNode ->  m selection
draw config tree = do
  root       <- attach config.selector  
  svg        <- appendTo root Svg (config.viewbox <> 
                                    [ classed "tree", width config.svg.width, height config.svg.height ]) 
  container  <- appendTo svg  Group [ fontFamily      "sans-serif", fontSize 10.0 ]
  links      <- appendTo container Group [ classed "links"]
  nodes      <- appendTo container Group [ classed "nodes"]

  theLinks_  <- simpleJoin links Path (links_ tree) keyIsID_
  setAttributes theLinks_ [ strokeWidth 1.5, strokeColor config.color, strokeOpacity 0.4, fill "none", config.linkPath ]

  -- we make a group to hold the node circle and the label text
  nodeJoin_  <- simpleJoin nodes Group (descendants_ tree) keyIsID_ 
  setAttributes nodeJoin_ config.nodeTransform

  theNodes <- appendTo nodeJoin_ Circle
                [ fill         (\(d :: Datum_) -> if treeDatum_.hasChildren d then "#999" else "#555")
                , radius       2.5
                , strokeColor "white"
                ]

  theLabels <- appendTo nodeJoin_ Text
                [ dy         0.31
                , x          (treeDatum_.textX config.layout)
                , textAnchor (treeDatum_.textAnchor config.layout)
                , text       treeDatum_.name
                , fill       config.color
                ]               
  pure svg
-- Snippet_End