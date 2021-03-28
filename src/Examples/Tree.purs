module D3.Examples.Tree where

import D3.Attributes.Instances (Attribute, Datum)
import D3.Attributes.Sugar (strokeColor, strokeOpacity)
import Prelude
import Unsafe.Coerce (unsafeCoerce)

-- | Model types
type Model = { links :: Links, nodes :: Nodes }

type Node = { name :: String, x :: Number, y :: Number }
type Link = { source :: String, target :: String, count :: Number }

type Links = Array Link
type Nodes = Array Node

datumToNode = unsafeCoerce :: Datum -> Node
datumToLink = unsafeCoerce :: Datum -> Link

-- | Example Model data
exampleLink :: Link
exampleLink = { source: "a", target: "b", count: 1.0 }

exampleNode :: Node
exampleNode = { name: "a", x: 0.0,  y: 0.0 }

exampleLinks :: Links
exampleLinks = [ exampleLink, exampleLink, exampleLink ]
exampleNodes :: Nodes
exampleNodes = [ exampleNode, exampleNode, exampleNode ]

model :: Model
model = { links: exampleLinks, nodes: exampleNodes }

-- | Example Chart to work with Model types and data above

linkColor :: Link -> Attribute
linkColor a = 
  if a.source == a.target
  then strokeColor"green"
  else strokeColor "blue"

linkWidth :: Link -> Attribute
linkWidth a = strokeOpacity $ a.count * 5.0

linkAttrs :: Array (Link -> Attribute)
linkAttrs = [ linkWidth, linkColor ]

-- || this example is written using the Data Structure style of interpreter, not tagless
-- || kept for reference to be re-written with Tagless soon
{-
tree :: Selection Model
tree = 
  Hook "div" $ 
    Append Svg [] [
        Join Group (\a -> a.links) End
      , Join Group (\a -> a.nodes) End
      , Join Group (\a -> a.nodes) End
    ]

joinNodes :: JoinData Model Nodes
joinNodes m = 
  Tuple m.nodes $
    Append "circle" [ strokeOpacity 0.75
                    , x (\d -> (datumToNode d).x * 5.0) ] [
    ]

joinLinks :: JoinData Model Links
joinLinks m =
  Tuple m.links End

joinLabels :: JoinData Model Nodes
joinLabels m =
  Tuple m.nodes End
-}