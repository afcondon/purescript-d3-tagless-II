module D3.Examples.Tree where

import D3.Attributes.Sugar
import Prelude (bind, negate, pure, ($))

import D3.Attributes.Instances (Attribute, Datum)
import D3.Interpreter.Tagless (class D3Tagless, append, hook)
import D3.Selection (Chainable, D3Selection, D3_Node, Element(..), node, node_)
import Unsafe.Coerce (unsafeCoerce)

-- | Model types
type Model = { links :: Links, nodes :: Nodes }

type Node = { name :: String, x :: Number, y :: Number }
type Link = { source :: String, target :: String, count :: Number }

type Links = Array Link
type Nodes = Array Node

datumToNode = unsafeCoerce :: Datum -> Node
datumToLink = unsafeCoerce :: Datum -> Link

-- | Script components, attributes, transformations etc
svgAttributes :: Array Chainable
svgAttributes = [
    width 1000.0
  , height 1000.0
  , viewBox (-500.0) (-500.0) 1000.0 1000.0
]

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

enter :: ∀ m. (D3Tagless m) => m D3Selection
enter = do
  root <- hook "div#tree"
  svg  <- append "svg-tree" $ node Svg svgAttributes
  _    <- append "links-group"  $ node_ Group
  _    <- append "nodes-group"  $ node_ Group
  _    <- append "labels-group" $ node_ Group

  pure svg



  -- joinSelection_ <- join state.model $ Join {
  --     element   : Text
  --   , key       : DatumIsKey
  --   , selection : SelectionName "letter-group"
  --   , projection: unsafeCoerce -- null projection
  --   , behaviour : enterUpdateExit transition
  -- }
