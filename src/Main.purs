module Main where

import Prelude (Unit, bind, pure, ($))
import D3.Selection 
import D3.Interpreter.Tagless
import Effect (Effect)
import Effect.Console (log)

-- | Model types
-- type Model = { links :: Links, nodes :: Nodes }

-- type Node = { name :: String, x :: Number, y :: Number }
-- type Link = { source :: String, target :: String, count :: Number }

-- type Links = Array Link
-- type Nodes = Array Node

-- datumToNode = unsafeCoerce :: Datum -> Node
-- datumToLink = unsafeCoerce :: Datum -> Link

-- -- | Example Model data
-- exampleLink :: Link
-- exampleLink = { source: "a", target: "b", count: 1.0 }

-- exampleNode :: Node
-- exampleNode = { name: "a", x: 0.0,  y: 0.0 }

-- exampleLinks :: Links
-- exampleLinks = [ exampleLink, exampleLink, exampleLink ]
-- exampleNodes :: Nodes
-- exampleNodes = [ exampleNode, exampleNode, exampleNode ]

-- model :: Model
-- model = { links: exampleLinks, nodes: exampleNodes }

-- -- | Example Chart to work with Model types and data above

-- linkColor :: Link -> Attribute
-- linkColor a = 
--   if a.source == a.target
--   then strokeColor"green"
--   else strokeColor "blue"

-- linkWidth :: Link -> Attribute
-- linkWidth a = strokeOpacity $ a.count * 5.0

-- linkAttrs :: Array (Link -> Attribute)
-- linkAttrs = [ linkWidth, linkColor ]

-- tree :: Selection Model
-- tree = 
--   Hook "div" $ 
--     Append Svg [] [
--         Join Group (\a -> a.links) End
--       , Join Group (\a -> a.nodes) End
--       , Join Group (\a -> a.nodes) End
--     ]

-- joinNodes :: JoinData Model Nodes
-- joinNodes m = 
--   Tuple m.nodes $
--     Append "circle" [ strokeOpacity 0.75
--                     , x (\d -> (datumToNode d).x * 5.0) ] [
--     ]

-- joinLinks :: JoinData Model Links
-- joinLinks m =
--   Tuple m.links End

-- joinLabels :: JoinData Model Nodes
-- joinLabels m =
--   Tuple m.nodes End

script :: ∀ m. (D3Tagless m) => m D3Selection
script = do
    root <- hook "div#root"
    
    svg <- append $ Node Svg (someAttributes _SomeDatum ) [ Node Group [] [ node__ Circle ] ]

    _ <- join Circle { enter: [], update: [], exit: [] }

    pure nullD3Selection



main :: Effect Unit
main = do
  _ <- d3Run script
  log "🍝"
