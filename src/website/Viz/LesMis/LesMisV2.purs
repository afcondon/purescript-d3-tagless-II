module D3.Viz.LesMisV2 where

-- | Force-directed graph visualization using PSD3v2
-- | Port of Les Misérables character network to PSD3v2 architecture
-- |
-- | This demonstrates:
-- | - PSD3v2 SelectionM for DOM manipulation
-- | - PSD3v2 SimulationM for force simulation
-- | - D3v2SimM interpreter supporting both capabilities
-- | - Type-safe data joins with phantom types

import Prelude

import Control.Monad.State (class MonadState)
import D3.Viz.LesMiserables.Model (LesMisRawModel, LesMisSimNode)
import Data.Map as Map
import Data.Number (sqrt)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import PSD3.Data.Node (D3Link_Swizzled, SimulationNode)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.FFI (keyIsID_)
import PSD3v2.Attribute.Types (cx, cy, fill, radius, stroke, strokeWidth, x1, x2, y1, y2, id_, class_, width, height, viewBox)
import PSD3v2.Capabilities.Selection (select, appendChild, joinData, append)
import PSD3v2.Capabilities.Simulation (init, addTickFunction, start, Step(..))
import PSD3v2.Interpreter.D3v2 (D3v2SimM, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..), SBound)
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)
import Data.Int (toNumber)
import Web.DOM.Element (Element)
import Data.Array as Array

-- | Indexed link for data join (links don't have Ord instance, so we use index)
newtype IndexedLink = IndexedLink { index :: Int, link :: D3Link_Swizzled }

instance Eq IndexedLink where
  eq (IndexedLink a) (IndexedLink b) = a.index == b.index

instance Ord IndexedLink where
  compare (IndexedLink a) (IndexedLink b) = compare a.index b.index

-- | Draw LesMis force-directed graph using PSD3v2
-- |
-- | This is a direct port of LesMis.drawSimplified to PSD3v2 architecture.
-- | Uses the D3v2SimM interpreter with SelectionM and SimulationM capabilities.
drawLesMisV2 :: forall row.
  Array (Force LesMisSimNode) ->
  Set.Set String ->
  LesMisRawModel ->
  String ->
  D3v2SimM row LesMisSimNode Unit
drawLesMisV2 forcesArray activeForces model containerSelector = do
  -- Get window dimensions
  (Tuple w h) <- liftEffect getWindowWidthHeight

  -- Create SVG container
  container <- select containerSelector
  svg <- appendChild SVG
    [ width w
    , height h
    , viewBox (show ((-w) / 2.0) <> " " <> show ((-h) / 2.0) <> " " <> show w <> " " <> show h)
    , id_ "lesmis-v2-svg"
    , class_ "lesmis-v2"
    ]
    container

  -- Create groups for links and nodes
  linksGroup <- appendChild Group
    [ id_ "links"
    , class_ "links"
    ]
    svg

  nodesGroup <- appendChild Group
    [ id_ "nodes"
    , class_ "nodes"
    ]
    svg

  -- Initialize simulation
  { nodes: nodesInSim, links: linksInSim } <- init
    { nodes: model.nodes
    , links: model.links
    , forces: forcesArray
    , activeForces: activeForces
    , config:
        { alpha: 1.0
        , alphaTarget: 0.0
        , alphaMin: 0.001
        , alphaDecay: 0.0228
        , velocityDecay: 0.4
        }
    , keyFn: keyIsID_
    , ticks: Map.empty
    }

  -- Wrap links with indices for data join (links don't have built-in identity)
  let indexedLinks = Array.mapWithIndex (\i link -> IndexedLink { index: i, link }) linksInSim

  -- Join links to DOM
  JoinResult { enter: linkEnter } <- joinData indexedLinks "line" linksGroup
  linkLines <- append Line
    [ x1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.x :: Number)
    , y1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.y :: Number)
    , x2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.x :: Number)
    , y2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.y :: Number)
    , strokeWidth (\(IndexedLink il) -> sqrt (unsafeCoerce il.link).value :: Number)
    , stroke (\(IndexedLink il) -> d3SchemeCategory10N_ (toNumber (unsafeCoerce il.link).target.group) :: String)
    ]
    linkEnter

  -- Join nodes to DOM
  JoinResult { enter: nodeEnter } <- joinData nodesInSim "circle" nodesGroup
  nodeCircles <- append Circle
    [ cx (\(d :: LesMisSimNode) -> d.x)
    , cy (\(d :: LesMisSimNode) -> d.y)
    , radius 5.0
    , fill (\(d :: LesMisSimNode) -> d3SchemeCategory10N_ (toNumber d.group))
    , stroke "#fff"
    , strokeWidth 2.0
    ]
    nodeEnter

  -- Add tick functions to update positions
  addTickFunction "nodes" $ Step nodeCircles
    [ cx (\(d :: LesMisSimNode) -> d.x)
    , cy (\(d :: LesMisSimNode) -> d.y)
    ]

  addTickFunction "links" $ Step linkLines
    [ x1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.x :: Number)
    , y1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.y :: Number)
    , x2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.x :: Number)
    , y2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.y :: Number)
    ]

  -- Start simulation
  start

  pure unit
