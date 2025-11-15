module D3.Viz.Spago.RenderV2 where

import Prelude

import D3.Viz.Spago.Model (SpagoSimNode, SpagoSwizzledLink)
import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, translateNode, nodeClass)
import D3.Viz.Spago.Files (LinkType(..))
import PSD3.Data.Node (D3Link_Swizzled)
import PSD3v2.Attribute.Types (class_, transform, stroke, strokeWidth, x1, y1, x2, y2, radius, fill, opacity, textContent, textAnchor, x, y, cx, cy)
import PSD3v2.Behavior.Types (Behavior(..), simulationDrag)
import PSD3v2.Capabilities.Selection (joinData, append, on, setAttrs, appendChild)
import PSD3v2.Interpreter.D3v2 (D3v2SimM, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), JoinResult(..), SBound, SEmpty)
import Unsafe.Coerce (unsafeCoerce)
import Data.Array as Array
import Web.DOM.Element (Element)

-- | Indexed link for data join (links don't have Ord instance, so we use index)
newtype IndexedLink = IndexedLink { index :: Int, link :: D3Link_Swizzled }

instance Eq IndexedLink where
  eq (IndexedLink a) (IndexedLink b) = a.index == b.index

instance Ord IndexedLink where
  compare (IndexedLink a) (IndexedLink b) = compare a.index b.index

-- | Link helper functions
linkClass :: SpagoSwizzledLink -> String
linkClass l = show l.linktype

linkColor :: SpagoSwizzledLink -> String
linkColor l = case l.linktype of
  P2P -> "red"
  M2P -> "blue"
  M2M_Graph -> "green"
  M2M_Tree -> "orange"

-- | Render nodes as circles (simplified version without Group wrapper)
-- | TODO: Upgrade to multi-level DOM (Group → Circle + Text) once PSD3v2 has nested builders
renderNodes :: forall row.
  Array SpagoSimNode ->
  D3v2Selection_ SEmpty ElementType SpagoSimNode ->
  SpagoSceneAttributes ->
  D3v2SimM row SpagoSimNode (D3v2Selection_ SBound Element SpagoSimNode)
renderNodes nodes nodesGroup _attrs = do
  -- Join nodes to DOM
  JoinResult { enter: nodeEnter, update: nodeUpdate, exit: _nodeExit } <- joinData nodes "circle" nodesGroup

  -- Process enter: Create circles (simplified - no Group wrapper for now)
  -- TODO: Implement multi-level DOM (Group → Circle + Text) once PSD3v2 has nested builders
  nodeCircles <- append Circle
    [ cx (\(d :: SpagoSimNode) -> d.x)
    , cy (\(d :: SpagoSimNode) -> d.y)
    , radius (\(d :: SpagoSimNode) -> d.r)
    , fill "steelblue"
    , opacity 0.7
    , class_ (\(d :: SpagoSimNode) -> nodeClass d)
    ]
    nodeEnter

  -- Attach simulation drag to node circles
  _ <- on (Drag $ simulationDrag "spago") nodeCircles

  -- Process update: Update existing nodes (simplified)
  -- TODO: Proper update handling
  _ <- setAttrs
    [ cx (\(d :: SpagoSimNode) -> d.x)
    , cy (\(d :: SpagoSimNode) -> d.y)
    ]
    nodeUpdate

  -- Process exit: Remove exiting nodes
  -- (For now, we'll let them stay - proper removal needs transition support)

  pure nodeCircles

-- | Render links using simple Line elements
renderLinks :: forall row.
  Array D3Link_Swizzled ->
  D3v2Selection_ SEmpty ElementType IndexedLink ->
  D3v2SimM row SpagoSimNode (D3v2Selection_ SBound Element IndexedLink)
renderLinks links linksGroup = do
  -- Wrap links with indices for data join
  let indexedLinks = Array.mapWithIndex (\i link -> IndexedLink { index: i, link }) links

  -- Join links to DOM
  JoinResult { enter: linkEnter } <- joinData indexedLinks "line" linksGroup

  -- Create link lines
  linkLines <- append Line
    [ x1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.x :: Number)
    , y1 (\(IndexedLink il) -> (unsafeCoerce il.link).source.y :: Number)
    , x2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.x :: Number)
    , y2 (\(IndexedLink il) -> (unsafeCoerce il.link).target.y :: Number)
    , class_ (\(IndexedLink il) -> linkClass (unsafeCoerce il.link :: SpagoSwizzledLink))
    , stroke (\(IndexedLink il) -> linkColor (unsafeCoerce il.link :: SpagoSwizzledLink))
    , strokeWidth 1.0
    ]
    linkEnter

  pure linkLines
