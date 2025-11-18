module D3.Viz.Spago.Render where

import Prelude

import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, enterAttrs, updateAttrs, translateNode)
import D3.Viz.Spago.Files (LinkType(..))
import D3.Viz.Spago.Model (SpagoSimNode)
import Data.Array ((:))
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as String
import PSD3.Capabilities.Selection (class SelectionM, appendTo, on, selectUnder, setAttributes)
import PSD3.Data.Node (NodeID)
import PSD3.Internal.Attributes.Sugar (classed, remove, strokeColor, transform', x1, x2, y1, y2)
import PSD3.Internal.FFI (keyIsID_, simdrag_)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..), SelectionAttribute)
import PSD3.Internal.Types (D3Selection_, Element(..))
import PSD3v2.Simulation.Update (RenderCallbacks)

-- Helper type for swizzled links (what we get after D3 swizzles them)
type SpagoSwizzledLink = { source :: SpagoSimNode, target :: SpagoSimNode, linktype :: LinkType }

-- Helper functions for links (phantom type friendly)
linkClass :: SpagoSwizzledLink -> String
linkClass l = show l.linktype

linkColor :: SpagoSwizzledLink -> String
linkColor l = case l.linktype of
  P2P -> "red"
  M2P -> "blue"
  M2M_Graph -> "green"
  M2M_Tree -> "orange"

-- | Create a CSS class attribute from tags
-- | Tags from the tag map are joined with spaces and added as CSS classes
makeTagClassesAttr :: Maybe (Map.Map NodeID (Set.Set String)) -> SelectionAttribute SpagoSimNode
makeTagClassesAttr Nothing = classed ""
makeTagClassesAttr (Just tagMap) = classed \(d :: SpagoSimNode) ->
  let tags = fromMaybe Set.empty $ Map.lookup d.id tagMap
  in String.joinWith " " $ Array.fromFoldable tags

-- | Render callbacks for Spago visualization
-- |
-- | Implements the complex DOM structure:
-- | - Outer Group (positioned via transform)
-- | - Inner Circle (visual representation)
-- | - Inner Text (label)
-- |
-- | This shows how to use the generic updateSimulation with multi-level DOM structures.
spagoRenderCallbacks :: forall m.
  Monad m =>
  SelectionM D3Selection_ m =>
  RenderCallbacks SpagoSceneAttributes D3Selection_ m SpagoSimNode
spagoRenderCallbacks = {
  -- Node rendering: Group → Circle + Text structure
  onNodeEnter: \enterSel attrs -> do
    let tagClassesAttr = makeTagClassesAttr attrs.tagMap
        enterAttrsWithTags = tagClassesAttr : enterAttrs

    -- Create group with position and classes
    nodeEnter <- appendTo enterSel Group enterAttrsWithTags

    -- Add circle child
    _ <- appendTo nodeEnter Circle attrs.circles

    -- Add text child
    _ <- appendTo nodeEnter Text attrs.labels

    -- Add drag behavior to the group
    _ <- nodeEnter `on` Drag (CustomDrag "spago" simdrag_)

    pure nodeEnter

  , onNodeUpdate: \updateSel attrs -> do
      let tagClassesAttr = makeTagClassesAttr attrs.tagMap
          updateAttrsWithTags = tagClassesAttr : updateAttrs

      -- Update group attributes (position, classes)
      setAttributes updateSel updateAttrsWithTags

      -- Update circle child attributes
      updateCircles <- selectUnder updateSel (show Circle)
      setAttributes updateCircles attrs.circles

      -- Update text child attributes
      updateLabels <- selectUnder updateSel (show Text)
      setAttributes updateLabels attrs.labels

  , onNodeExit: \exitSel ->
      setAttributes exitSel [ remove ]

  -- Link rendering: Simple line elements
  , onLinkEnter: \enterSel attrs -> do
      linkEnter <- appendTo enterSel Line [
          classed (\(d :: SpagoSwizzledLink) -> linkClass d)
        , strokeColor (\(d :: SpagoSwizzledLink) -> linkColor d)
        , classed "enter"
        ]
      pure linkEnter

  , onLinkUpdate: \updateSel attrs ->
      setAttributes updateSel [ classed "update" ]

  , onLinkExit: \exitSel ->
      setAttributes exitSel [ remove ]

  -- Tick function attributes
  , nodeTickAttrs: \attrs ->
      [ transform' \(d :: SpagoSimNode) -> translateNode d ]

  , linkTickAttrs:
      [ x1 (\(d :: SpagoSwizzledLink) -> d.source.x)
      , y1 (\(d :: SpagoSwizzledLink) -> d.source.y)
      , x2 (\(d :: SpagoSwizzledLink) -> d.target.x)
      , y2 (\(d :: SpagoSwizzledLink) -> d.target.y)
      ]
}
