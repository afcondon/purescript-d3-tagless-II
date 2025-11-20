module D3.Viz.Spago.Render where

import Prelude

import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, enterAttrs, updateAttrs, translateNode)
import D3.Viz.Spago.Files (LinkType(..))
import D3.Viz.Spago.Model (SpagoSimNode)
import D3.Viz.Spago.Tooltip (showNodeTooltip, hideNodeTooltip)
import D3.Viz.Spago.Highlight (highlightConnected_, clearHighlights_)
import Data.Array ((:))
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Data.Set as Set
import Data.String as String
import PSD3.Data.Node (D3Link_Swizzled, NodeID)
import PSD3.Internal.FFI (keyIsID_, simdrag_)
import Unsafe.Coerce (unsafeCoerce)
import PSD3v2.Attribute.Types (class_, stroke, transform, x1, x2, y1, y2, Attribute)
import PSD3v2.Behavior.Types (Behavior(..), simulationDrag, onClickWithDatum, onMouseEnterWithInfo, onMouseLeave)
import PSD3v2.Capabilities.Selection (class SelectionM, append, appendChild, appendChildInheriting, on, openSelection, remove, setAttrs)
import PSD3v2.Interpreter.D3v2 (D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBoundOwns, SBoundInherits, SEmpty, SPending, SExiting)
import PSD3v2.Simulation.Update (RenderCallbacks)
import Web.DOM.Element (Element)

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
makeTagClassesAttr :: Maybe (Map.Map NodeID (Set.Set String)) -> Attribute SpagoSimNode
makeTagClassesAttr Nothing = class_ ""
makeTagClassesAttr (Just tagMap) = class_ \(d :: SpagoSimNode) ->
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
  SelectionM D3v2Selection_ m =>
  RenderCallbacks SpagoSceneAttributes D3v2Selection_ m SpagoSimNode
spagoRenderCallbacks = {
  -- Node rendering: Group with Circle + Text children
  -- v2 approach: Create group, then use appendChildInheriting to add children
  onNodeEnter: \enterSel attrs -> do
    let tagClassesAttr = makeTagClassesAttr attrs.tagMap
        groupAttrs = tagClassesAttr : transform translateNode : []
        circleAttrs = attrs.circles
        labelAttrs = attrs.labels

    -- Create outer group positioned via transform
    groupEnter <- append Group groupAttrs enterSel

    -- Add circle child that inherits the parent's data
    circleEnter <- appendChildInheriting Circle circleAttrs groupEnter

    -- Add text label child that also inherits the parent's data
    labelEnter <- appendChildInheriting Text labelAttrs groupEnter

    -- Add drag behavior to group
    _ <- on (Drag (simulationDrag "spago")) groupEnter

    -- Add click behavior if handler provided
    case attrs.nodeClick of
      Just handler -> void $ on (onClickWithDatum handler) groupEnter
      Nothing -> pure unit

    -- Add tooltip and highlight hover behavior
    _ <- on (onMouseEnterWithInfo \info -> do
      showNodeTooltip info.datum info.pageX info.pageY
      highlightConnected_ info.datum
    ) groupEnter
    _ <- on (onMouseLeave \_ -> do
      hideNodeTooltip
      clearHighlights_
    ) groupEnter

    pure groupEnter

  , onNodeUpdate: \updateSel attrs -> do
      let tagClassesAttr = makeTagClassesAttr attrs.tagMap
          groupAttrs = tagClassesAttr : transform translateNode : []

      -- Update group attributes (tag classes and position)
      _ <- setAttrs groupAttrs updateSel

      -- TODO: Optionally update circle/text children via openSelection if needed

      pure unit

  , onNodeExit: \exitSel ->
      remove exitSel

  -- Link rendering: Simple line elements
  , onLinkEnter: \enterSel attrs -> do
      linkEnter <- append Line [
          class_ (\(d :: D3Link_Swizzled) -> linkClass (unsafeCoerce d))
        , stroke (\(d :: D3Link_Swizzled) -> linkColor (unsafeCoerce d))
        , class_ "enter"
        ] enterSel
      pure linkEnter

  , onLinkUpdate: \updateSel attrs -> do
      _ <- setAttrs [ class_ "update" ] updateSel
      pure unit

  , onLinkExit: \exitSel ->
      remove exitSel

  -- Tick function attributes - update group position on each tick
  , nodeTickAttrs: \_attrs ->
      [ transform \(d :: SpagoSimNode) -> translateNode d ]

  , linkTickAttrs:
      [ x1 (\(d :: D3Link_Swizzled) -> (unsafeCoerce d :: SpagoSwizzledLink).source.x)
      , y1 (\(d :: D3Link_Swizzled) -> (unsafeCoerce d :: SpagoSwizzledLink).source.y)
      , x2 (\(d :: D3Link_Swizzled) -> (unsafeCoerce d :: SpagoSwizzledLink).target.x)
      , y2 (\(d :: D3Link_Swizzled) -> (unsafeCoerce d :: SpagoSwizzledLink).target.y)
      ]
}
