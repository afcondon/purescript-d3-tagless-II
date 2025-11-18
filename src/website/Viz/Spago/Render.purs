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
import PSD3.Data.Node (D3Link_Swizzled, NodeID)
import PSD3.Internal.FFI (keyIsID_, simdrag_)
import Unsafe.Coerce (unsafeCoerce)
import PSD3v2.Attribute.Types (class_, stroke, transform, x1, x2, y1, y2, Attribute)
import PSD3v2.Behavior.Types (Behavior(..), simulationDrag)
import PSD3v2.Capabilities.Selection (class SelectionM, append, appendChild, on, openSelection, remove, setAttrs)
import PSD3v2.Interpreter.D3v2 (D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SBound, SPending, SExiting)
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
  -- v2 approach: Create group, then use openSelection to add children
  onNodeEnter: \enterSel attrs -> do
    let tagClassesAttr = makeTagClassesAttr attrs.tagMap
        enterAttrsWithTags = tagClassesAttr : enterAttrs

    -- Create group with position and classes
    nodeEnter <- append Group enterAttrsWithTags enterSel

    -- Get empty selection for adding circle children
    circleParent <- openSelection nodeEnter "g"
    _ <- appendChild Circle attrs.circles circleParent

    -- Get empty selection for adding text children
    textParent <- openSelection nodeEnter "g"
    _ <- appendChild Text attrs.labels textParent

    -- Add drag behavior to the group
    _ <- on (Drag (simulationDrag "spago")) nodeEnter

    pure nodeEnter

  , onNodeUpdate: \updateSel attrs -> do
      let tagClassesAttr = makeTagClassesAttr attrs.tagMap
          updateAttrsWithTags = tagClassesAttr : updateAttrs

      -- Update group attributes (position, classes)
      -- Note: Circle and text children attributes are set once during enter
      -- and don't need per-update changes in this visualization
      _ <- setAttrs updateAttrsWithTags updateSel

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

  -- Tick function attributes
  , nodeTickAttrs: \attrs ->
      [ transform \(d :: SpagoSimNode) -> translateNode d ]

  , linkTickAttrs:
      [ x1 (\(d :: D3Link_Swizzled) -> (unsafeCoerce d :: SpagoSwizzledLink).source.x)
      , y1 (\(d :: D3Link_Swizzled) -> (unsafeCoerce d :: SpagoSwizzledLink).source.y)
      , x2 (\(d :: D3Link_Swizzled) -> (unsafeCoerce d :: SpagoSwizzledLink).target.x)
      , y2 (\(d :: D3Link_Swizzled) -> (unsafeCoerce d :: SpagoSwizzledLink).target.y)
      ]
}
