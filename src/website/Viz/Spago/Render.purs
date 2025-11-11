module D3.Viz.Spago.Render where

import Prelude

import Data.Array ((:))

import PSD3.Attributes (DatumFn(..), DatumFnI(..), unwrapDatumFn)
import PSD3.Internal.Attributes.Sugar (classed, remove, strokeColor, transform', x1, x2, y1, y2)
import PSD3.Internal.Types (D3Selection_, Element(..))
import D3.Viz.Spago.Draw.Attributes (SpagoSceneAttributes, enterAttrs, updateAttrs)
import D3.Viz.Spago.Model (datum_, link_)
import PSD3.Internal.FFI (keyIsID_, simdrag_)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..), SelectionAttribute)
import PSD3.Capabilities.Selection (class SelectionM, appendTo, on, selectUnder, setAttributes)
import PSD3.Simulation.Update (RenderCallbacks)
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.Array as Array
import PSD3.Data.Node (NodeID)
import Data.Maybe (fromMaybe)
import PSD3.Internal.Types (Datum_)

-- | Create a CSS class attribute from tags
-- | Tags from the tag map are joined with spaces and added as CSS classes
makeTagClassesAttr :: forall d. Maybe (Map.Map NodeID (Set.Set String)) -> SelectionAttribute d
makeTagClassesAttr Nothing = classed ""
makeTagClassesAttr (Just tagMap) = classed (DatumFn tagClassesFn)
  where
    tagClassesFn :: Datum_ -> String
    tagClassesFn d =
      let nodeId = datum_.id d
          tags = fromMaybe Set.empty $ Map.lookup nodeId tagMap
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
  RenderCallbacks SpagoSceneAttributes D3Selection_ m
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
      linkEnter <- appendTo enterSel Line [ classed (DatumFn link_.linkClass), strokeColor (DatumFn link_.color), classed "enter" ]
      pure linkEnter

  , onLinkUpdate: \updateSel attrs ->
      setAttributes updateSel [ classed "update" ]

  , onLinkExit: \exitSel ->
      setAttributes exitSel [ remove ]

  -- Tick function attributes
  , nodeTickAttrs: \attrs ->
      [ transform' (unwrapDatumFn (DatumFn datum_.translateNode)) ]

  , linkTickAttrs:
      [ x1 (DatumFn (_.x <<< link_.source))
      , y1 (DatumFn (_.y <<< link_.source))
      , x2 (DatumFn (_.x <<< link_.target))
      , y2 (DatumFn (_.y <<< link_.target))
      ]
}
