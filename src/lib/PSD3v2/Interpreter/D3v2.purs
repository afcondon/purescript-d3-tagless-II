module PSD3v2.Interpreter.D3v2
  ( D3v2Selection_
  , D3v2M
  , D3v2SimM
  , runD3v2M
  , runD3v2SimM
  , execD3v2SimM
  , evalD3v2SimM
  , reselectD3v2
  , queryAllD3v2
  ) where

import Prelude

import Control.Monad.State (class MonadState, StateT, runStateT, evalStateT, execStateT, modify_, get)
import Data.Array as Array
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple, snd, fst)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafePartial)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force(..), _name, SimVariable(..), _handle)
import PSD3.Internal.Simulation.Functions as SimFn
import PSD3.Internal.FFI (onTick_, getNodes_, getLinksFromSimulation_)
import Data.Lens (use, (.~), view)
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Lens.At (at)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import PSD3v2.Attribute.Types (Attribute(..), AttributeName(..), AttributeValue(..))
import PSD3v2.Capabilities.Selection (class SelectionM)
import PSD3v2.Capabilities.Simulation (class SimulationM, class SimulationM2, Step(..))
import PSD3v2.Capabilities.Transition (class TransitionM)
import PSD3v2.Selection.Operations as Ops
import PSD3v2.Selection.Query as Query
import PSD3v2.Selection.Types (Selection(..), SelectionImpl(..), SBoundOwns, SBoundInherits, SEmpty, JoinResult(..))
import PSD3v2.Transition.FFI as TransitionFFI
import Web.DOM.Element (Element)
import Web.DOM.Element as Element

-- | Selection type for D3v2 interpreter
-- |
-- | This is just a newtype wrapper around PSD3v2.Selection.Types.Selection
-- | to distinguish it from other interpreter's selection types.
newtype D3v2Selection_ (state :: Type) (parent :: Type) (datum :: Type)
  = D3v2Selection_ (Selection state parent datum)

-- | The D3v2 interpreter monad (without simulation state)
-- |
-- | Wraps Effect to allow for DOM manipulation.
newtype D3v2M a = D3v2M (Effect a)

derive newtype instance Functor D3v2M
derive newtype instance Apply D3v2M
derive newtype instance Applicative D3v2M
derive newtype instance Bind D3v2M
derive newtype instance Monad D3v2M
derive newtype instance MonadEffect D3v2M

-- | Run the D3v2 interpreter
runD3v2M :: D3v2M ~> Effect
runD3v2M (D3v2M eff) = eff

-- | The D3v2 simulation monad (with simulation state)
-- |
-- | Wraps Aff (not Effect) to support async operations like scene transitions.
-- | StateT provides simulation state management.
-- | Required for force-directed graphs with transitions.
newtype D3v2SimM row d a = D3v2SimM (StateT { simulation :: D3SimulationState_ d | row } Aff a)

derive newtype instance Functor (D3v2SimM row d)
derive newtype instance Apply (D3v2SimM row d)
derive newtype instance Applicative (D3v2SimM row d)
derive newtype instance Bind (D3v2SimM row d)
derive newtype instance Monad (D3v2SimM row d)
derive newtype instance MonadEffect (D3v2SimM row d)
derive newtype instance MonadAff (D3v2SimM row d)
derive newtype instance MonadState { simulation :: D3SimulationState_ d | row } (D3v2SimM row d)

-- | Run the D3v2 simulation interpreter and return result + final state
runD3v2SimM :: forall a d row. { simulation :: D3SimulationState_ d | row } -> D3v2SimM row d a -> Aff (Tuple a { simulation :: D3SimulationState_ d | row })
runD3v2SimM state (D3v2SimM st) = runStateT st state

-- | Run the D3v2 simulation interpreter and return only the final state
execD3v2SimM :: forall a d row. { simulation :: D3SimulationState_ d | row } -> D3v2SimM row d a -> Aff { simulation :: D3SimulationState_ d | row }
execD3v2SimM state (D3v2SimM st) = map snd $ runStateT st state

-- | Run the D3v2 simulation interpreter and return only the result
evalD3v2SimM :: forall a d row. { simulation :: D3SimulationState_ d | row } -> D3v2SimM row d a -> Aff a
evalD3v2SimM state (D3v2SimM st) = map fst $ runStateT st state

-- | SelectionM instance for D3v2 interpreter
-- |
-- | Delegates all operations to PSD3v2.Selection.Operations,
-- | which uses the phantom types with unsafePartial for safe pattern matching.
instance SelectionM D3v2Selection_ D3v2M where

  select selector = D3v2M do
    sel <- Ops.select selector
    pure $ D3v2Selection_ sel

  selectAll selector (D3v2Selection_ sel) = D3v2M do
    result <- Ops.selectAll selector sel
    pure $ D3v2Selection_ result

  openSelection (D3v2Selection_ sel) selector = D3v2M do
    result <- Ops.selectAll selector sel
    pure $ D3v2Selection_ result

  renderData elemType foldableData selector (D3v2Selection_ emptySelection) enterAttrs updateAttrs exitAttrs = D3v2M do
    result <- Ops.renderData elemType foldableData selector emptySelection enterAttrs updateAttrs exitAttrs
    pure $ D3v2Selection_ result

  appendData elemType foldableData attrs (D3v2Selection_ emptySelection) = D3v2M do
    result <- Ops.appendData elemType foldableData attrs emptySelection
    pure $ D3v2Selection_ result

  joinData foldableData selector (D3v2Selection_ emptySelection) = D3v2M do
    JoinResult { enter, update, exit } <- Ops.joinData foldableData selector emptySelection
    pure $ JoinResult
      { enter: D3v2Selection_ enter
      , update: D3v2Selection_ update
      , exit: D3v2Selection_ exit
      }

  joinDataWithKey foldableData keyFn selector (D3v2Selection_ emptySelection) = D3v2M do
    JoinResult { enter, update, exit } <- Ops.joinDataWithKey foldableData keyFn selector emptySelection
    pure $ JoinResult
      { enter: D3v2Selection_ enter
      , update: D3v2Selection_ update
      , exit: D3v2Selection_ exit
      }

  updateJoin (D3v2Selection_ emptySelection) _elemType foldableData keyFn selector = D3v2M do
    JoinResult { enter, update, exit } <- Ops.joinDataWithKey foldableData keyFn selector emptySelection
    pure $ JoinResult
      { enter: D3v2Selection_ enter
      , update: D3v2Selection_ update
      , exit: D3v2Selection_ exit
      }

  append elemType attrs (D3v2Selection_ pendingSelection) = D3v2M do
    result <- Ops.append elemType attrs pendingSelection
    pure $ D3v2Selection_ result

  setAttrs attrs (D3v2Selection_ boundSelection) = D3v2M do
    result <- Ops.setAttrs attrs boundSelection
    pure $ D3v2Selection_ result

  setAttrsExit attrs (D3v2Selection_ exitingSelection) = D3v2M do
    result <- Ops.setAttrsExit attrs exitingSelection
    pure $ D3v2Selection_ result

  remove (D3v2Selection_ exitingSelection) = D3v2M do
    Ops.remove exitingSelection

  clear selector = D3v2M do
    Ops.clear selector

  merge (D3v2Selection_ sel1) (D3v2Selection_ sel2) = D3v2M do
    result <- Ops.merge sel1 sel2
    pure $ D3v2Selection_ result

  appendChild elemType attrs (D3v2Selection_ emptySelection) = D3v2M do
    result <- Ops.appendChild elemType attrs emptySelection
    pure $ D3v2Selection_ result

  appendChildInheriting elemType attrs (D3v2Selection_ boundSelection) = D3v2M do
    result <- Ops.appendChildInheriting elemType attrs boundSelection
    pure $ D3v2Selection_ result

  on behavior (D3v2Selection_ selection) = D3v2M do
    result <- Ops.on behavior selection
    pure $ D3v2Selection_ result

  renderTree (D3v2Selection_ parent) tree = D3v2M do
    selectionsMap <- Ops.renderTree parent tree
    pure $ map D3v2Selection_ selectionsMap

-- | TransitionM instance for D3v2 interpreter
-- |
-- | Implements animated transitions using D3's transition engine.
-- | Applies transitions to each element in the bound selection.
instance TransitionM D3v2Selection_ D3v2M where

  withTransition config (D3v2Selection_ selection) attrs = D3v2M do
    -- Extract elements, data, and indices from the bound selection
    let { elements, data: datumArray, indices } = unsafePartial case selection of
          Selection (BoundSelection r) -> r

    -- Get transition configuration
    let Milliseconds duration = config.duration

    -- Apply transition to each element with its corresponding datum and index
    let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
    paired # traverseWithIndex_ \arrayIndex { datum, element } -> do
      -- Use logical index from indices array if present, otherwise use array index
      let logicalIndex = case indices of
            Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
            Nothing -> arrayIndex

      -- Create a D3 transition for this element
      transition <- TransitionFFI.createTransition_
        duration
        (TransitionFFI.maybeMillisecondsToNullable config.delay)
        (TransitionFFI.maybeEasingToNullable config.easing)
        element

      -- Apply each attribute to the transition
      attrs # traverse_ \attr -> case attr of
        StaticAttr (AttributeName name) value ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

        DataAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

        IndexedAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum logicalIndex)) transition

  withTransitionExit config (D3v2Selection_ selection) attrs = D3v2M do
    -- Extract elements and data from the exiting selection
    let { elements, data: datumArray } = unsafePartial case selection of
          Selection (ExitingSelection r) -> r

    -- Get transition configuration
    let Milliseconds duration = config.duration

    -- Apply transition to each element with its corresponding datum and index
    let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
    paired # traverseWithIndex_ \index { datum, element } -> do
      -- Create a D3 transition for this element
      transition <- TransitionFFI.createTransition_
        duration
        (TransitionFFI.maybeMillisecondsToNullable config.delay)
        (TransitionFFI.maybeEasingToNullable config.easing)
        element

      -- Apply each attribute to the transition
      attrs # traverse_ \attr -> case attr of
        StaticAttr (AttributeName name) value ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

        DataAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

        IndexedAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum index)) transition

      -- Remove element after transition completes (D3 pattern: transition.remove())
      TransitionFFI.transitionRemove_ transition

  withTransitionStaggered config delayFn (D3v2Selection_ selection) attrs = D3v2M do
    -- Extract elements, data, and indices from the bound selection
    let { elements, data: datumArray, indices } = unsafePartial case selection of
          Selection (BoundSelection r) -> r

    -- Get transition configuration
    let Milliseconds duration = config.duration

    -- Apply transition to each element with its corresponding datum and index
    let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
    paired # traverseWithIndex_ \arrayIndex { datum, element } -> do
      -- Use logical index from indices array if present, otherwise use array index
      let logicalIndex = case indices of
            Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
            Nothing -> arrayIndex

      -- Compute the delay for this element using the delay function
      let Milliseconds elementDelay = delayFn datum logicalIndex

      -- Create a D3 transition for this element with computed delay
      transition <- TransitionFFI.createTransition_
        duration
        (toNullable (Just elementDelay))
        (TransitionFFI.maybeEasingToNullable config.easing)
        element

      -- Apply each attribute to the transition
      attrs # traverse_ \attr -> case attr of
        StaticAttr (AttributeName name) value ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

        DataAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

        IndexedAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum logicalIndex)) transition

  withTransitionExitStaggered config delayFn (D3v2Selection_ selection) attrs = D3v2M do
    -- Extract elements and data from the exiting selection
    let { elements, data: datumArray } = unsafePartial case selection of
          Selection (ExitingSelection r) -> r

    -- Get transition configuration
    let Milliseconds duration = config.duration

    -- Apply transition to each element with its corresponding datum and index
    let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
    paired # traverseWithIndex_ \index { datum, element } -> do
      -- Compute the delay for this element using the delay function
      let Milliseconds elementDelay = delayFn datum index

      -- Create a D3 transition for this element with computed delay
      transition <- TransitionFFI.createTransition_
        duration
        (toNullable (Just elementDelay))
        (TransitionFFI.maybeEasingToNullable config.easing)
        element

      -- Apply each attribute to the transition
      attrs # traverse_ \attr -> case attr of
        StaticAttr (AttributeName name) value ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

        DataAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

        IndexedAttr (AttributeName name) f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum index)) transition

      -- Remove element after transition completes (D3 pattern: transition.remove())
      TransitionFFI.transitionRemove_ transition

-- Helper function to convert AttributeValue to String
attributeValueToString :: AttributeValue -> String
attributeValueToString (StringValue s) = s
attributeValueToString (NumberValue n) = show n
attributeValueToString (BooleanValue b) = show b

-- =============================================================================
-- D3v2SimM Instances (SelectionM + TransitionM + SimulationM2)
-- =============================================================================

-- | SelectionM instance for D3v2SimM
-- |
-- | Same as D3v2M but lifted through StateT
instance SelectionM D3v2Selection_ (D3v2SimM row d) where
  select selector = liftEffect $ do
    sel <- Ops.select selector
    pure $ D3v2Selection_ sel

  appendChild elemType attrs (D3v2Selection_ parent) = liftEffect $ do
    child <- Ops.appendChild elemType attrs parent
    pure $ D3v2Selection_ child

  appendChildInheriting elemType attrs (D3v2Selection_ boundSelection) = liftEffect $ do
    child <- Ops.appendChildInheriting elemType attrs boundSelection
    pure $ D3v2Selection_ child

  joinData foldable tag (D3v2Selection_ parent) = liftEffect $ do
    JoinResult result <- Ops.joinData foldable tag parent
    pure $ JoinResult
      { enter: D3v2Selection_ result.enter
      , update: D3v2Selection_ result.update
      , exit: D3v2Selection_ result.exit
      }

  joinDataWithKey foldableData keyFn selector (D3v2Selection_ emptySelection) = liftEffect $ do
    JoinResult { enter, update, exit } <- Ops.joinDataWithKey foldableData keyFn selector emptySelection
    pure $ JoinResult
      { enter: D3v2Selection_ enter
      , update: D3v2Selection_ update
      , exit: D3v2Selection_ exit
      }

  append elemType attrs (D3v2Selection_ pending) = liftEffect $ do
    bound <- Ops.append elemType attrs pending
    pure $ D3v2Selection_ bound

  setAttrs attrs (D3v2Selection_ bound) = liftEffect $ do
    bound' <- Ops.setAttrs attrs bound
    pure $ D3v2Selection_ bound'

  setAttrsExit attrs (D3v2Selection_ exiting) = liftEffect $ do
    exiting' <- Ops.setAttrsExit attrs exiting
    pure $ D3v2Selection_ exiting'

  remove (D3v2Selection_ exiting) = liftEffect $
    Ops.remove exiting

  clear selector = liftEffect $
    Ops.clear selector

  selectAll tag (D3v2Selection_ parent) = liftEffect $ do
    selection <- Ops.selectAll tag parent
    pure $ D3v2Selection_ selection

  openSelection (D3v2Selection_ parent) tag = liftEffect $ do
    selection <- Ops.selectAll tag parent
    pure $ D3v2Selection_ selection

  updateJoin (D3v2Selection_ emptySelection) _elemType foldableData keyFn selector = liftEffect $ do
    JoinResult { enter, update, exit } <- Ops.joinDataWithKey foldableData keyFn selector emptySelection
    pure $ JoinResult
      { enter: D3v2Selection_ enter
      , update: D3v2Selection_ update
      , exit: D3v2Selection_ exit
      }

  renderData elemType foldable tag (D3v2Selection_ parent) enterFn updateFn exitFn = liftEffect $ do
    bound <- Ops.renderData elemType foldable tag parent enterFn updateFn exitFn
    pure $ D3v2Selection_ bound

  appendData elemType foldableData attrs (D3v2Selection_ emptySelection) = liftEffect $ do
    bound <- Ops.appendData elemType foldableData attrs emptySelection
    pure $ D3v2Selection_ bound

  merge (D3v2Selection_ sel1) (D3v2Selection_ sel2) = liftEffect $ do
    merged <- Ops.merge sel1 sel2
    pure $ D3v2Selection_ merged

  on behavior (D3v2Selection_ selection) = do
    -- Get simulation state from StateT context
    state <- get
    -- Call onWithSimulation with the simulation state
    result <- liftEffect $ Ops.onWithSimulation behavior state.simulation selection
    pure $ D3v2Selection_ result

  renderTree (D3v2Selection_ parent) tree = liftEffect $ do
    selectionsMap <- Ops.renderTree parent tree
    pure $ map D3v2Selection_ selectionsMap

-- | TransitionM instance for D3v2SimM
-- |
-- | Same as D3v2M but lifted through StateT
instance TransitionM D3v2Selection_ (D3v2SimM row d) where
  withTransition transConfig (D3v2Selection_ selection) attrs = liftEffect $
    case selection of
      Selection (BoundSelection { elements, data: datumArray, indices }) -> do
        let Milliseconds duration = transConfig.duration
        let paired = Array.zipWith (\element datum -> { element, datum }) elements datumArray

        paired # traverseWithIndex_ \arrayIndex { datum, element } -> do
          let logicalIndex = case indices of
                Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
                Nothing -> arrayIndex

          transition <- TransitionFFI.createTransition_
            duration
            (TransitionFFI.maybeMillisecondsToNullable transConfig.delay)
            (TransitionFFI.maybeEasingToNullable transConfig.easing)
            element

          attrs # traverse_ \attr -> case attr of
            StaticAttr (AttributeName name) value ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

            DataAttr (AttributeName name) fn ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString $ fn datum) transition

            IndexedAttr (AttributeName name) fn ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString $ fn datum logicalIndex) transition

      _ -> pure unit

  withTransitionExit transConfig (D3v2Selection_ selection) attrs = liftEffect $
    case selection of
      Selection (ExitingSelection { elements }) -> do
        let Milliseconds duration = transConfig.duration

        elements # traverse_ \element -> do
          transition <- TransitionFFI.createTransition_
            duration
            (TransitionFFI.maybeMillisecondsToNullable transConfig.delay)
            (TransitionFFI.maybeEasingToNullable transConfig.easing)
            element

          attrs # traverse_ \attr -> case attr of
            StaticAttr (AttributeName name) value ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

            _ -> pure unit  -- Exit selections don't have data, so only static attrs work

          TransitionFFI.transitionRemove_ transition

      _ -> pure unit

  withTransitionStaggered transConfig delayFn (D3v2Selection_ selection) attrs = liftEffect $
    case selection of
      Selection (BoundSelection { elements, data: datumArray, indices }) -> do
        let Milliseconds duration = transConfig.duration
        let paired = Array.zipWith (\element datum -> { element, datum }) elements datumArray

        paired # traverseWithIndex_ \arrayIndex { datum, element } -> do
          let logicalIndex = case indices of
                Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
                Nothing -> arrayIndex

          -- Compute the delay for this element using the delay function
          let Milliseconds elementDelay = delayFn datum logicalIndex

          transition <- TransitionFFI.createTransition_
            duration
            (toNullable (Just elementDelay))
            (TransitionFFI.maybeEasingToNullable transConfig.easing)
            element

          attrs # traverse_ \attr -> case attr of
            StaticAttr (AttributeName name) value ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

            DataAttr (AttributeName name) fn ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString $ fn datum) transition

            IndexedAttr (AttributeName name) fn ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString $ fn datum logicalIndex) transition

      _ -> pure unit

  withTransitionExitStaggered transConfig delayFn (D3v2Selection_ selection) attrs = liftEffect $
    case selection of
      Selection (ExitingSelection { elements, data: datumArray }) -> do
        let Milliseconds duration = transConfig.duration
        let paired = Array.zipWith (\element datum -> { element, datum }) elements datumArray

        paired # traverseWithIndex_ \index { datum, element } -> do
          -- Compute the delay for this element using the delay function
          let Milliseconds elementDelay = delayFn datum index

          transition <- TransitionFFI.createTransition_
            duration
            (toNullable (Just elementDelay))
            (TransitionFFI.maybeEasingToNullable transConfig.easing)
            element

          attrs # traverse_ \attr -> case attr of
            StaticAttr (AttributeName name) value ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

            DataAttr (AttributeName name) fn ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString $ fn datum) transition

            IndexedAttr (AttributeName name) fn ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString $ fn datum index) transition

          TransitionFFI.transitionRemove_ transition

      _ -> pure unit

-- =============================================================================
-- SimulationM Instance (Force Simulations)
-- =============================================================================

-- | Helper function to apply PSD3v2 attributes to elements (synchronous, for tick callbacks)
-- | Used by tick functions to update DOM on each simulation frame
-- | Returns Unit directly (not Effect Unit) because D3 tick callbacks must be synchronous
applyAttributeSync :: forall datum. Element -> datum -> Int -> Attribute datum -> Unit
applyAttributeSync element datum index attr = case attr of
  StaticAttr (AttributeName name) value ->
    setAttributeSync_ name (attributeValueToString value) element

  DataAttr (AttributeName name) f ->
    setAttributeSync_ name (attributeValueToString (f datum)) element

  IndexedAttr (AttributeName name) f ->
    setAttributeSync_ name (attributeValueToString (f datum index)) element

-- | FFI for setting attributes synchronously (no Effect wrapper)
-- | Used in D3 tick callbacks which must be synchronous
foreign import setAttributeSync_ :: String -> String -> Element -> Unit

-- | SimulationM instance for D3v2SimM
-- |
-- | Delegates to existing PSD3.Internal.Simulation.Functions
-- | but uses PSD3v2's Attribute system for tick callbacks
-- |
-- | Note: D3v2Selection_ is partially applied to (SBound) (Element) to match
-- | the expected kind Type -> Type for SimulationM's sel parameter
instance SimulationM (D3v2Selection_ SBoundOwns Element) (D3v2SimM row d) where
  init config = do
    -- 1. Initialize force library in simulation state
    let forcesMap = Map.fromFoldable $ config.forces <#> \f ->
          Tuple (view _name f) (unsafeCoerce f)
    modify_ \state -> state { simulation = state.simulation #
      (_Newtype <<< prop (Proxy :: Proxy "forceLibrary")) .~ forcesMap }

    -- 2. Set up nodes first (must come before forces)
    nodesInSim <- SimFn.simulationSetNodes config.nodes

    -- 3. Activate specified forces (must come BEFORE setting links!)
    SimFn.simulationActualizeForces config.activeForces

    -- 4. Set links (returns simulation-enhanced data)
    linksInSim <- SimFn.simulationSetLinks config.links config.nodes config.keyFn

    -- 5. Set configuration variables (except Alpha)
    SimFn.simulationSetVariable $ AlphaTarget config.config.alphaTarget
    SimFn.simulationSetVariable $ AlphaMin config.config.alphaMin
    SimFn.simulationSetVariable $ AlphaDecay config.config.alphaDecay
    SimFn.simulationSetVariable $ VelocityDecay config.config.velocityDecay

    -- 6. Add initial tick functions from config
    handle <- use _handle
    _ <- (Map.toUnfoldable config.ticks :: Array (Tuple _ _)) # traverse_ \(Tuple label step) ->
      case step of
        Step (D3v2Selection_ selection) attrs -> do
          -- Extract elements and data from bound selection
          let { elements, data: datumArray, indices } = unsafePartial case selection of
                Selection (BoundSelection r) -> r

          -- Create tick callback that applies PSD3v2 attributes
          let makeTick _ =
                let paired = Array.zipWith (\d e -> { datum: d, element: e }) datumArray elements
                    _ = Array.mapWithIndex (\arrayIndex { datum, element } ->
                          let logicalIndex = case indices of
                                Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
                                Nothing -> arrayIndex
                              _ = attrs <#> \attr -> applyAttributeSync element datum logicalIndex attr
                          in unit
                        ) paired
                in unit

          -- Register tick callback with D3
          pure $ onTick_ handle label makeTick

    -- 7. Return enhanced data for joining to DOM
    pure { nodes: nodesInSim, links: linksInSim }

  addTickFunction label (Step (D3v2Selection_ selection) attrs) = do
    handle <- use _handle

    -- Extract elements and data from bound selection
    let { elements, data: datumArray, indices } = unsafePartial case selection of
          Selection (BoundSelection r) -> r

    -- Create tick callback that applies PSD3v2 attributes
    let makeTick _ =
          let paired = Array.zipWith (\d e -> { datum: d, element: e }) datumArray elements
              _ = Array.mapWithIndex (\arrayIndex { datum, element } ->
                    let logicalIndex = case indices of
                          Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
                          Nothing -> arrayIndex
                        _ = attrs <#> \attr -> applyAttributeSync element datum logicalIndex attr
                    in unit
                  ) paired
          in unit

    -- Register tick callback with D3
    pure $ onTick_ handle label makeTick

  removeTickFunction label = do
    handle <- use _handle
    pure $ onTick_ handle label (const unit)

  start = SimFn.simulationStart

  stop = SimFn.simulationStop

-- | SimulationM2 instance for D3v2SimM (dynamic simulations with update)
-- |
-- | Extends SimulationM with update and reheat capabilities
instance SimulationM2 (D3v2Selection_ SBoundOwns Element) (D3v2SimM row d) where
  update config = do
    -- 1. Update nodes if provided (or get current nodes)
    nodesBeforeFilter <- case config.nodes of
      Just newNodes -> SimFn.simulationSetNodes newNodes
      Nothing -> do
        handle <- use _handle
        let opaqueNodes = getNodes_ handle
        pure $ unsafeCoerce opaqueNodes

    -- 2. Apply node filter if provided
    let nodesInSim = case config.nodeFilter of
          Just filterFn -> Array.filter filterFn nodesBeforeFilter
          Nothing -> nodesBeforeFilter

    -- 3. If filtering occurred, update simulation with filtered nodes
    _ <- case config.nodeFilter of
      Just _ -> SimFn.simulationSetNodes nodesInSim
      Nothing -> pure nodesInSim

    -- 4. Update active forces if provided
    _ <- case config.activeForces of
      Just forces -> SimFn.simulationActualizeForces forces
      Nothing -> pure unit

    -- 5. Update configuration variables if provided
    _ <- case config.config of
      Just vars -> do
        SimFn.simulationSetVariable $ AlphaTarget vars.alphaTarget
        SimFn.simulationSetVariable $ AlphaMin vars.alphaMin
        SimFn.simulationSetVariable $ AlphaDecay vars.alphaDecay
        SimFn.simulationSetVariable $ VelocityDecay vars.velocityDecay
      Nothing -> pure unit

    -- 6. Handle links with filtering
    linksInSim <- case config.links, config.linkFilter of
      -- New links provided WITH filter: filter first, then swizzle
      Just newLinks, Just filterFn -> do
        let filteredLinks = Array.filter filterFn newLinks
        SimFn.simulationSetLinks filteredLinks nodesInSim config.keyFn

      -- New links provided WITHOUT filter: just swizzle
      Just newLinks, Nothing ->
        SimFn.simulationSetLinks newLinks nodesInSim config.keyFn

      -- No new links, but filter provided: can't filter already-swizzled links
      -- This case is invalid - log warning and return current links
      Nothing, Just _ -> do
        handle <- use _handle
        let swizzledLinks = getLinksFromSimulation_ handle
        liftEffect $ log "Warning: linkFilter ignored - can't filter already-swizzled links. Provide 'links' to apply filter."
        pure swizzledLinks

      -- No new links, no filter: return current links
      Nothing, Nothing -> do
        handle <- use _handle
        let swizzledLinks = getLinksFromSimulation_ handle
        pure swizzledLinks

    -- 7. Return updated data for re-joining to DOM
    pure { nodes: nodesInSim, links: linksInSim }

  reheat alpha = SimFn.simulationSetVariable $ Alpha alpha

-- | Helper function for reselecting from D3v2 renderTree results
-- |
-- | This wraps the `reselect` function from Operations to work with D3v2Selection_ newtype.
reselectD3v2
  :: forall datum datumOut
   . String
  -> Map.Map String (D3v2Selection_ SBoundOwns Element datum)
  -> Effect (D3v2Selection_ SEmpty Element datumOut)
reselectD3v2 name selectionsMap = do
  -- Unwrap D3v2Selection_ to Selection
  let unwrappedMap = map (\(D3v2Selection_ sel) -> sel) selectionsMap
  -- Call reselect
  result <- Ops.reselect name unwrappedMap
  -- Wrap result back in D3v2Selection_
  pure $ D3v2Selection_ result

-- | Query using CSS selector across all named selections
-- |
-- | Like `queryAll` but works with D3v2Selection_ wrapper.
-- | Properly unwraps the newtype before querying.
queryAllD3v2
  :: forall datum datumOut
   . String  -- ^ CSS selector
  -> Map.Map String (D3v2Selection_ SBoundOwns Element datum)
  -> Effect (D3v2Selection_ SEmpty Element datumOut)
queryAllD3v2 selector selectionsMap = do
  -- Unwrap D3v2Selection_ to Selection - CRITICAL!
  let unwrappedMap = map (\(D3v2Selection_ sel) -> sel) selectionsMap
  -- Call queryAll from Query module
  result <- Query.queryAll selector unwrappedMap
  -- Wrap result back in D3v2Selection_
  pure $ D3v2Selection_ result
