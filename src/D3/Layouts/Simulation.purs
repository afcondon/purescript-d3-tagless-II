module D3.Layouts.Simulation where

import D3.Selection (Chainable, D3Attr, D3Selection_)
import Data.Map (Map)
import Prelude (Unit)

-- import D3.Base.Attributes (Attr)
-- import D3.Base.Selection (Label)

-- | a record to initialize / configure simulations
type SimulationConfig_ = { 
      alpha         :: Number
    , alphaTarget   :: Number
    , alphaMin      :: Number
    , alphaDecay    :: Number
    , velocityDecay :: Number
}

defaultConfigSimulation :: SimulationConfig_
defaultConfigSimulation = { 
      alpha        : 1.0
    , alphaTarget  : 0.0
    , alphaMin     : 0.0001
    , alphaDecay   : 0.0228
    , velocityDecay: 0.4
}

-- this is the row that gets added ot your Model's nodes when initialized by D3
type D3SimulationNode r = { x :: Number
                          , y :: Number
                          , group :: Number
                          , vx :: Number
                          , vy :: Number
                          , index :: Number
                          | r } -- extra node information from model

-- | express the additions that D3 makes in terms of rows for clarity and DRY
-- after the GraphLink type has been bound in D3 it is changed to the following
type D3SimulationLink r l = { id :: ID
                          , source :: D3SimulationNode r
                          , target :: D3SimulationNode r
                          , value :: Number
                          | l } -- extra link information from model

-- | Force Layout core types
type ID = Int -- TODO this needs to be polymorphic eventually
type Link = forall r. { id :: ID, source :: ID, target :: ID | r }
type Node = forall r i. { id :: i | r }
type IdFn = Link -> ID
newtype ForceName = ForceName String
data Force = Force ForceName ForceType
data ForceType =
    ForceMany
  | ForceCenter Number Number
  -- | ForceLink (Array Link) IdFn
  | ForceCollide Number
  | ForceX Number
  | ForceY Number
  | ForceRadial Number Number
  | Custom

type SimulationRecord_ = { 
    label  :: String
  , config :: SimulationConfig_
  , nodes  :: Array Node
  , links  :: Array Link
  , forces :: Array Force
  , tick   :: Unit -> Unit -- could be Effect Unit
  , drag   :: Simulation -> Unit -- could be Effect Unit
}

data Simulation = Simulation SimulationRecord_
type TickMap :: forall k. k -> Type
type TickMap model = Map String (Array Chainable)
data DragBehavior = DefaultDrag String String -- only one implementation rn and implemented on _ side 

{-
-- |                   FORCE LAYOUT (SIMULATION) interpreter
startSimulation :: forall model. NativeSelection -> D3 model Unit
startSimulation simulation = pure $ startSimulation_ simulation

stopSimulation :: forall model. NativeSelection -> D3 model Unit
stopSimulation simulation = pure $ stopSimulation_ simulation

-- | get a reference to a simulation that we can then use elsewhere
-- | ie having interpreted a Selection such that the DOM is set up to run a simulation
-- | NB this is actually modifying the model in the Context
interpretSimulation :: forall model node link. Simulation -> 
                              (model -> Array node) ->  -- project nodes from model
                              (model -> Array link) ->  -- project links from model
                              (Array link -> Array node -> model) -> -- repackage nodes & links as model
                              D3 model NativeSelection
interpretSimulation (Simulation r) getNodes getLinks repackage =
  do
    (Context model scope) <- get
    let sim = initSimulation_ r.config
        nodes = nativeNodes $ getNodes model
        links = nativeLinks $ getLinks model
    -- updateScope sim (Just r.label)
    traverse_ (interpretForce sim) r.forces
    let initializedNodes = unnativeNodes $ putNodesInSimulation_ sim nodes
        initializedLinks = unnativeLinks $ putLinksInSimulation_ sim links
        initializedModel = repackage initializedLinks initializedNodes
    put (Context initializedModel (insert r.label sim scope))
    pure sim
  where
    nativeNodes   = unsafeCoerce :: Array node -> Array Native_
    nativeLinks   = unsafeCoerce :: Array link -> Array Native_
    unnativeNodes = unsafeCoerce :: Array Native_ -> Array node
    unnativeLinks = unsafeCoerce :: Array Native_ -> Array link

interpretForce :: forall model. NativeSelection -> Force -> D3 model Unit
interpretForce simulation = do
  case _ of
    (Force label ForceMany)                 -> pure $ forceMany_ simulation label 
    (Force label (ForceCenter cx cy))       -> pure $ forceCenter_ simulation label cx cy
    -- (Force (ForceLink links idFn)) -> pure $ forceLinks
    (Force label (ForceCollide radius_))    -> pure $ forceCollide_ simulation label radius_
    (Force label (ForceX x))                -> pure $ forceX_ simulation label x
    (Force label (ForceY y))                -> pure $ forceY_ simulation label y
    (Force label (ForceRadial cx cy))       -> pure $ forceRadial_ simulation label cx cy
    (Force label Custom)                    -> pure $ unit -- do this later as needed

-- getNativeSelections :: Map String NativeSelection -> TickMap model -> Array (Tuple NativeSelection (Array (Tuple attr fn)))
-- getNativeSelections scope []


getNativeSelection :: forall x. (Map String NativeSelection) -> Map String x -> Array (Tuple NativeSelection x)
getNativeSelection scopeMap tickMap = fromFoldable nativeTuples
  where
    tickTuples :: Array (Tuple String x)
    tickTuples = toUnfoldable tickMap

    maybeNativeTuple :: Tuple String x -> Tuple (Maybe NativeSelection) x
    maybeNativeTuple = lmap (flip lookup scopeMap)

    maybeNativeTuples :: Array (Tuple (Maybe NativeSelection) x)
    maybeNativeTuples = maybeNativeTuple <$> tickTuples

    nativeTuples :: Array (Tuple NativeSelection x)
    nativeTuples = foldl foldFn [] maybeNativeTuples 

    foldFn list (Tuple (Just a) b) = (Tuple a b) : list
    foldFn list (Tuple Nothing _)  = list

addAttrFnToTick :: forall model. Tuple NativeSelection Attr -> D3 model Unit
addAttrFnToTick (Tuple selection attr) = pure $ addAttrFnToTick_ selection attr 

interpretTickMap :: forall model. NativeSelection -> TickMap model -> D3 model Unit
interpretTickMap simulation tickMap = do
  (Context model scope) <- get
  --  [(label,attrs)] -> [(nativeselection, attr)] so as enable build up of list on JS side
  let 
    attrs :: Array (Tuple NativeSelection Attr)
    attrs = concatMap (\(Tuple x ys) -> (Tuple x) <$> ys) (getNativeSelection scope tickMap)
  -- TODO pending better solution will pass Attr (purescript type) over FFI and decode there
  traverse_ addAttrFnToTick attrs
  pure $ attachTickFnToSimulation_ simulation

interpretDrag :: forall model. DragBehavior -> D3 model Unit 
interpretDrag (DefaultDrag selectionName simulationName) = do
  (Context model scope) <- get
  let selection  = lookup selectionName scope
  let simulation = lookup simulationName scope
  pure $ case selection, simulation of
          (Just sel), (Just sim) -> attachDefaultDragBehavior_ sel sim
          _, _ -> unit
-}

-- | foreign types associated with Force Layout Simulation
-- TODO structures here carried over from previous interpreter - review and refactor
foreign import data D3Simulation_ :: Type

foreign import initSimulation_            :: SimulationConfig_ -> D3Simulation_
foreign import startSimulation_           :: D3Simulation_ -> Unit
foreign import stopSimulation_            :: D3Simulation_ -> Unit
foreign import putNodesInSimulation_      :: D3Simulation_ -> Array Node -> D3Simulation_
foreign import putLinksInSimulation_      :: D3Simulation_ -> Array Link -> D3Simulation_

-- TODO this all has to change completely to work within Tagless 
foreign import data NativeSelection :: Type -- just temporarily defined to allow foreign functions to pass
foreign import addAttrFnToTick_           :: D3Selection_ -> D3Attr -> Unit
foreign import attachTickFnToSimulation_  :: D3Selection_ -> Unit
foreign import attachDefaultDragBehavior_ :: D3Selection_ -> D3Selection_ -> Unit
foreign import setAlphaTarget_            :: D3Selection_ -> Number -> Unit

-- 
foreign import forceMany_                 :: D3Simulation_ -> String -> Unit
foreign import forceCenter_               :: D3Simulation_ -> String -> Number -> Number -> Unit
foreign import forceCollide_              :: D3Simulation_ -> String -> Number -> Unit
foreign import forceX_                    :: D3Simulation_ -> String -> Number -> Unit
foreign import forceY_                    :: D3Simulation_ -> String -> Number -> Unit
foreign import forceRadial_               :: D3Simulation_ -> String -> Number -> Number -> Unit
