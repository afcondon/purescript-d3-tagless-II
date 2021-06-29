module D3.Interpreter.D3 where

import Prelude hiding (append)

import Control.Monad.State (class MonadState, StateT, runStateT)
import D3.Attributes.Instances (Attribute(..), unbox)
import D3.Attributes.Sugar (classed, viewBox)
import D3.Data.Types (D3Selection_, Element(..))
import D3.FFI (d3AddTransition_, d3Append_, d3AttachZoomDefaultExtent_, d3AttachZoom_, d3Data_, d3EnterAndAppend_, d3Exit_, d3FilterSelection_, d3KeyFunction_, d3LowerSelection_, d3OrderSelection_, d3RaiseSelection_, d3RemoveSelection_, d3SelectAllInDOM_, d3SelectFirstInDOM_, d3SelectionIsEmpty_, d3SelectionSelectAll_, d3SelectionSelect_, d3SetAttr_, d3SetHTML_, d3SetProperty_, d3SetText_, d3SortSelection_, defaultDrag_, defaultSimulationDrag_, disableDrag_, onTick_, selectionOn_)
import D3.Interpreter (class D3InterpreterM, append, attach, modify)
import D3.Layouts.Simulation (SimulationM, SimulationR)
import D3.Selection (Behavior(..), ChainableS(..), D3_Node(..), DragBehavior(..), Join(..), Keys(..), OrderingAttribute(..), node)
import D3.Simulation.Config (ChainableF(..), D3ForceHandle_)
import D3.Zoom (ScaleExtent(..), ZoomExtent(..))
import Data.Foldable (foldl)
import Data.Tuple (Tuple, fst)
import Effect (Effect)
import Effect.Class (class MonadEffect)

-- not actually using Effect in foreign fns to keep sigs simple (for now)

-- newtype D3M :: forall k. k -> Type -> Type
newtype D3M state selection a = D3M (StateT state Effect a) 

derive newtype instance functorD3M     :: Functor           (D3M state selection)
derive newtype instance applyD3M       :: Apply             (D3M state selection)
derive newtype instance applicativeD3M :: Applicative       (D3M state selection)
derive newtype instance bindD3M        :: Bind              (D3M state selection)
derive newtype instance monadD3M       :: Monad             (D3M state selection)
derive newtype instance monadStateD3M  :: MonadState  Unit  (D3M Unit selection) 
derive newtype instance monadStateD3MSimulation  :: MonadState SimulationR (D3M SimulationR selection) 
derive newtype instance monadEffD3M    :: MonadEffect       (D3M state selection)

runD3M :: forall a. D3M Unit D3Selection_ a -> Effect (Tuple a Unit)
runD3M (D3M state) = runStateT state unit

d3Run :: forall a. D3M Unit D3Selection_ a -> Effect a
d3Run (D3M state) = liftA1 fst $ runStateT state unit

-- runD3M :: forall a st. D3M st D3Selection_ a -> Effect (Tuple a Unit)
-- runD3M (D3M state) = runStateT state unit

-- d3Run :: forall a st. D3M st D3Selection_ a -> Effect a
-- d3Run (D3M state) = liftA1 fst $ runStateT state unit


instance d3TaglessD3M :: D3InterpreterM D3Selection_ (D3M Unit D3Selection_) where
  attach selector = pure $ d3SelectAllInDOM_ selector 

  append selection_ (D3_Node element attributes) = do
    let appended_ = d3Append_ (show element) selection_
    modify appended_ attributes   

  filter selection_ selector = pure $ d3FilterSelection_ selection_ selector

  modify selection_ attributes = pure $ foldl applyChainableSD3 selection_ attributes

  join selection (Join j) = do
    let 
      selectS = d3SelectionSelectAll_ (show j.element) selection
      dataS   = case j.key of
                  UseDatumAsKey    -> d3Data_        j.data    selectS 
                  (ComputeKey fn)  -> d3KeyFunction_ j.data fn selectS 
      enterS  = d3EnterAndAppend_ (show j.element) dataS
      enterS' = foldl applyChainableSD3 enterS j.behaviour
    pure enterS'

  join selection (JoinGeneral j) = do
    let
      selectS = d3SelectionSelectAll_ (show j.element) selection
      dataS  = case j.key of
                UseDatumAsKey    -> d3Data_        j.data    selectS 
                (ComputeKey fn)  -> d3KeyFunction_ j.data fn selectS 
      enterS = d3EnterAndAppend_ (show j.element) dataS
      exitS  = d3Exit_ dataS
      _      = foldl applyChainableSD3 enterS  j.behaviour.enter
      _      = foldl applyChainableSD3 exitS   j.behaviour.exit
      _      = foldl applyChainableSD3 dataS   j.behaviour.update
    pure enterS

  on selection (Tick tick) = do
    let makeTick :: Array ChainableS -> D3Selection_ -> Unit -> Unit
        makeTick attributes selection_ _ = do
          let _ = (applyChainableSD3 selection_) <$> attributes
          unit
        _ = onTick_ tick.simulation tick.name (makeTick tick.chain selection)
    pure selection 
  
  on selection (Drag drag) = do
    case drag of 
      DefaultDrag     -> pure $ defaultDrag_ selection 
      NoDrag          -> pure $ disableDrag_ selection 
      (CustomDrag fn) -> pure $ defaultDrag_ selection -- TODO no custom drag implemented yet

  on selection (Zoom config) = do
    let 
      (ScaleExtent smallest largest) = config.scale
      target = selection
      -- TODO recover the ability to "direct" the zoom to element other than the one receiving the event
      -- ie for controllers, containers etc

    -- sticking to the rules of no ADT's on the JS side we case on the ZoomExtent here
    pure $ 
      case config.extent of
        DefaultZoomExtent -> 
          d3AttachZoomDefaultExtent_ selection {
            scaleExtent: [ smallest, largest ]
          , name  : config.name
          , target
          } 

        (ZoomExtent ze)   -> do
          d3AttachZoom_ selection { 
            extent     : [ [ ze.left, ze.top ], [ ze.right, ze.bottom ] ]
          , scaleExtent: [ smallest, largest ]
          , name  : config.name
          , target
          }


applyChainableSD3 :: D3Selection_ -> ChainableS -> D3Selection_
applyChainableSD3 selection_ (AttrT (ToAttribute label attr)) = 
  d3SetAttr_ label (unbox attr) selection_

-- NB only protection against non-text attribute for Text field is in the helper function
-- and similarly for Property and HTML
applyChainableSD3 selection_ (TextT (ToAttribute label attr))     = d3SetText_     (unbox attr) selection_ 
applyChainableSD3 selection_ (PropertyT (ToAttribute label attr)) = d3SetProperty_ (unbox attr) selection_ 
applyChainableSD3 selection_ (HTMLT (ToAttribute label attr))     = d3SetHTML_     (unbox attr) selection_ 

-- NB this remove call will have no effect on elements with active or pending transitions
-- and this gives rise to very counter-intuitive misbehaviour as subsequent enters clash with 
-- elements that should have been removed
-- also NB "selection" here will often be a "transition" but this distinction won't matter (i think)
-- TODO remove is not like other chainables, in fact it's not chainable since it returns unit
applyChainableSD3 selection_ RemoveT = do
  let _ = d3RemoveSelection_ selection_ 
  selection_

-- for transition in D3 we must use .call(selection, transition) so that chain continues
-- in this interpreter it's enought to just return the selection instead of the transition
applyChainableSD3 selection_ (TransitionT chain transition) = do
  let tHandler = d3AddTransition_ selection_ transition
      _        = foldl applyChainableSD3 tHandler chain
  selection_ -- NB we return selection, not transition

applyChainableSD3 selection_ (OnT event listener) = selectionOn_ selection_ (show event) listener

applyChainableSD3 selection_ (OrderingT oAttr) =
  case oAttr of
    Order          -> d3OrderSelection_ selection_
    (Sort compare) -> d3SortSelection_ selection_ compare
    Raise          -> d3RaiseSelection_ selection_
    Lower          -> d3LowerSelection_ selection_


-- TODO reuse existing SVG if it's the right one
removeExistingSVG :: forall m. D3InterpreterM D3Selection_ m => String -> m D3Selection_
removeExistingSVG rootSelector = do
  let
    root     = d3SelectFirstInDOM_ rootSelector
    -- check for an svg element under the given root
    previous = d3SelectionSelect_ (rootSelector <> " svg") root
  pure $ case d3SelectionIsEmpty_ previous of -- 
          true  -> previous
          false -> d3RemoveSelection_ previous 
