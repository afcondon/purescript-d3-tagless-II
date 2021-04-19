module D3.Interpreter.Tagless where

import D3.Selection

import Control.Monad.Identity.Trans (IdentityT)
import Control.Monad.State (class MonadState, StateT, State, runStateT)
import D3.Attributes.Instances (Attribute(..), unbox)
import D3.Layouts.Simulation (defaultSimulationDrag_, onTick_)
import Data.Foldable (foldl)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, Unit, pure, show, unit, ($), (<>), (<$>))

-- not actually using Effect in foreign fns to keep sigs simple (for now)
-- also not really making a ton of use of StateT, but it is good to have a 
-- place to stash D3's global state such as named transitions etc
newtype D3M :: forall k. k -> Type -> Type
newtype D3M selection a = D3M (StateT Unit Effect a) 
-- TODO don't really need a State instance now, could be ReaderT, however, state might make a comeback so leaving for now

derive newtype instance functorD3M     :: Functor           (D3M selection)
derive newtype instance applyD3M       :: Apply             (D3M selection)
derive newtype instance applicativeD3M :: Applicative       (D3M selection)
derive newtype instance bindD3M        :: Bind              (D3M selection)
derive newtype instance monadD3M       :: Monad             (D3M selection)
derive newtype instance monadStateD3M  :: MonadState  Unit  (D3M selection) 
derive newtype instance monadEffD3M    :: MonadEffect       (D3M selection)

-- TODO see whether it can be useful to extend the interpreter here, for different visualization types
-- in particular, it could be good to have Simulation do it's join function by putting nodes / links
-- into both DOM and Simulation for example (and current implementation is gross and wrong)
class (Monad m) <= D3Tagless selection m where
  attach :: Selector                  -> m selection
  append :: selection      -> D3_Node -> m selection
  join   :: ∀ a. selection -> Join a  -> m selection

  attachZoom :: selection -> ZoomConfig -> m selection

infix 4 join as <+>

runD3M :: forall a. D3M D3Selection_ a -> Effect (Tuple a Unit)
runD3M (D3M state) = runStateT state unit

instance d3TaglessD3M :: D3Tagless D3Selection_ (D3M D3Selection_) where
  attach selector = pure $ d3SelectAllInDOM_ selector 

  append selection_ (D3_Node element attributes) = do
    let appended_ = d3Append_ (show element) selection_
    pure $ foldl applyChainable appended_ attributes    

  join selection (Join j) = do
    let 
      selectS = d3SelectionSelectAll_ (show j.element) selection
      dataS   = case j.key of
                  UseDatumAsKey    -> d3Data_        j.data    selectS 
                  (ComputeKey fn)  -> d3KeyFunction_ j.data fn selectS 
      enterS  = d3EnterAndAppend_ (show j.element) dataS
      enterS' = foldl applyChainable enterS j.behaviour
    pure enterS'

  join selection (JoinSimulation j) = do
    let makeTick :: Array Chainable -> D3Selection_ -> Unit -> Unit
        makeTick attributes selection_ _ = do
          let _ = (applyChainable selection_) <$> attributes
          unit

    let 
      initialS = d3SelectionSelectAll_ (show j.element) selection
      dataS    = case j.key of
                    UseDatumAsKey    -> d3Data_        j.data    initialS 
                    (ComputeKey fn)  -> d3KeyFunction_ j.data fn initialS 
      enterS   = d3EnterAndAppend_ (show j.element) dataS
      _        = foldl applyChainable enterS  j.behaviour
      _        = onTick_ j.simulation j.tickName (makeTick j.onTick enterS)
      _        = case j.onDrag of
                    DefaultDrag -> defaultSimulationDrag_ enterS j.simulation
                    _ -> unit
    pure dataS

  join selection (JoinGeneral j) = do
    let
      selectS = d3SelectionSelectAll_ (show j.element) selection
      dataS  = case j.key of
                UseDatumAsKey    -> d3Data_        j.data    selectS 
                (ComputeKey fn)  -> d3KeyFunction_ j.data fn selectS 
      enterS = d3EnterAndAppend_ (show j.element) dataS
      exitS  = d3Exit_ dataS
      _        = foldl applyChainable enterS  j.behaviour.enter
      _        = foldl applyChainable exitS   j.behaviour.exit
      _        = foldl applyChainable dataS   j.behaviour.update
    pure dataS

  attachZoom selection config = do
    let 
      (ScaleExtent smallest largest) = config.scale
    
    -- sticking to the rules of no ADT's on the JS side we case on the ZoomExtent here
    pure $ 
      case config.extent of
        DefaultZoomExtent -> 
          d3AttachZoomDefaultExtent_ selection {
            scaleExtent: [ smallest, largest ]
          , qualifier  : config.qualifier
          } 

        (ZoomExtent ze)   -> do
          d3AttachZoom_ selection { 
            extent     : [ [ ze.left, ze.top ], [ ze.right, ze.bottom ] ]
          , scaleExtent: [ smallest, largest ]
          , qualifier  : config.qualifier
          }
        -- TODO write casae for: (ExtentFunction f) -> selection


applyChainable :: D3Selection_ -> Chainable -> D3Selection_
applyChainable selection_ (AttrT (Attribute label attr)) = -- spy "d3SetAttr" $ 
  d3SetAttr_ label (unbox attr) selection_
-- NB only protection against non-text attribute for Text field is in the helper function
applyChainable selection_ (TextT (Attribute label attr)) = d3SetText_ (unbox attr) selection_ 
-- NB this remove call will have no effect on elements with active or pending transitions
-- and this gives rise to very counter-intuitive misbehaviour as subsequent enters clash with 
-- elements that should have been removed
applyChainable selection_ RemoveT = d3RemoveSelection_ selection_ -- "selection" here will often be a "transition"
-- for transition in D3 we must use .call(selection, transition) so that chain continues
-- in this interpreter it's enought to just return the selection instead of the transition
applyChainable selection_ (TransitionT chain transition) = do
  let tHandler = d3AddTransition selection_ transition
      _        = foldl applyChainable tHandler chain
  selection_ -- NB we return selection, not transition

newtype D3PrinterM a = D3PrinterM (State String a)

derive newtype instance functorD3PrinterM     :: Functor           D3PrinterM
derive newtype instance applyD3PrinterM       :: Apply             D3PrinterM
derive newtype instance applicativeD3PrinterM :: Applicative       D3PrinterM
derive newtype instance bindD3PrinterM        :: Bind              D3PrinterM
derive newtype instance monadD3PrinterM       :: Monad             D3PrinterM
derive newtype instance monadStateD3PrinterM  :: MonadState String D3PrinterM 

instance d3Tagless :: D3Tagless String D3PrinterM where
  attach selector       = pure $ "attaching to " <> selector <> " in DOM"
  append selection node = pure $ "appending " <> show node <> " to " <> selection
  join selection (Join j)           = pure $ "entering a " <> show j.element <> " for each datum" 
  join selection (JoinGeneral j)    = pure $ "entering a " <> show j.element <> " for each datum" 
  join selection (JoinSimulation j) = pure $ "entering a " <> show j.element <> " for each datum" 
  attachZoom selection zoomConfig = pure $ "attaching a zoom handler to " <> selection
