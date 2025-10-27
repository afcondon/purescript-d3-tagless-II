module PSD3.Unlinked.LesMis where -- unlinked

import Prelude

import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get, put)
import PSD3.Internal.Attributes.Instances (Label)
import D3.Viz.LesMiserables as LesMis
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import PSD3.Internal.FFI (linksForceName_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), SimVariable(..), allNodes, initialSimulationState)
import PSD3.Button as Button
import PSD3.Expandable as Expandable
import PSD3.Toggle as Toggle
import PSD3.Capabilities.Simulation (actualizeForces, init, setConfigVariable, start)
import PSD3.Interpreter.D3 (runWithD3_Simulation)
import Data.Lens (Lens', use, view, (%=), (.=))
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.FormField as FormField
import Snippets (Cell(..), Notebook, renderNotebook, substituteSnippetCells)
import PSD3.Shared.Utilities as Utils
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | Finalize
  | ToggleCard (Lens' State Expandable.Status)
  | ToggleForce Label
  | Freeze
  | Reheat

type State = {
    simulation      :: D3SimulationState_
  , panels  :: { code :: Expandable.Status }
  , notebook :: forall w. Notebook (Set Label) w Action
  , activeForces    :: Set Label
}

_panels = prop (Proxy :: Proxy "panels")
_notebook = prop (Proxy :: Proxy "notebook")

_code :: Lens' State Expandable.Status
_code = _panels <<< prop (Proxy :: Proxy "code")

-- | ================================================================================================
-- | Everything Forces: force names to keep typos at bay, lenses to access the forces in the State
-- | ================================================================================================
forceLibrary :: Map Label Force
forceLibrary = initialize [ forces.manyBodyNeg, forces.manyBodyPos, forces.collision, forces.center, forces.links ]

forceNames :: { center :: String, collision :: String, links :: String, manyBodyPos :: String, manyBodyNeg :: String  }
forceNames = {
    manyBodyNeg: "many body negative"
  , manyBodyPos: "many body positive"
  , collision: "collision"
  , center: "center"
  , links: linksForceName_
}

-- Note: Cannot use lens due to forall in notebook field, use direct field access instead

forces :: { center :: Force, collision :: Force, links :: Force, manyBodyPos :: Force , manyBodyNeg :: Force }
forces = { 
    manyBodyNeg: createForce forceNames.manyBodyNeg  (RegularForce ForceManyBody) allNodes [ F.strength (-40.0) ]
  , manyBodyPos: createForce forceNames.manyBodyPos  (RegularForce ForceManyBody) allNodes [ F.strength 30.0 ]
  , collision:   createForce forceNames.collision    (RegularForce ForceCollide)  allNodes [ F.radius 4.0 ]
  , center:      createForce forceNames.center       (RegularForce ForceCenter)   allNodes [ F.x 0.0, F.y 0.0, F.strength 1.0 ]
  , links:       createLinkForce Nothing [] -- link force is special, there can be only one of them
}

toggleForceByName :: forall m. MonadState State m => String -> m Unit
toggleForceByName name = do
  state <- get
  let updated = state { activeForces =
        if Set.member name state.activeForces
          then Set.delete name state.activeForces
          else Set.insert name state.activeForces
      }
  put updated

 
-- | ================================================================================================
-- | Halogen component 
-- | ================================================================================================
component :: forall query output m. 
  MonadAff m => 
  H.Component query Unit output m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize   = Just Finalize }
  }
  where
  initialState :: State
  initialState = {
        simulation: initialSimulationState forceLibrary
      , panels: { code: Expandable.Expanded }
      , notebook: lesMisNotebook
      , activeForces: Set.fromFoldable [forceNames.center, forceNames.manyBodyNeg, forceNames.collision, forceNames.links]
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Utils.tailwindClass "story-container" ]
      [ HH.div -- [ Utils.tailwindClass "story-panel" ] 
        [ Utils.tailwindClass "story-panel-code"]
        [ FormField.field_
            { label: HH.text "(hide this panel if screen too small)"
            , helpText: []
            , error: []
            , inputId: "show-code"
            }
          [ Toggle.toggle
            [ HP.id "show-code"
            , HP.checked
              $ Expandable.toBoolean (view _code state)
            , HE.onChange \_ -> ToggleCard _code
            ]
          ]
        -- , Expandable.content_ (view _code state) [ controls state ]
        , Expandable.content_ (view _code state) (renderNotebook state.activeForces state.notebook)
        ]  
      , HH.div [ Utils.tailwindClass "svg-container" ] []
      ]
      
-- Snippet_Start
-- Name: LesMisHandleActions
handleAction :: forall m. 
  Bind m => 
  MonadAff m => 
  MonadState State m =>
  Action -> m Unit
handleAction = case _ of

  ToggleCard _cardState -> _cardState %= not

  Initialize -> do
    notebook' <- traverse substituteSnippetCells lesMisNotebook
    _notebook .= notebook'

    response <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    let graph = readGraphFromFileContents response

    state <- get
    let forcesArray = [ forces.manyBodyNeg, forces.manyBodyPos, forces.collision, forces.center, forces.links ]
    runWithD3_Simulation do
      LesMis.drawSimplified forcesArray state.activeForces graph "div.svg-container"

  Finalize ->  pure unit

  ToggleForce name -> do
    toggleForceByName name
    state <- get
    runWithD3_Simulation do
      actualizeForces state.activeForces
      setConfigVariable $ Alpha 0.7
      start

  Freeze  -> runWithD3_Simulation $ setConfigVariable $ Alpha 0.0
  Reheat  -> do
    runWithD3_Simulation do
      setConfigVariable $ Alpha 0.7
      start
-- Snippet_End

controls :: forall p125. Set Label -> HH.HTML p125 Action
controls activeForces =
    Button.buttonGroup [ HP.class_ $ HH.ClassName "flex-col" ]
      [ Button.buttonVertical
        [ HE.onClick (const $ ToggleForce forceNames.links) ]
        [ HH.text $ "links: " <> if Set.member forceNames.links activeForces then "On" else "Off" ]
      , Button.buttonVertical
        [ HE.onClick (const $ ToggleForce forceNames.manyBodyPos) ]
        [ HH.text $ "many body +: " <> if Set.member forceNames.manyBodyPos activeForces then "On" else "Off" ]
      , Button.buttonVertical
        [ HE.onClick (const $ ToggleForce forceNames.manyBodyNeg) ]
        [ HH.text $ "many body: -" <> if Set.member forceNames.manyBodyNeg activeForces then "On" else "Off" ]
      , Button.buttonVertical
        [ HE.onClick (const $ ToggleForce forceNames.collision) ]
        [ HH.text $ "collision: " <> if Set.member forceNames.collision activeForces then "On" else "Off" ]
      , Button.buttonVertical
        [ HE.onClick (const $ Freeze) ]
        [ HH.text "Freeze" ]
      , Button.buttonVertical
        [ HE.onClick (const $ Reheat) ]
        [ HH.text "Reheat!" ]
      ]

lesMisNotebook :: forall w. Notebook (Set Label) w Action
lesMisNotebook = [
    Blurb 

    """This example introduces a new capability, signalled by the SimulationM2
    constraint on the function. This monad runs with a D3 Simulation engine in its
    State. This allows us to let the simulation engine do the layout, we provide
    the nodes and (optionally) links and configure the simulation with additional
    forces. """
  
  , RenderWithState controls

  , Blurb

  """ From the D3 docs: "This module implements a velocity Verlet numerical
  integrator for simulating physical forces on particles. The simulation is
  simplified: it assumes a constant unit time step Δt = 1 for each step, and a
  constant unit mass m = 1 for all particles. As a result, a force F acting on a
  particle is equivalent to a constant acceleration a over the time interval Δt,
  and can be simulated simply by adding to the particle’s velocity, which is then
  added to the particle’s position.""

  """

  , SnippetFile "LesMisScript"
  , SnippetFile "LesMisHandleActions"
  , SnippetFile "LesMisAccessors"
]

