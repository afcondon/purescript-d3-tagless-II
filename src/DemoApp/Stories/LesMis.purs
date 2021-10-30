module Stories.LesMis where

import Prelude
import Data.Lens

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState)
import D3.Attributes.Instances (Label)
import D3.Examples.LesMiserables as LesMis
import D3.Examples.LesMiserables.File (readGraphFromFileContents)
import D3.FFI (linksForceName)
import D3.Simulation.Config as F
import D3.Simulation.Forces (createForce, createLinkForce, initialize)
import D3.Simulation.Types (D3SimulationState_, Force, ForceStatus(..), ForceType(..), RegularForceType(..), SimVariable(..), allNodes, getStatusMap, initialSimulationState, showMaybeForceStatus, toggleForceStatus)
import D3Tagless.Block.Button as Button
import D3Tagless.Block.Expandable as Expandable
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Capabilities (actualizeForces, setConfigVariable, start)
import D3Tagless.Instance.Simulation (runWithD3_Simulation)
import Data.Array (singleton)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.FormField as FormField
import Snippets (readSnippetFiles)
import Stories.Utilities (syntaxHighlightedCode)
import Stories.Utilities as Utils
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
  , snippets :: { draw :: String, accessors :: String, handler :: String }
  , forceStatuses   :: Map Label ForceStatus
}

_panels = prop (Proxy :: Proxy "panels")
_snippets = prop (Proxy :: Proxy "snippets")

_drawCode :: Lens' State String
_drawCode = _snippets <<< prop (Proxy :: Proxy "draw")
_handlerCode :: Lens' State String
_handlerCode = _snippets <<< prop (Proxy :: Proxy "handler")
_accessorsCode :: Lens' State String
_accessorsCode = _snippets <<< prop (Proxy :: Proxy "accessors")
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
  , links: linksForceName
}

_forceStatuses :: Lens' State (Map Label ForceStatus)
_forceStatuses = prop (Proxy :: Proxy "forceStatuses")
_forceStatus :: forall p.
  Strong p => Choice p => String ->
  p ForceStatus ForceStatus ->
  p State State
_forceStatus label = _forceStatuses <<< at label <<< _Just

_linksSetting :: forall p. Strong p => Choice p => p ForceStatus ForceStatus -> p State State
_linksSetting = _forceStatus forceNames.links
_manyBodyNegSetting :: forall p. Strong p => Choice p => p ForceStatus ForceStatus -> p State State
_manyBodyNegSetting = _forceStatus forceNames.manyBodyNeg
_manyBodyPosSetting :: forall p. Strong p => Choice p => p ForceStatus ForceStatus -> p State State
_manyBodyPosSetting = _forceStatus forceNames.manyBodyPos
_collisionSetting :: forall p. Strong p => Choice p => p ForceStatus ForceStatus -> p State State
_collisionSetting = _forceStatus forceNames.collision

forces :: { center :: Force, collision :: Force, links :: Force, manyBodyPos :: Force , manyBodyNeg :: Force }
forces = { 
    manyBodyNeg: createForce forceNames.manyBodyNeg  (RegularForce ForceManyBody) allNodes [ F.strength (-40.0) ]
  , manyBodyPos: createForce forceNames.manyBodyPos  (RegularForce ForceManyBody) allNodes [ F.strength 30.0 ]
  , collision:   createForce forceNames.collision    (RegularForce ForceCollide)  allNodes [ F.radius 4.0 ]
  , center:      createForce forceNames.center       (RegularForce ForceCenter)   allNodes [ F.x 0.0, F.y 0.0, F.strength 1.0 ]
  , links:       createLinkForce Nothing [] -- link force is special, there can be only one of them
}

toggleForceByName :: forall m. MonadState State m => String -> m Unit
toggleForceByName name
  | name == forceNames.manyBodyNeg  = _manyBodyNegSetting %= toggleForceStatus
  | name == forceNames.manyBodyPos  = _manyBodyPosSetting %= toggleForceStatus
  | name == forceNames.collision    = _collisionSetting   %= toggleForceStatus
  | name == forceNames.links        = _linksSetting       %= toggleForceStatus
  | otherwise = pure unit

 
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
      , panels: { code: Expandable.Collapsed }
      , snippets: { draw: "", accessors: "", handler: "" }
      , forceStatuses: getStatusMap forceLibrary
    }

  controls state = 
    [ HH.div
      [ Utils.tailwindClass "story-panel-controls"] 
      [ Button.buttonGroup [ HP.class_ $ HH.ClassName "flex-col" ]
        [ Button.buttonVertical
          [ HE.onClick (const $ ToggleForce forceNames.links) ]
          [ HH.text $ "links: " <> showMaybeForceStatus (preview _linksSetting state) ]
        , Button.buttonVertical
          [ HE.onClick (const $ ToggleForce forceNames.manyBodyPos) ]
          [ HH.text $ "many body +: " <> showMaybeForceStatus (preview _manyBodyPosSetting state) ]
        , Button.buttonVertical
          [ HE.onClick (const $ ToggleForce forceNames.manyBodyNeg) ]
          [ HH.text $ "many body: -" <> showMaybeForceStatus (preview _manyBodyNegSetting state) ]
        , Button.buttonVertical
          [ HE.onClick (const $ ToggleForce forceNames.collision) ]
          [ HH.text $ "collision: " <> showMaybeForceStatus (preview _collisionSetting state) ]
        , Button.buttonVertical
          [ HE.onClick (const $ Freeze) ]
          [ HH.text "Freeze" ]
        , Button.buttonVertical
          [ HE.onClick (const $ Reheat) ]
          [ HH.text "Reheat!" ]
        ]
      ]
    ]

  render state =
    HH.div [ Utils.tailwindClass "story-container" ]
      [ HH.div -- [ Utils.tailwindClass "story-panel"]
        [ Utils.tailwindClass "story-panel-controls"] 
        (controls state)
      , HH.div -- [ Utils.tailwindClass "story-panel" ] 
            [ Utils.tailwindClass "story-panel-code"]
            [ FormField.field_
                { label: HH.text "Code"
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
            , Expandable.content_ (view _code state) blurbtext
            , Expandable.content_ (view _code state) $ 
                syntaxHighlightedCode (view _drawCode state) <>
                syntaxHighlightedCode (view _accessorsCode state) <>
                syntaxHighlightedCode (view _handlerCode state) 
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
    response <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    let graph = readGraphFromFileContents response

    text <- H.liftAff $ readSnippetFiles "LesMisScript"
    _drawCode .= text
    text <- H.liftAff $ readSnippetFiles "LesMisHandleActions"
    _handlerCode .= text
    text <- H.liftAff $ readSnippetFiles "LesMisAccessors"
    _accessorsCode .= text

    _forceStatus forceNames.center       %= (const ForceActive)
    _forceStatus forceNames.manyBodyNeg  %= (const ForceActive)
    _forceStatus forceNames.collision    %= (const ForceActive)
    _forceStatus forceNames.links        %= (const ForceActive)

    _forceStatus forceNames.manyBodyPos  %= (const ForceDisabled)
    
    runWithD3_Simulation do
      statuses <- use _forceStatuses
      actualizeForces statuses
      LesMis.draw graph "div.svg-container"

  Finalize ->  pure unit

  ToggleForce name -> do
    toggleForceByName name
    runWithD3_Simulation do
      statuses <- use _forceStatuses
      actualizeForces statuses
      setConfigVariable $ Alpha 0.7
      start

  Freeze  -> runWithD3_Simulation $ setConfigVariable $ Alpha 0.0
  Reheat  -> do
    runWithD3_Simulation do
      setConfigVariable $ Alpha 0.7
      start
-- Snippet_End

blurbtext = (HH.p [ HP.classes [ HH.ClassName "m-2", HH.ClassName "w-2/3" ] ]) <$> ((singleton <<< HH.text) <$> texts)
  where texts = ["""
    
This example introduces a new capability, signalled by the SimulationM
constraint on the function. This monad runs with a D3 Simulation engine in its
State. This allows us to let the simulation engine do the layout, we provide
the nodes and (optionally) links and configure the simulation with additional
forces. """

, """From the D3 docs: "This module implements a velocity Verlet numerical
integrator for simulating physical forces on particles. The simulation is
simplified: it assumes a constant unit time step Δt = 1 for each step, and a
constant unit mass m = 1 for all particles. As a result, a force F acting on a
particle is equivalent to a constant acceleration a over the time interval Δt,
and can be simulated simply by adding to the particle’s velocity, which is then
added to the particle’s position.""
    
  """]

