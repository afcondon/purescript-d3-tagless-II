module Stories.LesMis where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, modify_)
import D3.Attributes.Instances (Label)
import D3.Examples.LesMiserables as LesMis
import D3.Examples.LesMiserables.File (readGraphFromFileContents)
import D3.FFI (linksForceName)
import D3.Simulation.Config as F
import D3.Simulation.Forces (createForce, createLinkForce, getStatusMap, initialize)
import D3.Simulation.Functions (_d3Simulation, simulationStart)
import D3.Simulation.Types (D3SimulationState_, Force, ForceStatus(..), ForceType(..), RegularForceType(..), SimVariable(..), _linkdata, _name, _status, allNodes, initialSimulationState, showMaybeForceStatus, toggleForceStatus)
import D3Tagless.Block.Button as Button
import D3Tagless.Block.Expandable as Expandable
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Capabilities (addForces, setConfigVariable, setForceStatuses, setLinks)
import D3Tagless.Instance.Simulation (runWithD3_Simulation)
import Data.Lens (Lens', _Just, over, preview, use, view, (%=))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map, empty, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.FormField as FormField
import Stories.Utilities (syntaxHighlightedCode)
import Stories.Utilities as Utils
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data Action
  = Initialize
  | Finalize
  | ToggleCard (Lens' State Expandable.Status)
  | ToggleManyBody
  | ToggleLinks
  | Freeze
  | Reheat

type State = { 
    simulation      :: D3SimulationState_
  , blurb           :: Expandable.Status
  , code            :: Expandable.Status
  , forceLibrary    :: Map Label Force
}

_blurb :: Lens' State Expandable.Status
_blurb = prop (Proxy :: Proxy "blurb")

_code :: Lens' State Expandable.Status
_code = prop (Proxy :: Proxy "code")

_forceLibrary :: Lens' State (Map Label Force)
_forceLibrary = prop (Proxy :: Proxy "forceLibrary")

_linksSetting = _forceLibrary <<< at linksForceName <<< _Just <<< _status

_manyBodySetting = _forceLibrary <<< at "many body" <<< _Just <<< _status
 
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
        simulation: initialSimulationState 2
      , blurb: Expandable.Collapsed
      , code: Expandable.Collapsed
      , forceLibrary: initialize [ 
          createForce "center"      (RegularForce ForceCenter)   allNodes [ F.x 0.0, F.y 0.0, F.strength 1.0 ]
        , createForce "many body"   (RegularForce ForceManyBody) allNodes []
        , createForce "collision"   (RegularForce ForceCollide)  allNodes [ F.radius 4.0 ]
        , createForce "collision20" (RegularForce ForceCollide)  allNodes [ F.radius 20.0] -- NB initially not enabled
        , createLinkForce Nothing [ ]
        ]
    }

  controls state = 
    [ HH.div
      [ Utils.tailwindClass "story-panel-controls"] 
      [ Button.buttonGroup [ HP.class_ $ HH.ClassName "flex-col" ]
        [ Button.buttonVertical
          [ HE.onClick (const $ ToggleLinks) ] -- { enable: ["links"], disable: [""]}
          [ HH.text $ showMaybeForceStatus (preview _linksSetting state) ]
        , Button.buttonVertical
          [ HE.onClick (const $ ToggleManyBody) ]
          [ HH.text $ showMaybeForceStatus (preview _manyBodySetting state) ]
        , Button.buttonVertical
          [ HE.onClick (const $ Freeze) ]
          [ HH.text "Freeze" ]
        , Button.buttonVertical
          [ HE.onClick (const $ Reheat) ]
          [ HH.text "Reheat!" ]
        ]
      ]
    ]

    
  -- render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Utils.tailwindClass "story-container" ]
      [ HH.div -- [ Utils.tailwindClass "story-panel"]
        [ Utils.tailwindClass "story-panel-controls"] 
        (controls state)
      , HH.div -- [ Utils.tailwindClass "story-panel" ] 
            [ Utils.tailwindClass "story-panel-about"]
            [ FormField.field_
              { label: HH.text "About"
              , helpText: []
              , error: []
              , inputId: "show-blurb"
              }
              [ Toggle.toggle
                [ HP.id "show-blurb"
                , HP.checked
                  $ Expandable.toBoolean state.blurb
                , HE.onChange \_ -> ToggleCard _blurb
                ]
              ]
            , Expandable.content_ state.blurb [ HH.text blurbtext ]
            ]  
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
                  $ Expandable.toBoolean state.code
                , HE.onChange \_ -> ToggleCard _code
                ]
              ]
            , Expandable.content_ state.code $ syntaxHighlightedCode codetext 
            ]  
      , HH.div [ Utils.tailwindClass "svg-container" ] []
      ]

handleAction :: forall m. 
  Bind m => 
  MonadAff m => 
  MonadState State m =>
  Action -> m Unit
handleAction = case _ of

  ToggleCard lens -> do
    cardState <- H.get
    H.put (over lens not cardState)

  Initialize -> do
    response <- H.liftAff $ AJAX.get ResponseFormat.string "http://localhost:1234/miserables.json"
    let graph = readGraphFromFileContents response

    state <- H.get
    runWithD3_Simulation $ addForces state.forceLibrary
    runWithD3_Simulation $ setForceStatuses $ fromFoldable $ (\l -> Tuple l ForceActive) <$> [ "center", "many body", "collision", "links" ]
    runWithD3_Simulation $ LesMis.graphScript graph "div.svg-container"

  Finalize ->  pure unit -- runWithD3_Simulation removeAllForces

  ToggleManyBody -> do
    _manyBodySetting %= toggleForceStatus
    state <- H.get
    let statusMap = getStatusMap state.forceLibrary
    runWithD3_Simulation $ setForceStatuses statusMap
    runWithD3_Simulation $ setConfigVariable $ Alpha 0.7
    simulationStart

  ToggleLinks -> do
    _linksSetting %= toggleForceStatus
    state <- H.get
    let statusMap = getStatusMap state.forceLibrary
    runWithD3_Simulation $ setForceStatuses statusMap
    runWithD3_Simulation $ setConfigVariable $ Alpha 0.7
    simulationStart

  Freeze  -> runWithD3_Simulation $ setConfigVariable $ Alpha 0.0
  Reheat  -> do
    runWithD3_Simulation $ setConfigVariable $ Alpha 0.7
    simulationStart

codetext :: String
codetext = 
  """
-- type-safe(ish) accessors for the data that is given to D3
-- we lose the type information in callbacks from the FFI, such as for attributes
-- but since we know what we gave we can coerce it back to the initial type.
link_ = {
    source: (\d -> (unboxD3SimLink d).source)
  , target: (\d -> (unboxD3SimLink d).target)
  , value:  (\d -> (unboxD3SimLink d).value)
  , color:  (\d -> d3SchemeCategory10N_ (toNumber $ (unboxD3SimLink d).target.group))
}

datum_ = {
-- direct accessors to fields of the datum (BOILERPLATE)
    index : (\d -> (unboxD3SimNode d).index)
  , id    : (\d -> (unboxD3SimNode d).id)
  , x     : (\d -> (unboxD3SimNode d).x)
  , y     : (\d -> (unboxD3SimNode d).y)
  , group : (\d -> (unboxD3SimNode d).group)

  , colorByGroup:
      (\d -> d3SchemeCategory10N_ (toNumber $ datum_.group d))
}

-- | recipe for this force layout graph
graphScript :: forall row m. 
  Bind m => 
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m => 
  SimulationM D3Selection_ m =>
  LesMisRawModel -> Selector D3Selection_ -> m Unit
graphScript model selector = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  (root :: D3Selection_) <- attach selector
  svg        <- root D3.+ (node Svg [ viewBox (-w / 2.0) (-h / 2.0) w h
                                               , classed "lesmis" ] )
  linksGroup <- svg  D3.+ (node Group  [ classed "link", strokeColor "#999", strokeOpacity 0.6 ])
  nodesGroup <- svg  D3.+ (node Group  [ classed "node", strokeColor "#fff", strokeOpacity 1.5 ])
  
  -- in contrast to a simple SelectionM function, we have additional typeclass capabilities for simulation
  -- which we use here to introduce the nodes and links to the simulation
  simulationNodes <- setNodes model.nodes
  simulationLinks <- setLinks model.links datum_.id -- the "links" force will already be there
  
  -- joining the data from the model after it has been put into the simulation
  linksSelection <- simpleJoin linksGroup Line   simulationLinks [ strokeWidth (sqrt <<< link_.value), strokeColor link_.color ]
  nodesSelection <- simpleJoin nodesGroup Circle simulationNodes [ radius 5.0, fill datum_.colorByGroup ]

  -- both links and nodes are updated on each step of the simulation, 
  -- in this case it's a simple translation of underlying (x,y) data for the circle centers
  -- tick functions have names, in this case "nodes" and "links"
  addTickFunction "nodes" $ Step nodesSelection [ cx datum_.x, cy datum_.y  ]
  addTickFunction "links" $ Step linksSelection [ x1 (_.x <<< link_.source)
                                                , y1 (_.y <<< link_.source)
                                                , x2 (_.x <<< link_.target)
                                                , y2 (_.y <<< link_.target)
                                                ]
  _ <- nodesSelection `on` Drag DefaultDrag

  _ <- svg `on`  Zoom { extent    : ZoomExtent { top: 0.0, left: 0.0 , bottom: h, right: w }
                      , scale     : ScaleExtent 1.0 4.0 -- wonder if ScaleExtent ctor could be range operator `..`
                      , name : "LesMis"
                      }

  pure unit
  """

blurbtext :: String
blurbtext = 
  """This example introduces a new capability, signalled by the SimulationM constraint on the function. This monad runs with a D3 Simulation engine in its State. This allows us to let the simulation engine do the layout, we provide the nodes and (optionally) links and configure the simulation with additional forces.

From the D3 docs: 
"This module implements a velocity Verlet numerical integrator for simulating
physical forces on particles. The simulation is simplified: it assumes a
constant unit time step Δt = 1 for each step, and a constant unit mass m = 1
for all particles. As a result, a force F acting on a particle is equivalent to
a constant acceleration a over the time interval Δt, and can be simulated
simply by adding to the particle’s velocity, which is then added to the
particle’s position."" 


"""

