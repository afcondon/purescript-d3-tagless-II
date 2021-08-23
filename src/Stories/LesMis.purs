module Stories.LesMis where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, modify_)
import D3.Examples.LesMiserables as LesMis
import D3.Examples.LesMiserables.File (readGraphFromFileContents)
import D3.Simulation.Config as F
import D3.Simulation.Forces (createForce, enableForce)
import D3.Simulation.Functions (simulationStart)
import D3.Simulation.Types (D3SimulationState_, Force, RegularForceType(..), SimVariable(..), allNodes, initialSimulationState)
import D3Tagless.Block.Button as Button
import D3Tagless.Block.Expandable as Expandable
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Capabilities (addForces, removeAllForces, setConfigVariable, setForcesByLabel)
import D3Tagless.Instance.Simulation (runEffectSimulation)
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.FormField as FormField
import Stories.Utilities (syntaxHighlightedCode)
import Stories.Utilities as Utils
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | Finalize
  | ToggleCard (Lens' State Expandable.Status)
  | ToggleManyBody
  | ToggleLinks
  | Freeze
  | Reheat

data ManyBodyParam = SmallRadius | BigRadius
derive instance Eq ManyBodyParam
instance Show ManyBodyParam where
  show SmallRadius = "Compact"
  show BigRadius   = "Expanded"
data LinksSetting  = LinksOn | LinksOff
derive instance Eq LinksSetting
instance Show LinksSetting where
  show LinksOn = "Link Force"
  show LinksOff = "No link force"

type State = { 
    simulation :: D3SimulationState_
  , manybodySetting :: ManyBodyParam
  , linksSetting    :: LinksSetting
  , blurb           :: Expandable.Status
  , code            :: Expandable.Status
  , forces          :: Array Force
}

_blurb :: Lens' State Expandable.Status
_blurb = prop (Proxy :: Proxy "blurb")

_code :: Lens' State Expandable.Status
_code = prop (Proxy :: Proxy "code")

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
      , manybodySetting: SmallRadius
      , linksSetting: LinksOn
      , blurb: Expandable.Collapsed
      , code: Expandable.Collapsed
      , forces: [ 
          enableForce $ createForce "center"      ForceCenter   allNodes [ F.x 0.0, F.y 0.0, F.strength 1.0 ]
        , enableForce $ createForce "many body"   ForceManyBody allNodes []
        , enableForce $ createForce "collision"   ForceCollide  allNodes [ F.radius 4.0 ]
        ,               createForce "collision20" ForceCollide  allNodes [ F.radius 20.0] -- NB initially not enabled
        ]
    }

  controls state = 
    [ HH.div
      [ Utils.tailwindClass "story-panel-controls"] 
      [ Button.buttonGroup [ HP.class_ $ HH.ClassName "flex-col" ]
        [ Button.buttonVertical
          [ HE.onClick (const $ ToggleLinks) ] -- { enable: ["links"], disable: [""]}
          [ HH.text $ show state.linksSetting ]
        , Button.buttonVertical
          [ HE.onClick (const $ ToggleManyBody) ]
          [ HH.text $ show state.manybodySetting ]
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
    runEffectSimulation $ addForces state.forces
    runEffectSimulation $ LesMis.graphScript graph "div.svg-container"

  Finalize ->  runEffectSimulation removeAllForces

  ToggleManyBody -> do
    state <- H.get
    let newSetting = case state.manybodySetting of
                      SmallRadius -> BigRadius
                      BigRadius   -> SmallRadius
    case newSetting of
      SmallRadius -> runEffectSimulation $ setForcesByLabel { enable: ["collision"], disable: ["collision20"]} 
      BigRadius   -> runEffectSimulation $ setForcesByLabel { enable: ["collision20"], disable: ["collision"]} 
    modify_ (\s -> s { manybodySetting = newSetting })
    simulationStart

  ToggleLinks -> do
    state <- H.get
    let newSetting = case state.linksSetting of
                      LinksOn  -> LinksOff
                      LinksOff -> LinksOn
    case newSetting of
      LinksOn  -> runEffectSimulation $ setForcesByLabel { enable: ["links"], disable: []} 
      LinksOff -> runEffectSimulation $ setForcesByLabel { enable: [], disable: ["links"]} 
    modify_ (\s -> s { linksSetting = newSetting })
    simulationStart

  Freeze  -> runEffectSimulation $ setConfigVariable $ Alpha 0.0
  Reheat  -> simulationStart


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
  linksSelection <- linksGroup D3.<+> Join Line   simulationLinks [ strokeWidth (sqrt <<< link_.value), strokeColor link_.color ]
  nodesSelection <- nodesGroup D3.<+> Join Circle simulationNodes [ radius 5.0, fill datum_.colorByGroup ]

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
