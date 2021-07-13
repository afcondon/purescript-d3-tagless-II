module Stories.LesMis where

import Prelude

import Affjax as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, put)
import D3.Examples.LesMiserables as LesMis
import D3.Examples.LesMiserables.File (readGraphFromFileContents)
import D3.Simulation.Config as F
import D3.Simulation.Forces (createForce, enableForce)
import D3.Simulation.Functions (simulationStart)
import D3.Simulation.Types (Force, ForceType(..), SimVariable(..), SimulationState_)
import D3Tagless.Block.Button as Button
import D3Tagless.Block.Expandable as Expandable
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Capabilities (ForceConfigLists, loadForces, removeAllForces, setConfigVariable, setForcesByLabel)
import D3Tagless.Instance.Simulation (exec_D3M_Simulation, runEffectSimulation)
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Effect.Aff (Fiber)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.FormField as FormField
import Stories.Tailwind.Styles as Tailwind
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | Finalize
  | ToggleCard (Lens' State Expandable.Status)
  | ConfigureForces ForceConfigLists
  | Freeze
  | Reheat

type Input = SimulationState_

type State = { 
    simulationState :: SimulationState_
  , blurb           :: Expandable.Status
  , code            :: Expandable.Status
}

_blurb :: Lens' State Expandable.Status
_blurb = prop (Proxy :: Proxy "blurb")

_code :: Lens' State Expandable.Status
_code = prop (Proxy :: Proxy "code")

lesMisForces :: Array Force
lesMisForces = 
    [ createForce "center" ForceCenter  [ F.x 0.0, F.y 0.0, F.strength 1.0 ]
    , createForce "many body" ForceManyBody  []
    , createForce "collision" ForceCollide  [ F.radius 4.0 ]
    , createForce "collision20" ForceCollide  [ F.radius 20.0]
    ]

component :: forall query output m. 
  MonadAff m => 
  H.Component query Input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    , finalize   = Just Finalize }
  }
  where
  initialState :: Input -> State
  initialState simulation = { 
        simulationState: simulation
      , blurb: Expandable.Collapsed
      , code: Expandable.Collapsed
    }

  controls = 
    [ HH.div
      [ Tailwind.apply "story-panel-controls"] 
      [ Button.buttonGroup [ HP.class_ $ HH.ClassName "flex-col" ]
        [ Button.buttonVertical
          [ HE.onClick (const $ ConfigureForces { enable: ["center"], disable: [""]}) ]
          [ HH.text "Centering" ]
        , Button.buttonVertical
          [ HE.onClick (const $ ConfigureForces { enable: ["many body"], disable: [""]}) ]
          [ HH.text "Many body" ]
        , Button.buttonVertical
          [ HE.onClick (const $ ConfigureForces { enable: ["collision"], disable: ["collision20"]}) ]
          [ HH.text "Collision 4" ]
        , Button.buttonVertical
          [ HE.onClick (const $ ConfigureForces { enable: ["collision20"], disable: ["collision"]}) ]
          [ HH.text "Collision 20" ]
        , Button.buttonVertical
          [ HE.onClick (const $ Freeze) ]
          [ HH.text "Freeze" ]
        , Button.buttonVertical
          [ HE.onClick (const $ Reheat) ]
          [ HH.text "Reheat!" ]
        ]
      ]
    ]

    
  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Tailwind.apply "story-container" ]
      [ HH.div -- [ Tailwind.apply "story-panel"]
        [ Tailwind.apply "story-panel-controls"] 
        controls
      , HH.div -- [ Tailwind.apply "story-panel" ] 
            [ Tailwind.apply "story-panel-about"]
            [ FormField.field_
              { label: HH.text "About"
              , helpText: []
              , error: []
              , inputId: "show-blurb"
              }
              [ Toggle.toggle
                [ HP.id_ "show-blurb"
                , HP.checked
                  $ Expandable.toBoolean state.blurb
                , HE.onChange \_ -> ToggleCard _blurb
                ]
              ]
            , Expandable.content_ state.blurb [ HH.text blurbtext ]
            ]  
      , HH.div -- [ Tailwind.apply "story-panel" ] 
            [ Tailwind.apply "story-panel-code"]
            [ FormField.field_
                { label: HH.text "Code"
                , helpText: []
                , error: []
                , inputId: "show-code"
                }
              [ Toggle.toggle
                [ HP.id_ "show-code"
                , HP.checked
                  $ Expandable.toBoolean state.code
                , HE.onChange \_ -> ToggleCard _code
                ]
              ]
            , Expandable.content_ state.code [ HH.pre_ [ HH.code_ [ HH.text codetext] ] ]
            ]  
      , HH.div [ Tailwind.apply "svg-container" ] []
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

    runEffectSimulation (loadForces lesMisForces)
    runEffectSimulation (LesMis.graphScript graph "div.svg-container")

  Finalize ->  runEffectSimulation removeAllForces

  ConfigureForces enableDisable -> do
    runEffectSimulation (setForcesByLabel enableDisable)
    runEffectSimulation (setConfigVariable $ Alpha 0.8)

  Freeze  -> runEffectSimulation (setConfigVariable $ Alpha 0.0)
  Reheat  -> simulationStart


codetext :: String
codetext = 
  """script :: forall m. SelectionM D3Selection_ m => m ((Array Char) -> m D3Selection_)
  script = do 
    let 
      transition :: ChainableS
      transition = transitionWithDuration $ Milliseconds 2000.0
      -- new entries enter at this position, updating entries need to transition to it on each update
      xFromIndex :: Datum_ -> Index_ -> Number
      xFromIndex _ i = 50.0 + ((indexIsNumber i) * 48.0)

    root        <- attach "div#gup"
    svg         <- append root $ node Svg [ viewBox 0.0 0.0 650.0 650.0 ]
    letterGroup <- append svg  $ node_ Group

    pure $ \letters -> 
      do 
        letterGroup <+> JoinGeneral {
            element   : Text
          , key       : UseDatumAsKey
          , "data"    : letters
          , behaviour : { 
              enter:  [ classed  "enter"
                      , fill     "green"
                      , x        xFromIndex
                      , y        0.0
                      -- , yu (NWU { i: 0, u: Px })
                      , text     (singleton <<< datumIsChar)
                      , fontSize 48.0
                      ]  
                      `andThen` (transition `to` [ y 200.0 ]) 

            , update: [ classed "update"
                      , fill "gray"
                      , y 200.0
                      ] 
                      `andThen` (transition `to` [ x xFromIndex ] ) 

            , exit:   [ classed "exit"
                      , fill "brown"
                      ] 
                      `andThen` (transition `to` [ y 400.0, remove ])
            }
        }"""

blurbtext :: String
blurbtext = 
  """Id sint laboris reprehenderit officia anim nisi consectetur voluptate enim.
  Commodo cillum minim nisi laborum eiusmod veniam ullamco id ex fugiat eu anim.
  Irure est aute laborum duis. Lorem dolore id sunt incididunt ut ea. Nostrud
  enim officia nisi anim consequat cupidatat consectetur consequat ex excepteur.
  Lorem nisi in reprehenderit ex adipisicing magna elit aute sunt. Cillum non
  Lorem minim duis culpa ullamco aute ex minim. Mollit anim in nisi tempor enim
  exercitation dolore. Veniam consequat minim nostrud amet duis dolore tempor
  voluptate quis culpa. Laborum dolor pariatur ut est cupidatat elit deserunt
  occaecat tempor aliquip anim. 
  
  Velit irure ea voluptate ipsum ex exercitation
  dolore voluptate reprehenderit sit anim sunt. Anim fugiat ad ut qui cillum
  tempor occaecat et deserunt nostrud non ipsum. Id non qui mollit culpa elit
  cillum ipsum excepteur adipisicing qui. Incididunt adipisicing sit incididunt
  consequat minim id do exercitation cupidatat est sunt mollit. Anim ut ullamco
  enim culpa. Adipisicing ad non esse laboris anim consequat ut velit esse
  consequat tempor. Commodo magna esse ullamco ipsum et ipsum minim dolore esse
  veniam ea commodo labore. Nulla deserunt id ad anim anim proident labore
  occaecat sint esse nostrud. Duis velit nostrud ullamco cillum cillum Lorem
  cupidatat irure."""
