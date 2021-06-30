module Stories.LesMis where

import Prelude

import Control.Monad.State (class MonadState)
import D3.Examples.LesMiserables as LesMis
import D3.Interpreter.D3 (d3Run, removeExistingSVG)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Fiber, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Stories.Tailwind.Styles as Tailwind
import Ocelot.Block.FormField as FormField
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Block.Expandable as Expandable
import Halogen.HTML.Events as HE
import D3Tagless.Block.Toggle as Toggle
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

type Query :: forall k. k -> Type
type Query = Const Void

data Action
  = Initialize
  | Finalize
  | ToggleCard (Lens' State Expandable.Status)

type State = { 
    fiber :: Maybe (Fiber Unit)
  , blurb :: Expandable.Status
  , code  :: Expandable.Status
}

_blurb :: Lens' State Expandable.Status
_blurb = prop (Proxy :: Proxy "blurb")

_code :: Lens' State Expandable.Status
_code = prop (Proxy :: Proxy "code")

component :: forall m. MonadAff m => H.Component Query Unit Void m
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
      fiber: Nothing
    , blurb: Expandable.Collapsed
    , code: Expandable.Collapsed
  }

  controls = [] -- placeholder for controls for theta, alpha etc
    
  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Tailwind.apply "story-container" ]
      [ HH.div -- [ Tailwind.apply "story-panel"]
        [ Tailwind.apply "story-panel-controls"] 
        [ HH.text "Les Mis" ]
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

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard lens -> do
    st <- H.get
    H.put (over lens not st)

  Initialize -> do
    detached <- H.liftEffect $ d3Run   $ removeExistingSVG "div.svg-container"
    fiber    <- H.liftAff    $ forkAff $ LesMis.drawGraph  "div.svg-container"

    H.modify_ (\state -> state { fiber = Just fiber })

  Finalize -> do
    fiber <- H.gets _.fiber
    _ <- case fiber of
            Nothing      -> pure unit
            (Just fiber) -> H.liftAff $ killFiber (error "Cancelling fiber and terminating computation") fiber
    H.modify_ (\state -> state { fiber = Nothing })





codetext :: String
codetext = 
  """script :: forall m. D3SelectionM D3Selection_ m => m ((Array Char) -> m D3Selection_)
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
