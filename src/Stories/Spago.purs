module Stories.Spago where

import Prelude

import Control.Monad.State (class MonadState)
import D3.Examples.Spago as Spago
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

type Query :: forall k. k -> Type
type Query = Const Void

data Action
  = Initialize
  | Finalize
  
type State = { 
    fiber  :: Maybe (Fiber Unit)
}

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
  initialState = { fiber: Nothing }

  controls = [] -- placeholder for controls for theta, alpha etc

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Tailwind.apply "story-container" ]
      [ HH.div [ Tailwind.apply "story-panel-about"] 
          [ HH.text "Spago"
          , HH.text blurbtext 
          ]
      , HH.div [ Tailwind.apply "svg-container" ] []
      ]

  -- render :: State -> H.ComponentHTML Action () m
  -- render state =
  --   HH.div [ HP.id "d3story-overlay", HP.classes [ HH.ClassName "force" ] ]
  --     [ HH.div [ HP.id "blurb" ]  [ HH.h1_ [ HH.text "Source navigator using data from Spago / purs" ]
  --                                          , HH.div [ HP.id "inner-blurb" ] [ HH.text blurbtext ] ]
  --     , HH.div [ Tailwind.apply "svg-container" ] []
  --     ]

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  Initialize -> do
      detached <- H.liftEffect $ d3Run $ removeExistingSVG "div.svg-container"

      fiber <- H.liftAff $ forkAff $ Spago.drawGraph

      H.modify_ (\state -> state { fiber = Just fiber })

  Finalize -> do
      fiber <- H.gets _.fiber
      _ <- case fiber of
              Nothing      -> pure unit
              (Just fiber) -> H.liftAff $ killFiber (error "Cancelling fiber and terminating computation") fiber
      H.modify_ (\state -> state { fiber = Nothing })



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
