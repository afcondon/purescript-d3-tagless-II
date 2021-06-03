module Stories.LesMis where

import Prelude

import Control.Monad.State (class MonadState)
import D3.Examples.LesMiserables as LesMis
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Fiber, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Query :: forall k. k -> Type
type Query = Const Void

data Action
  = Initialize
  | Finalize
  
type State = { fiber  :: Maybe (Fiber Unit) }

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

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ HP.id "d3story"]
      [ HH.div
        [ HP.id "controls" ]
        [ HH.h1_ [ HH.text "Force Layout Simulation" ]]

      , HH.div [ HP.id "blurb" ]

          [ HH.text 
          
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
  
  ]

      , HH.div [ HP.id "code" ]
          [ HH.text

"""script :: forall m. D3InterpreterM D3Selection_ m => m ((Array Char) -> m D3Selection_)
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

          ]

      , HH.div -- the div where the d3 script will appear
          [ HP.id "force" ]
          []
      ]

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction Initialize = do
    fiber <- H.liftAff $ forkAff $ LesMis.drawGraph

    H.modify_ (\state -> state { fiber = Just fiber })

handleAction Finalize = do
    fiber <- H.gets _.fiber
    _ <- case fiber of
            Nothing      -> pure unit
            (Just fiber) -> H.liftAff $ killFiber (error "Cancelling fiber and terminating computation") fiber
    H.modify_ (\state -> state { fiber = Nothing })
