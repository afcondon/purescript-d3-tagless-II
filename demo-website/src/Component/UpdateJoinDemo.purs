module Component.UpdateJoinDemo where

import Prelude

import D3.Viz.TreeAPI.UpdateJoinDemo as Demo
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Interpreter.D3 (runD3v2M)

type State = Unit

data Action
  = Initialize
  | ShowInitial
  | ShowThree
  | ShowSeven

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> unit
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.class_ (H.ClassName "scene-join-demo-container") ]
    [ HH.h1_ [ HH.text "UpdateNestedJoin Demo" ]
    , HH.p_
        [ HH.text "This demonstrates UpdateNestedJoin - the clean solution for General Update Pattern with type decomposition!" ]

    , HH.div
        [ HP.class_ (H.ClassName "controls") ]
        [ HH.button
            [ HE.onClick \_ -> ShowInitial
            , HP.class_ (H.ClassName "btn btn-primary")
            ]
            [ HH.text "Show 5 Circles (Initial)" ]
        , HH.button
            [ HE.onClick \_ -> ShowThree
            , HP.class_ (H.ClassName "btn btn-warning")
            ]
            [ HH.text "Show 3 Circles (Exit)" ]
        , HH.button
            [ HE.onClick \_ -> ShowSeven
            , HP.class_ (H.ClassName "btn btn-success")
            ]
            [ HH.text "Show 7 Circles (Enter)" ]
        ]

    , HH.div
        [ HP.id "scene-join-demo-viz"
        , HP.class_ (H.ClassName "visualization")
        ]
        []

    , HH.div
        [ HP.class_ (H.ClassName "explanation") ]
        [ HH.h3_ [ HH.text "What's Happening?" ]
        , HH.ul_
            [ HH.li_ [ HH.text "Initial: 5 circles enter from center with transition" ]
            , HH.li_ [ HH.text "Show 3: 2 circles exit (shrink away), 3 update positions" ]
            , HH.li_ [ HH.text "Show 7: 2 new circles enter from center, 5 update positions" ]
            ]
        , HH.h3_ [ HH.text "The Code" ]
        , HH.pre_
            [ HH.code_
                [ HH.text """T.updateNestedJoin "circles" "circle"
  [scene]              -- Outer data: SceneData
  (_.points)           -- Decompose to Array DataPoint
  (\\point -> ...)      -- Template
  { enterBehavior: Just { initialAttrs: [...], transition: ... }
  , updateBehavior: Just { attrs: [], transition: ... }
  , exitBehavior: Just { attrs: [...], transition: ... }
  }"""
                ]
            ]
        ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    liftEffect $ runD3v2M $ Demo.drawInitial "#scene-join-demo-viz"

  ShowInitial -> do
    liftEffect $ runD3v2M $ Demo.drawInitial "#scene-join-demo-viz"

  ShowThree -> do
    liftEffect $ runD3v2M $ Demo.updateToThree "#scene-join-demo-viz"

  ShowSeven -> do
    liftEffect $ runD3v2M $ Demo.updateToSeven "#scene-join-demo-viz"
