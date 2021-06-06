module Stories.Trees where

import Prelude

import Affjax (Error)
import Control.Monad.State (class MonadState, get, put)
import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeModel, TreeType(..))
import D3.Examples.Tree.Configure as Tree
import D3.Layouts.Hierarchical (getTreeViaAJAX, makeModel)
import Data.Array (catMaybes)
import Data.Const (Const)
import Data.Either (Either(..)) as E
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Query :: forall k. k -> Type
type Query = Const Void

data Action
  = Initialize
  | Layout TreeLayout TreeType
  
type State = Maybe TreeModel

component :: forall m. MonadAff m => H.Component Query Unit Void m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize }
  }
  where

  initialState :: State
  initialState = Nothing

  showState :: Maybe TreeModel -> String
  showState Nothing = "There is no model yet"
  showState (Just model) = show model.treeType <> " " <> show model.treeLayout

  treeClasses :: State -> Array HH.ClassName
  treeClasses model = do
    let classStrings = catMaybes [ Just "trees", (show <<< _.treeLayout) <$> model]
    HH.ClassName <$> classStrings
  
  controls model = do
    let
      active = showState model
    [ layoutButton active Vertical TidyTree
    , layoutButton active Vertical Dendrogram
    , layoutButton active Radial TidyTree
    , layoutButton active Radial Dendrogram
    , layoutButton active Horizontal TidyTree
    , layoutButton active Horizontal Dendrogram
    ]

  layoutButton active layout treetype = HH.div [buttonClass] [ HH.button [ HE.onClick $ const (Layout layout treetype) ] [ HH.text $ show layout <> " " <> show treetype ] ]
    where
      buttonClass = 
        HP.classes [ HH.ClassName $
                      if active == show treetype <> " " <> show layout
                      then "active"
                      else "inactive" 
                  ]

  render :: State -> H.ComponentHTML Action () m
  render state =  
      HH.div [ HP.id "d3story", HP.classes $ treeClasses state ]
      [ HH.div [ HP.id "banner" ] 
        [ HH.h1_ [ HH.text $ "Tree layout" ] ] 

      , HH.div [ HP.id "tree" ] [] -- the div where the d3 script will appear

      , HH.div [ HP.id "blurb" ] 
        [ HH.div [ HP.id "inner-blurb" ] [ HH.text blurbtext ] 
        , HH.div [ HP.id "controls" ] (controls state)
        ]

      , HH.div [ HP.id "code" ] [ HH.div [ HP.id "inner-code" ] [ HH.text codetext]]
      ]

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction Initialize = do
  treeJSON <- H.liftAff $ getTreeViaAJAX "http://localhost:1234/flare-2.json"

  case treeJSON of
    (E.Left err) -> pure unit
    (E.Right (tree :: TreeJson_)) -> do
      model <- H.liftAff $ makeModel TidyTree Vertical tree
      _     <- H.liftAff $ Tree.drawTree model
      H.modify_ (\_ -> Just model)
      pure unit
  pure unit

handleAction (Layout layout treetype) = do
  (model :: Maybe TreeModel) <- get
  case model of
    Nothing -> pure unit
    (Just model) -> do
      let updated = model { treeLayout = layout, treeType = treetype }
      _ <- H.liftAff $ Tree.drawTree updated
      put $ Just updated


codetext :: String
codetext = 
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
