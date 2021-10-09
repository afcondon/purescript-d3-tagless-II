module Stories.MetaTree where

import Prelude

import Control.Monad.State (class MonadState)
import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeModel, TreeType(..))
import D3.Examples.MetaTree as MetaTree
import D3.Examples.Tree.Configure as Tree
import D3.Layouts.Hierarchical (getTreeViaAJAX, makeModel)
import D3Tagless.Block.Expandable as Expandable
import D3Tagless.Block.Toggle as Toggle
import D3Tagless.Instance.Selection (eval_D3M)
import D3Tagless.Utility (removeExistingSVG)
import Data.Either (Either(..)) as E
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
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
  | ToggleCard (Lens' State Expandable.Status)

type State = { 
    tree :: Maybe TreeModel
  , blurb :: Expandable.Status
  , code  :: Expandable.Status
}

_blurb :: Lens' State Expandable.Status
_blurb = prop (Proxy :: Proxy "blurb")

_code :: Lens' State Expandable.Status
_code = prop (Proxy :: Proxy "code")

component :: forall query output m. MonadAff m => H.Component query Unit output m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize }
  }
  where

  initialState :: State
  initialState = { tree: Nothing, blurb: Expandable.Collapsed, code: Expandable.Collapsed }
  
  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Utils.tailwindClass "story-container" ]
      [ HH.div
            [ Utils.tailwindClass "story-panel-controls"] 
            [ HH.text "Meta Tree"
            ]
      , HH.div
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
      , HH.div
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

drawMetaTree :: TreeJson_ -> Aff Unit
drawMetaTree json =
  MetaTree.drawTree =<< makeModel TidyTree Vertical =<< Tree.getMetaTreeJSON =<< makeModel TidyTree Radial json

selector :: String
selector = "div.d3story" -- TODO redo how all this svg nonsense is handled

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard lens -> do
    st <- H.get
    H.put (over lens not st)

  Initialize -> do
    detached <- H.liftEffect $ eval_D3M $ removeExistingSVG selector

    treeJSON <- H.liftAff $ getTreeViaAJAX "/flare-2.json"

    case treeJSON of
      (E.Left err) -> pure unit
      (E.Right (tree :: TreeJson_)) -> do
        _     <- H.liftAff $ drawMetaTree tree
        -- H.modify_ (\_ -> Just model)
        pure unit
    pure unit

codetext :: String
codetext = 
  """datum_ = {
    x     : (\d -> (unboxD3TreeNode d).x)
  , y     : (\d -> (unboxD3TreeNode d).y)
-- now the fields which are in the original data object, embedded in this tree object
  , symbol: (\d -> (unboxD3TreeNode d).data.symbol)
  , param1: (\d -> (unboxD3TreeNode d).data.param1)
  , positionXY: (\d -> "translate(" <> show (datum_.x d) <> "," <> show (datum_.y d) <>")")
}

-- | Evaluate the tree drawing script in the "d3" monad which will render it in SVG
-- | TODO specialize runD3M so that this function isn't necessary
drawTree :: TreeModel -> Aff Unit
drawTree treeModel = liftEffect $ do
  widthHeight <- getWindowWidthHeight
  let tree = hierarchyFromJSON_ treeModel.json
  (_ :: Tuple D3Selection_ Unit) <- runD3M (treeScript widthHeight tree)
  pure unit

-- | "script" to produce the documentation-ready rendering of another script's structure
-- | (could also be the basis for graphical editor of scripts / trees)
treeScript :: forall m selection. Bind m => SelectionM selection m => 
  Tuple Number Number -> MetaTreeNode -> m selection
treeScript (Tuple width height) tree = do
  let 
    -- configure dimensions
    columns                    = 3.0  -- 3 columns, set in the grid CSS in index.html
    gap                        = 10.0 -- 10px set in the grid CSS in index.html
    svgWH                      = { width : ((width - ((columns - 1.0) * gap)) / columns)
                                 , height: height / 2.0 } -- 2 rows
    numberOfLevels             = (hNodeHeight_ tree) + 1.0
    spacing                    = { interChild: 120.0, interLevel: svgWH.height / numberOfLevels}
    layoutFn                   = (getLayout TidyTree) `treeSetNodeSize_` [ spacing.interChild, spacing.interLevel ]
    laidOutRoot_               = layoutFn `runLayoutFn_` tree
    { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot_
    xExtent                    = xMax - xMin -- ie if tree spans from -50 to 200, it's extent is 250
    yExtent                    = yMax - yMin -- ie if tree spans from -50 to 200, it's extent is 250


  -- "script"
  root       <- attach ".svg-container"                           
  svg        <- root `appendTo` (node Svg  [ viewBox (-svgWH.width / 2.0) (-80.0) 800.0 800.0 -- svgWH.width svgWH.height
                                         , classed "metatree" ] )
  container  <- svg  `appendTo` (node Group [ fontFamily      "sans-serif"
                                          , fontSize        18.0
                                          ])
  links      <- container `appendTo` (node Group [ classed "links"])
  nodes      <- container `appendTo` (node Group [ classed "nodes"])

  theLinks_  <- links <+> Join Path (links_ tree) [ strokeWidth   1.5
                                                  , strokeColor   "black"
                                                  , strokeOpacity 0.4
                                                  , fill          "none"
                                                  , verticalLink
                                                  ]

  nodeJoin_  <- nodes <+> Join Group (descendants_ tree) [ transform [ datum_.positionXY ] ]
  

  theNodes <- nodeJoin_ `appendTo` 
                (node Circle  [ fill         "blue"
                              , radius       20.0
                              , strokeColor "white"
                              , strokeWidth 3.0
                              ])

  labelsWhite <- nodeJoin_ `appendTo`
                (node Text  [ x          0.0
                            , y          3.0
                            , textAnchor "middle"
                            , text       datum_.symbol
                            , fill       "white"
                            ])
                            
  labelsGray <- nodeJoin_ `appendTo`
                (node Text  [ x          22.0
                            , y          3.0
                            , textAnchor "start"
                            , text       datum_.param1
                            , fill       "gray"
                            ])
                            
  pure svg
  """

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
