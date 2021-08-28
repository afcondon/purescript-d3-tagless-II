module Stories.Trees where

import Prelude

import Control.Monad.State (class MonadState, get)
import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeModel, TreeType(..))
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
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.FormField as FormField
import Ocelot.Block.Radio as Radio
import Ocelot.HTML.Properties (css)
import Stories.Utilities (blurbParagraphs, syntaxHighlightedCode)
import Stories.Utilities as Utils
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | SetLayout TreeLayout
  | SetType TreeType
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
  initialState = { 
      tree: Nothing
    , blurb: Expandable.Collapsed
    , code: Expandable.Collapsed
  }
  
  controlsRadio =
    HH.div
      [ css "flex-1" ]
      [ FormField.fieldset_
        { label: HH.text "Tree orientation"
        , inputId: "radio-vertical"
        , helpText: []
        , error: []
        }
        [ HH.div
          [ css "flex-1" ]
          [ Radio.radio
            [ css "pr-6" ]
            [ HP.name "tree-layout"
            , HP.checked true
            , HE.onClick $ const (SetLayout Vertical)
            ]
            [ HH.text "Vertical" ]
          , Radio.radio
            [ css "pr-6" ]
            [ HP.name "tree-layout"
            , HE.onClick $ const (SetLayout Horizontal) ]
            [ HH.text "Horizontal" ]
          , Radio.radio
            [ css "pr-6" ]
            [ HP.name "tree-layout"
            , HE.onClick $ const (SetLayout Radial) ]
            [ HH.text "Radial" ]
          ]
        ]
      , FormField.fieldset_
        { label: HH.text "Tree topology"
        , inputId: "radio-vertical"
        , helpText: []
        , error: []
        }
        [ HH.div
          [ css "flex-1" ]
          [ Radio.radio
            [ css "pr-6" ]
            [ HP.name "tree-type"
            , HP.checked true
            , HE.onClick $ const (SetType TidyTree)
            ]
            [ HH.text "TidyTree" ]
          , Radio.radio
            [ css "pr-6" ]
            [ HP.name "tree-type"
            , HE.onClick $ const (SetType Dendrogram) ]
            [ HH.text "Dendrogram" ]
          ]
        ]
      ]

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div [ Utils.tailwindClass "story-container" ]
      [ HH.div [ Utils.tailwindClass "story-panel-controls"] 
          [ controlsRadio ]
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
            , Expandable.content_ state.blurb blurbtext
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

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard lens -> do
    st <- H.get
    H.put (over lens not st)

  Initialize -> do
    detached <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.svg-container"

    treeJSON <- H.liftAff $ getTreeViaAJAX "http://localhost:1234/flare-2.json"

    case treeJSON of
      (E.Left err) -> pure unit
      (E.Right (treeJSON :: TreeJson_)) -> do
        model <- H.liftAff $ makeModel TidyTree Vertical treeJSON
        _     <- H.liftAff $ Tree.drawTree model "div.svg-container"
        H.modify_ (\st -> st { tree = Just model } )
        pure unit
    pure unit

  (SetLayout layout) -> do
    detached <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.svg-container"

    { tree } <- get
    case tree of
      Nothing -> pure unit
      (Just tree) -> do
        let updated = tree { treeLayout = layout }
        _ <- H.liftAff $ Tree.drawTree updated "div.svg-container"
        H.modify_ (\st -> st { tree = Just updated } )
        pure unit

  (SetType  treetype) -> do
    detached <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.svg-container"

    { tree } <- get
    case tree of
      Nothing -> pure unit
      (Just tree) -> do
        let updated = tree { treeType = treetype }
        _ <- H.liftAff $ Tree.drawTree updated "div.svg-container"
        H.modify_ (\st -> st { tree = Just updated } )
        pure unit


codetext :: String
codetext = 
  """
-- a record that packages up all the customizations that are needed to render the 6 variations on Tree
type ScriptConfig = { 
    layout        :: TreeLayout
  , selector      :: Selector String
  , linkPath      :: ChainableS
  , spacing       :: { interChild :: Number, interLevel :: Number }
  , viewbox       :: Array ChainableS
  , nodeTransform :: Array ChainableS
  , color         :: String
  , svg           :: { width :: Number, height :: Number }
}

-- | The eDSL script that renders tree layouts
-- | it has been parameterized rather heavily using the ScriptConfig record so that it can draw
-- | all six variations of [Radial, Horizontal, Vertical] * [Dendrogram, TidyTree] 
-- | NB there would be nothing wrong, per se, with individual examples, this just shows 
-- | some more composability, at the price of some direct legibility
script :: forall m selection. Bind m => SelectionM selection m => 
  ScriptConfig -> FlareTreeNode ->  m selection
script config tree = do
  root       <- attach config.selector  
  svg        <- root D3.+ (node Svg (config.viewbox <> [ classed "tree"]))          
  container  <- svg  D3.+ (node Group [ fontFamily      "sans-serif", fontSize 10.0 ])
  links      <- container D3.+  (node Group [ classed "links"] )
  nodes      <- container D3.+  (node Group [ classed "nodes"] )

  theLinks_  <- links D3.<-> Join Path (links_ tree) 
                                       [ strokeWidth   1.5, strokeColor   config.color, strokeOpacity 0.4
                                       , fill "none", config.linkPath ]

  -- we make a group to hold the node circle and the label text
  nodeJoin_  <- nodes D3.<-> Join Group (descendants_ tree) config.nodeTransform

  theNodes <- nodeJoin_ D3.+  
                (node Circle  [ fill         (\d -> if datum_.hasChildren d then "#999" else "#555")
                              , radius       2.5
                              , strokeColor "white"
                              ])

  theLabels <- nodeJoin_ D3.+
                (node Text  [ dy         0.31
                            , x          (datum_.textX config.layout)
                            , textAnchor (datum_.textAnchor config.layout)
                            , text       datum_.name
                            , fill       config.color
                            ])               
  pure svg
  """

blurbtext :: forall t235 t236. Array (HH.HTML t235 t236)
blurbtext = blurbParagraphs [

    """An abstract data type like a tree can be rendered in a number of different
    ways including at least the 6 variations shown here, arising from a
    combination of three layout orientations (Horizontal, Vertical and Radial)
    and to layout types (TidyTree or Dendrogram)"""

  , """Each format has it's uses, TidyTree forms are generally more compact and
  will often be preferred."""

  , """In addition to the six options shown here (which have fundamentally the
  same structure in the DOM) there are radically different representations such
  as Sunflowers and TreeMaps which can be used to show the same hierarchical
  data in ways that serve different purposes or make different aspects of the
  data salient."""

  , """The code shown in this example makes use of higher order functions to
  parameterize the drawing, thus enabling one function to encode all six
  forms."""

]