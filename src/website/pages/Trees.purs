module PSD3.Pages.Trees where

import Prelude

import Control.Monad.State (class MonadState, get)
import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeModel, TreeType(..))
import D3.Examples.Tree.Configure as Tree
import D3.Layouts.Hierarchical (getTreeViaAJAX, makeModel)
import PSD3.Components.Blocks.Expandable as Expandable
import PSD3.Components.Blocks.Toggle as Toggle
import D3Tagless.Instance.Selection (eval_D3M)
import D3Tagless.Utility (removeExistingSVG)
import Data.Either (Either(..)) as E
import Data.Lens (Lens', view, (%=), (.=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Components.UI.FormField as FormField
import PSD3.Components.UI.Radio as Radio
import PSD3.Components.UI.Properties (css)
import Snippets (readSnippetFiles)
import PSD3.Pages.Utilities (blurbParagraphs, syntaxHighlightedCode)
import PSD3.Pages.Utilities as Utils
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | SetLayout TreeLayout
  | SetType TreeType
  | ToggleCard (Lens' State Expandable.Status)

type State = { 
    tree :: Maybe TreeModel
  , panels  :: { blurb :: Expandable.Status, code :: Expandable.Status }
  , snippets :: { draw :: String }
}

_snippets = prop (Proxy :: Proxy "snippets")
_drawCode :: Lens' State String
_drawCode = _snippets <<< prop (Proxy :: Proxy "draw")

_panels = prop (Proxy :: Proxy "panels")

_blurb :: Lens' State Expandable.Status
_blurb = _panels <<< prop (Proxy :: Proxy "blurb")

_code :: Lens' State Expandable.Status
_code = _panels <<< prop (Proxy :: Proxy "code")

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
    , panels: { blurb:  Expandable.Collapsed, code:   Expandable.Collapsed }
    , snippets: { draw: "" }
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
                  $ Expandable.toBoolean (view _blurb state)
                , HE.onChange \_ -> ToggleCard _blurb
                ]
              ]
            , Expandable.content_ (view _blurb state) blurbtext
            ]  
      , HH.div
            [ Utils.tailwindClass "story-panel-code"]
            [ FormField.field_
                { label: HH.text "(hide this panel if screen too small)"
                , helpText: []
                , error: []
                , inputId: "show-code"
                }
              [ Toggle.toggle
                [ HP.id "show-code"
                , HP.checked
                  $ Expandable.toBoolean (view _code state)
                , HE.onChange \_ -> ToggleCard _code
                ]
              ]
            , Expandable.content_ (view _code state) $ syntaxHighlightedCode (view _drawCode state) 
            ]  
      , HH.div [ Utils.tailwindClass "svg-container" ] []
      ]

handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard _cardState -> _cardState %= not

  Initialize -> do
    detached <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.svg-container"

    text1 <- H.liftAff $ readSnippetFiles "TreeDraw"
    _drawCode .= text1

    treeJSON <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"

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
