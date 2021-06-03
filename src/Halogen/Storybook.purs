module Halogen.Storybook
  ( Stories
  , StoryQuery
  , runStorybook
  , module Halogen.Storybook.Proxy
  ) where

import Prelude

import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (foldMapDefaultL)
import Data.Maybe (Maybe(..), fromJust)
import Data.String as String
import Data.Tuple (Tuple(..), fst)
import Effect.Aff (Aff, launchAff_)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Storybook.Proxy (proxy)
import Halogen.VDom.Driver (runUI)
import JSURI (decodeURIComponent, encodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Routing.Hash (hashes)
import Web.HTML.HTMLElement (HTMLElement)
import Type.Proxy (Proxy(..))

data Query a = RouteChange String a

type Action = Void

type State =
  { route :: String
  }

type StoryQuery = Const Void :: Type -> Type

-- | Stories config, each story consists of a story name and a component.
-- | Note the component needs to be proxied explicitly.
-- |
-- | ```
-- | stories :: forall m. Stories m
-- | stories = Object.fromFoldable
-- |   [ Tuple "count" $ proxy $ ExpCount.component
-- | ```
type Stories m = Object.Object (H.Component StoryQuery Unit Void m)

type Slots = (child :: H.Slot StoryQuery Void String)

_child = Proxy :: Proxy "child"

type HTML m = H.ComponentHTML Action Slots m

type Config m =
  { stories :: Stories m
  , logo :: Maybe HH.PlainHTML
  }

class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ = HP.class_ <<< HH.ClassName

type StoryName =
  { name :: String
  , path :: String
  }

renderStoryNames :: forall m. State -> Array StoryName -> HTML m
renderStoryNames { route } items =
  HH.ul
  [ class_ "Storybook-nav-list"
  ] $ (Array.sortWith _.name items) <#> \item ->
    HH.li_
    [ HH.a
      [ class_ if route == item.path then linkActiveClass else linkClass
      , HP.href $ "#" <> (unsafePartial $ fromJust $ encodeURIComponent item.path)
      ]
      [ HH.text item.name ]
    ]
  where
  linkClass = "Storybook-link"
  linkActiveClass = linkClass <> " is-active"

renderSidebar :: forall m. Stories m -> State -> HTML m
renderSidebar stories state =
  HH.div [ class_ "Storybook-nav" ] $
    sorted <#> \(Tuple section items) -> case section of
      "" ->
        HH.div
        [ class_ "Storybook-nav-section" ]
        [ renderStoryNames state items ]
      _ ->
        HH.div
        [ class_ "Storybook-nav-section" ]
        [ HH.div
          [ class_ "Storybook-nav-section-title" ]
          [ HH.text section ]
        , renderStoryNames state items
        ]
  where
  nameObj = (Object.keys stories) # foldMapDefaultL case _ of
    "" -> Object.empty
    path ->
      case String.indexOf (String.Pattern "|") path of
        Nothing -> Object.singleton "" [{ name: path, path}]
        Just index ->
          let { before, after } = String.splitAt index path
          in
            Object.singleton before
              [{name: String.drop 1 after, path}]
  sorted = Array.sortWith fst $ Object.toUnfoldable nameObj

renderMain :: forall m. Stories m -> State -> HTML m
renderMain stories state =
  case Object.lookup state.route stories of
    Just cmp -> HH.slot _child state.route cmp unit absurd
    _ ->
      HH.div_
      [ HH.h2_ [ HH.text "Hello world" ]
      , HH.p_
        [ HH.text "This site is powered by "
        , HH.a
          [ HP.href "https://github.com/rnons/purescript-halogen-storybook" ]
          [ HH.text "halogen-storybook" ]
        , HH.text "."
        ]
      ]

render :: forall m. Config m -> State -> HTML m
render { stories, logo } state =
  HH.div [ class_ "Storybook" ]
  [ HH.a
    [ class_ "Storybook-logo"
    , HP.href "#"
    ]
    [ case logo of
        Nothing -> HH.text "Halogen Storybook"
        Just logo' -> HH.fromPlainHTML logo'
    ]
  , renderSidebar stories state
  , HH.div [ class_ "Storybook-main" ]
    [ renderMain stories state  ]
  ]

app :: forall m. Config m -> H.Component Query Unit Void m
app config = H.mkComponent
  { initialState: const initialState
  , render: render config
  , eval: H.mkEval $ H.defaultEval
    { handleQuery = handleQuery
    }
  }
  where

  initialState :: State
  initialState = { route: "" }

handleQuery
  :: forall a m
   . Query a
  -> H.HalogenM State Action Slots Void m (Maybe a)
handleQuery = case _ of
  RouteChange route a -> do
    void $ H.modify (\state -> state { route = route })
    pure (Just a)

-- | Takes stories config and mount element, and renders the storybook.
runStorybook
  :: Config Aff
  -> HTMLElement
  -> Aff Unit
runStorybook config body = do
  app' <- runUI (app config) unit body
  void $ H.liftEffect $ hashes $ \_ next ->
    case decodeURIComponent next of
         Nothing -> pure unit
         Just next' -> launchAff_ $ app'.query (H.mkTell $ RouteChange next')
