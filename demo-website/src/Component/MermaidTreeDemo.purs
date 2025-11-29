module Component.MermaidTreeDemo where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3v2.Attribute.Types (AttributeName(..), AttributeValue(..))
import PSD3v2.Interpreter.MermaidTree (runMermaidTree)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3v2.VizTree.Tree as T
import PSD3v2.Attribute.Types (Attribute(..)) as Attr

-- FFI
foreign import runMermaid :: Effect Unit

type State =
  { mermaidCode :: Maybe String
  }

data Action
  = Initialize
  | GenerateSimpleTree
  | GenerateJoinTree

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { mermaidCode: Nothing }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "mermaid-tree-demo") ]
    [ HH.h1_ [ HH.text "Mermaid Tree API Visualizer" ]
    , HH.p_ [ HH.text "This demonstrates the new Mermaid interpreter for the Tree API." ]

    , HH.div
        [ HP.class_ (H.ClassName "controls") ]
        [ HH.button
            [ HP.class_ (H.ClassName "btn")
            , HE.onClick \_ -> GenerateSimpleTree
            ]
            [ HH.text "Generate Simple Tree" ]
        , HH.button
            [ HP.class_ (H.ClassName "btn")
            , HE.onClick \_ -> GenerateJoinTree
            ]
            [ HH.text "Generate Data Join Tree" ]
        ]

    , case state.mermaidCode of
        Nothing -> HH.div_ [ HH.text "Click a button to generate a Mermaid diagram" ]
        Just code -> HH.div_
          [ HH.h2_ [ HH.text "Generated Mermaid Code:" ]
          , HH.pre
              [ HP.class_ (H.ClassName "mermaid-code") ]
              [ HH.text code ]
          , HH.div
              [ HP.class_ (H.ClassName "mermaid") ]
              [ HH.text code ]
          ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    pure unit

  GenerateSimpleTree -> do
    let
      tree = T.named SVG "svg" [ staticAttr "width" "800", staticAttr "height" "600" ] `T.withChildren`
        [ T.elem Group [ staticAttr "class" "container" ] `T.withChild`
            T.elem Circle [ staticAttr "r" "50", staticAttr "fill" "blue" ]
        , T.elem Text [ staticAttr "x" "100", staticAttr "y" "100" ]
        ]
    code <- liftEffect $ runMermaidTree tree
    H.modify_ _ { mermaidCode = Just code }
    -- Trigger Mermaid rendering after DOM update
    liftEffect runMermaid

  GenerateJoinTree -> do
    let
      sampleData = [ 1, 2, 3, 4, 5 ]
      tree = T.named SVG "svg" [ staticAttr "width" "800" ] `T.withChild`
        ( T.joinData "circles" "circle" sampleData $ \_ ->
            T.elem Circle
              [ dataAttr "cx" (\d -> NumberValue (toNumber d * 50.0))
              , dataAttr "cy" (\_ -> NumberValue 100.0)
              , staticAttr "r" "20"
              , staticAttr "fill" "steelblue"
              ]
        )
    code <- liftEffect $ runMermaidTree tree
    H.modify_ _ { mermaidCode = Just code }
    -- Trigger Mermaid rendering after DOM update
    liftEffect runMermaid

-- Helper to create static attributes
staticAttr :: forall d. String -> String -> Attr.Attribute d
staticAttr name value = Attr.StaticAttr (AttributeName name) (StringValue value)

-- Helper to create data-driven attributes
dataAttr :: forall d. String -> (d -> AttributeValue) -> Attr.Attribute d
dataAttr name fn = Attr.DataAttr (AttributeName name) fn
