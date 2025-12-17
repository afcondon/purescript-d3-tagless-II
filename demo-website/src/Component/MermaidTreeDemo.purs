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
import PSD3.Interpreter.Mermaid (runMermaidTree)
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST as T
import PSD3.Expr.Integration (evalAttr, evalAttrStr, fnAttr, fnAttrStr)
import PSD3.Expr.Expr (lit, str)

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
      tree = T.named SVG "svg" [ evalAttrStr "width" (str "800"), evalAttrStr "height" (str "600") ] `T.withChildren`
        [ T.elem Group [ evalAttrStr "class" (str "container") ] `T.withChild`
            T.elem Circle [ evalAttrStr "r" (str "50"), evalAttrStr "fill" (str "blue") ]
        , T.elem Text [ evalAttrStr "x" (str "100"), evalAttrStr "y" (str "100") ]
        ]
    code <- liftEffect $ runMermaidTree tree
    H.modify_ _ { mermaidCode = Just code }
    -- Trigger Mermaid rendering after DOM update
    liftEffect runMermaid

  GenerateJoinTree -> do
    let
      sampleData = [ 1, 2, 3, 4, 5 ]
      tree = T.named SVG "svg" [ evalAttrStr "width" (str "800") ] `T.withChild`
        ( T.joinData "circles" "circle" sampleData $ \d ->
            T.elem Circle
              [ fnAttr "cx" (\datum -> toNumber datum * 50.0)
              , evalAttr "cy" (lit 100.0)
              , evalAttrStr "r" (str "20")
              , evalAttrStr "fill" (str "steelblue")
              ]
        )
    code <- liftEffect $ runMermaidTree tree
    H.modify_ _ { mermaidCode = Just code }
    -- Trigger Mermaid rendering after DOM update
    liftEffect runMermaid
