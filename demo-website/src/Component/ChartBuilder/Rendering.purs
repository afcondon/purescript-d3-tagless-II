module Component.ChartBuilder.Rendering where

import Prelude

import Component.ChartBuilder.Types (Dataset)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import PSD3.AST (Tree)
import PSD3.AST as T
import PSD3.Expr.Friendly as F
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Internal.Selection.Types (SEmpty, ElementType(..))
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_, reselectD3v2)
import TreeBuilder3.Types (TreeNode)
import Web.DOM.Document as Document
import Web.DOM.Element (Element)
import Web.DOM.Node as Node
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

-- | Clear a container element
foreign import clearContainer :: String -> Effect Unit

-- | Render AST tree visualization with D3 (placeholder for v1)
renderAstVisualization :: forall state action o m. MonadAff m => H.HalogenM state action () o m Unit
renderAstVisualization = do
  liftEffect $ clearContainer "#ast-tree-preview"

  -- Similar to SimpleTreeBuilder: create a tree layout visualization
  let
    width = 600.0
    height = 300.0
    offsetX = 50.0
    offsetY = 50.0

  liftEffect $ runD3v2M do
    container <- select "#ast-tree-preview" :: _ (D3v2Selection_ SEmpty Element Unit)

    -- Create SVG with tree layout
    let
      mainTree :: Tree Unit
      mainTree =
        T.named SVG "ast-svg" [ F.width (F.num width), F.height (F.num height) ]
          `T.withChild`
          T.named Group "mainGroup"
            [ F.transform $ F.text $ "translate(" <> show offsetX <> "," <> show offsetY <> ")"
            ]
            `T.withChild`
            T.named Text "placeholder"
              [ F.x (F.num 0.0)
              , F.y (F.num 0.0)
              , F.fill $ F.text "#666"
              , F.fontSize (F.num 14.0)
              , F.textContent (F.text "AST visualization rendering...")
              ]

    _ <- renderTree container mainTree
    pure unit

-- | Render chart placeholder (actual chart rendering deferred to v2)
renderChartPlaceholder :: forall state action o m. MonadAff m => Dataset -> H.HalogenM state action () o m Unit
renderChartPlaceholder dataset = do
  liftEffect $ clearContainer "#chart-preview"

  liftEffect $ runD3v2M do
    container <- select "#chart-preview" :: _ (D3v2Selection_ SEmpty Element Unit)

    let
      placeholderTree :: Tree Unit
      placeholderTree =
        T.named SVG "chart-svg" [ F.width (F.num 600.0), F.height (F.num 400.0) ]
          `T.withChild`
          T.named Text "message"
            [ F.x (F.num 300.0)
            , F.y (F.num 200.0)
            , F.textAnchor $ F.text "middle"
            , F.fill $ F.text "#666"
            , F.fontSize (F.num 16.0)
            , F.textContent (F.text $ "Chart rendering with " <> dataset.name <> " - Coming in v2")
            ]

    _ <- renderTree container placeholderTree
    pure unit
