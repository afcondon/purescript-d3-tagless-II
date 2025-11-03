module PSD3.Shared.MermaidAST
  ( component
  , Input
  , Slot
  , _mermaidAST
  , mkInput
  ) where

-- | Reusable component for rendering Mermaid AST diagrams from PSD3 visualizations
-- |
-- | This component takes any SelectionM computation (written using PSD3's DSL)
-- | and automatically generates a hand-drawn Mermaid flowchart showing its AST structure.
-- |
-- | Example usage:
-- |
-- | ```purescript
-- | import PSD3.Shared.MermaidAST as MermaidAST
-- |
-- | type Slots = (mermaidAST :: MermaidAST.Slot Unit, ...)
-- |
-- | render state =
-- |   HH.div_
-- |     [ HH.h2_ [ HH.text "Three Little Circles" ]
-- |     , HH.p_ [ HH.text "Creates three circles with data binding" ]
-- |     , HH.slot_ MermaidAST._mermaidAST unit MermaidAST.component
-- |         (MermaidAST.mkInput myVisualization)
-- |     ]
-- |
-- | where
-- |   myVisualization = do
-- |     svg <- D3.attach "svg"
-- |     circles <- D3.simpleJoin svg Circle [1, 2, 3] unsafeCoerce
-- |     D3.setAttributes circles [cx 100.0, cy 100.0, radius 20.0]
-- |     pure circles
-- | ```

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import PSD3.Interpreter.MermaidAST (MermaidASTM, runMermaidAST)
import PSD3.Data.Node (NodeID)
import PSD3.Shared.Mermaid (mermaidDiagram, triggerMermaidRendering)
import Type.Proxy (Proxy(..))

-- | Input to the MermaidAST component
-- | Takes any SelectionM computation and generates its AST diagram
type Input =
  { computation :: MermaidASTM NodeID
  , className :: Maybe String
  }

-- | Slot type for embedding MermaidAST component
type Slot id = forall q. H.Slot q Void id

-- | Proxy for the component slot
_mermaidAST :: Proxy "mermaidAST"
_mermaidAST = Proxy

-- | Helper to create Input from just a computation
mkInput :: MermaidASTM NodeID -> Input
mkInput computation = { computation, className: Nothing }

-- | Helper to create Input with a custom class
mkInputWithClass :: MermaidASTM NodeID -> String -> Input
mkInputWithClass computation className = { computation, className: Just className }

type State =
  { mermaidCode :: String
  , className :: Maybe String
  , computation :: MermaidASTM NodeID
  }

data Action = Initialize

component :: forall q o m. MonadAff m => H.Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: Input -> State
initialState input =
  { mermaidCode: "graph TD\n    Loading[\"Loading...\"]"
  , className: input.className
  , computation: input.computation
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  mermaidDiagram state.mermaidCode state.className

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    -- Run the MermaidAST computation to generate the diagram
    diagramCode <- liftEffect $ runMermaidAST state.computation
    H.modify_ _ { mermaidCode = diagramCode }
    -- Trigger Mermaid rendering after diagram is in DOM
    triggerMermaidRendering

