module Component.AnimatedTreeCluster where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Interpreter.D3 (runD3M)
import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst)
import PSD3.Shared.FlareData (HierData)
import Data.Tree (Tree)
import PSD3.Internal.Types (D3Selection_)
import Unsafe.Coerce (unsafeCoerce)
import D3.Viz.AnimatedTree4Cluster4 as AnimatedTree
import D3.Viz.AnimatedTree4Cluster4 (LayoutType(..))

type VizState r =
  { dataTree :: Tree r
  , linksGroup :: D3Selection_ Unit
  , nodesGroup :: D3Selection_ Unit
  , chartWidth :: Number
  , chartHeight :: Number
  }

type State =
  { currentLayout :: LayoutType
  , vizState :: Maybe (VizState AnimatedTree.TreeModel)
  }

data Action
  = Initialize
  | ToggleLayout

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { currentLayout: TreeLayout
        , vizState: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "animated-tree-cluster-page" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "page-header" ] ]
        [ HH.h1_ [ HH.text "Animated Tree ↔ Cluster" ]
        , HH.p
            [ HP.classes [ HH.ClassName "page-description" ] ]
            [ HH.text "Smooth transitions between Tree (Reingold-Tilford) and Cluster (dendrogram) layouts using proper D3 data joins with stable keys." ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "controls" ] ]
        [ HH.button
            [ HP.classes [ HH.ClassName "button" ]
            , HE.onClick \_ -> ToggleLayout
            ]
            [ HH.text $ "Switch to " <> show (AnimatedTree.toggleLayout state.currentLayout) ]
        , HH.p
            [ HP.classes [ HH.ClassName "current-layout" ] ]
            [ HH.text $ "Current: " <> show state.currentLayout ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "viz-container" ]
        , HP.id "animated-tree-cluster"
        ]
        []
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "AnimatedTreeCluster: Initializing"
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)

    -- Load Flare data using AJAX.get to get JSON directly
    flareResult <- H.liftAff $ AJAX.get ResponseFormat.json "./data/flare-2.json"
    case flareResult of
      Left err -> log $ "Failed to load Flare data: " <> AJAX.printError err
      Right response -> do
        -- Extract JSON body (unsafeCoerce since we trust flare-2.json format)
        let flareData :: HierData
            flareData = unsafeCoerce response.body

        -- Initialize visualization
        Tuple vizState _ <- liftEffect $ runD3M $ AnimatedTree.draw flareData "#animated-tree-cluster"
        H.modify_ _ { vizState = Just vizState }

        -- Perform initial layout with Tree
        _ <- liftEffect $ runD3M $ AnimatedTree.animationStep
          vizState.dataTree
          vizState.linksGroup
          vizState.nodesGroup
          vizState.chartWidth
          vizState.chartHeight
          TreeLayout

        log "AnimatedTreeCluster: Initialized"

  ToggleLayout -> do
    state <- H.get
    let newLayout = AnimatedTree.toggleLayout state.currentLayout
    log $ "Toggling to " <> show newLayout
    H.modify_ _ { currentLayout = newLayout }

    case state.vizState of
      Nothing -> log "No viz state available"
      Just viz -> do
        _ <- liftEffect $ runD3M $ AnimatedTree.animationStep
          viz.dataTree
          viz.linksGroup
          viz.nodesGroup
          viz.chartWidth
          viz.chartHeight
          newLayout
        pure unit
