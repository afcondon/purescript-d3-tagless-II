module PSD3.Component.PSD3v2Examples where

import Prelude

import Data.Array (catMaybes)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (sequence, traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import D3.Viz.ThreeLittleCirclesV2 as ThreeLittleCirclesV2
import D3.Viz.ThreeLittleCirclesTransitionV2 as ThreeLittleCirclesTransitionV2
import D3.Viz.GUPV2 as GUPV2
import D3.Viz.TreeVizV2 as TreeVizV2
import D3.Viz.AnimatedTreeV2 as AnimatedTreeV2
import D3.Viz.AnimatedTreeV2 (LayoutType(..))
import D3.Viz.LesMisV2 as LesMisV2
import D3.Viz.LesMiserables.Model (LesMisSimNode)
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import PSD3v2.Interpreter.D3v2 as D3v2
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.FFI (linksForceName_)
import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.Set as Set
import Data.Map (Map)
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML.Events as HE

type State =
  { gupInitialized :: Boolean
  , treeLayout :: LayoutType
  , lesMisSimulation :: D3SimulationState_ LesMisSimNode
  }

data Action
  = Initialize
  | UpdateGUPRandom
  | ToggleTreeLayout

-- | Forces configuration for LesMis
forces :: { center :: Force LesMisSimNode, collision :: Force LesMisSimNode, links :: Force LesMisSimNode, manyBodyNeg :: Force LesMisSimNode }
forces = {
    manyBodyNeg: createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-100.0) ]
  , collision:   createForce "collision"          (RegularForce ForceCollide)  allNodes [ F.radiusVal 6.0 ]
  , center:      createForce "center"             (RegularForce ForceCenter)   allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
  , links:       createLinkForce Nothing []
}

forceLibrary :: Map String (Force LesMisSimNode)
forceLibrary = initialize [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { gupInitialized: false
        , treeLayout: TreeLayout
        , lesMisSimulation: initialSimulationState forceLibrary
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "psd3v2-examples-page" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "page-header" ] ]
        [ HH.h1_ [ HH.text "PSD3v2 Examples" ]
        , HH.p
            [ HP.classes [ HH.ClassName "page-description" ] ]
            [ HH.text "Type-safe D3 visualizations using phantom types and tagless final architecture" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "examples-grid" ] ]
        [ renderExample
            { id: "three-little-circles-v2"
            , title: "Three Little Circles"
            , description: "The classic D3 example reimplemented with PSD3v2"
            }
        , renderExample
            { id: "three-circles-transition-v2"
            , title: "Three Circles with Transitions"
            , description: "Animated transitions showing color mixing with elastic easing"
            }
        , renderGUPExample
        , renderExample
            { id: "tree-v2"
            , title: "Tree Layout"
            , description: "Reingold-Tilford tree layout algorithm (non-animating)"
            }
        , renderAnimatedTreeExample
        , renderExample
            { id: "lesmis-v2"
            , title: "Les Misérables Force-Directed Graph"
            , description: "Character network using PSD3v2 with force simulation. Integration of SelectionM + SimulationM capabilities."
            }
        ]
    ]

renderAnimatedTreeExample :: forall m. HH.HTML m Action
renderAnimatedTreeExample =
  HH.div
    [ HP.classes [ HH.ClassName "example-card" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "example-title" ] ]
        [ HH.text "Animated Tree / Cluster" ]
    , HH.p
        [ HP.classes [ HH.ClassName "example-description" ] ]
        [ HH.text "Toggle between Tree (Reingold-Tilford) and Cluster (Dendrogram) layouts. Click button to switch with animated transition." ]
    , HH.div
        [ HP.classes [ HH.ClassName "gup-controls" ] ]
        [ HH.button
            [ HP.classes [ HH.ClassName "gup-button" ]
            , HE.onClick \_ -> ToggleTreeLayout
            ]
            [ HH.text "Toggle Layout" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "example-viz-container" ]
        , HP.id "animated-tree-v2"
        ]
        []
    ]

renderGUPExample :: forall m. HH.HTML m Action
renderGUPExample =
  HH.div
    [ HP.classes [ HH.ClassName "example-card" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "example-title" ] ]
        [ HH.text "General Update Pattern (GUP)" ]
    , HH.p
        [ HP.classes [ HH.ClassName "example-description" ] ]
        [ HH.text "Enter-Update-Exit pattern with transitions. Click button to see random letters animate in/out." ]
    , HH.div
        [ HP.classes [ HH.ClassName "gup-controls" ] ]
        [ HH.button
            [ HP.classes [ HH.ClassName "gup-button" ]
            , HE.onClick \_ -> UpdateGUPRandom
            ]
            [ HH.text "Update with Random Letters" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "example-viz-container" ]
        , HP.id "gup-v2"
        ]
        []
    ]

renderExample :: forall w. { id :: String, title :: String, description :: String } -> HH.HTML w Action
renderExample { id, title, description } =
  HH.div
    [ HP.classes [ HH.ClassName "example-card" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "example-title" ] ]
        [ HH.text title ]
    , HH.p
        [ HP.classes [ HH.ClassName "example-description" ] ]
        [ HH.text description ]
    , HH.div
        [ HP.classes [ HH.ClassName "example-viz-container" ]
        , HP.id id
        ]
        []
    ]

-- | Generate a random subset of the alphabet (no duplicates, already sorted)
-- | Matches the classic D3 GUP demo behavior
generateRandomLetters :: forall m. MonadAff m => m String
generateRandomLetters = do
  let
    alphabet = toCharArray "abcdefghijklmnopqrstuvwxyz"

    -- Coin toss for each letter (60% probability of inclusion)
    coinToss :: Char -> m (Maybe Char)
    coinToss c = do
      n <- liftEffect random
      pure $ if n > 0.4 then Just c else Nothing

  -- Filter alphabet through coin toss
  choices <- traverse coinToss alphabet
  pure $ fromCharArray $ catMaybes choices

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "PSD3v2Examples: Initializing"
    -- Small delay to ensure DOM is ready
    H.liftAff $ delay (Milliseconds 100.0)

    -- Render Three Little Circles V2 (static)
    H.liftEffect $ D3v2.runD3v2M $ ThreeLittleCirclesV2.drawThreeCircles "#three-little-circles-v2"

    -- Render Three Circles with Transitions (animated)
    H.liftEffect $ D3v2.runD3v2M $ ThreeLittleCirclesTransitionV2.drawThreeCirclesTransition "#three-circles-transition-v2"

    -- Initialize GUP example with random letters
    randomText <- generateRandomLetters
    log $ "GUP: Initial text = " <> randomText
    H.liftEffect $ D3v2.runD3v2M do
      _ <- GUPV2.initializeGUP "#gup-v2"
      GUPV2.updateText "#letter-group" randomText

    -- Render Tree V2 (non-animating)
    H.liftEffect $ D3v2.runD3v2M $ TreeVizV2.drawTree "#tree-v2"

    -- Render Animated Tree V2 (initialize with tree layout)
    H.liftEffect $ D3v2.runD3v2M $ AnimatedTreeV2.initializeAnimatedTree "#animated-tree-v2"

    -- Load and render Les Misérables force-directed graph
    lesMisResponse <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    case lesMisResponse of
      Left _ -> log "Failed to load Les Misérables data"
      Right response -> do
        let graph = readGraphFromFileContents (Right response)
        let forcesArray = [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]
            activeForces = Set.fromFoldable ["many body negative", "collision", "center", linksForceName_]

        state <- H.get
        newState <- H.liftEffect $ D3v2.execD3v2SimM { simulation: state.lesMisSimulation } do
          LesMisV2.drawLesMisV2 forcesArray activeForces graph "#lesmis-v2"
        H.modify_ \s -> s { lesMisSimulation = newState.simulation }
        log "Les Misérables V2 initialized"

    H.modify_ _ { gupInitialized = true }

    pure unit

  UpdateGUPRandom -> do
    state <- H.get
    when state.gupInitialized do
      randomText <- generateRandomLetters
      log $ "GUP: New text = " <> randomText
      H.liftEffect $ D3v2.runD3v2M $ GUPV2.updateText "#letter-group" randomText

  ToggleTreeLayout -> do
    state <- H.get
    let newLayout = case state.treeLayout of
          TreeLayout -> ClusterLayout
          ClusterLayout -> TreeLayout
    log $ "Toggling tree layout to: " <> show newLayout
    H.modify_ _ { treeLayout = newLayout }
    H.liftEffect $ D3v2.runD3v2M $ AnimatedTreeV2.updateTreeLayout newLayout
