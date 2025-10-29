module PSD3.Understanding.Movement where -- TODO add additional movement examples on this page

import Prelude

import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (class MonadState)
import D3.Viz.GUP as GUP
import D3.Viz.LesMiserables as LesMis
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import Data.Array (catMaybes)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import PSD3.Internal.FFI (linksForceName_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3.Interpreter.D3 (eval_D3M, runD3M, runWithD3_Simulation)
import CodeSnippet (codeSnippet, triggerPrismHighlighting)
import PSD3.Understanding.TOC (renderTOC, tocAnchor, tocRoute)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.ExamplesNav as ExamplesNav
import PSD3.Shared.ZoomSticker as ZoomSticker
import PSD3.Website.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | State
type State = {
  simulation :: D3SimulationState_
, gupFiber :: Maybe (Fiber Unit)
}

-- | Actions
data Action = Initialize | Finalize

-- | Child component slots
type Slots = ( examplesNav :: forall q. H.Slot q Void Unit )

_examplesNav = Proxy :: Proxy "examplesNav"

-- | Forces configuration
forceLibrary :: Map String Force
forceLibrary = initialize [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]

forces :: { center :: Force, collision :: Force, links :: Force, manyBodyNeg :: Force }
forces = {
    manyBodyNeg: createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strength (-40.0) ]
  , collision:   createForce "collision"          (RegularForce ForceCollide)  allNodes [ F.radius 4.0 ]
  , center:      createForce "center"             (RegularForce ForceCenter)   allNodes [ F.x 0.0, F.y 0.0, F.strength 1.0 ]
  , links:       createLinkForce Nothing []
}

-- | Component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { simulation: initialSimulationState forceLibrary
      , gupFiber: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , finalize = Just Finalize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ -- TOC Panel (LHS)
      renderTOC
        { title: "Page Contents"
        , items:
            [ tocAnchor "section-1" "1. General Update Pattern" 0
            , tocRoute (Explore "GUP") "→ How-to guide" 1
            , tocAnchor "section-2" "2. Force-Directed Graph" 0
            , tocAnchor "example" "2a. Interactive Layout" 1
            , tocAnchor "code" "2b. Implementation" 1
            ]
        , image: Just "images/understanding-bookmark-trees.jpeg"
        }

    -- Navigation Panel (RHS)
    , HH.slot_ _examplesNav unit ExamplesNav.component Movement

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Movement & Transition" ]
        , HH.p_
            [ HH.text "This page explores two key aspects of data visualization: the General Update Pattern for managing enter/update/exit transitions, and force-directed layouts that use physics simulation to position nodes and links." ]
        ]

    -- Section 1: General Update Pattern
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-1"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. The General Update Pattern" ]
        , HH.p_
            [ HH.text "This deceptively simple example shows off an aspect of screen-based data visualization that has no analogue in paper visualizations: the ability to specify how updates to the data should be represented." ]
        , HH.p_
            [ HH.text "In this example, some letters of the alphabet are presented and then constantly updated. When a letter enters at first, it falls in from the top and it is green. If it's still present in the next set of letters it stays on the screen, but it turns gray and moves to an alphabetically correct new position. And if it's not present in the new data, it turns red and falls out before disappearing." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "gup-viz" ] ]
                []
            ]
        -- SNIPPET: GUP src/website/Viz/GUP.purs 17-68
        , codeSnippet "GUP" "haskell"
        ]

    -- Section 2: Les Misérables Force Layout
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-2"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "2. Force-Directed Graph: Les Misérables" ]
        , HH.p_
            [ HH.text "Force-directed graphs use physics simulation to position nodes and links. Nodes repel each other like charged particles, while links act as springs pulling connected nodes together. The simulation finds an equilibrium that naturally reveals the structure of the network." ]
        , HH.p_
            [ HH.text "This example uses the simplified SimulationM API - a single "
            , HH.code_ [ HH.text "init" ]
            , HH.text " call with a configuration record, followed by "
            , HH.code_ [ HH.text "start" ]
            , HH.text ". The graph shows character co-occurrence in Victor Hugo's Les Misérables, where node size represents importance and link thickness shows the strength of connections."
            ]
        ]

    -- Visualization section
    , HH.section
        [ HP.id "example"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "section-title" ] ]
            [ HH.text "2a. Interactive Force Layout" ]
        , HH.p_
            [ HH.text "Drag nodes to see the force simulation respond. The simulation applies multiple forces: center (pulls toward middle), charge (nodes repel), collision (prevents overlap), and link (pulls connected nodes together)." ]
        , HH.div
            [ HP.classes [ HH.ClassName "viz-container" ] ]
            [ ZoomSticker.render
            , HH.div [ HP.classes [ HH.ClassName "lesmis-container" ] ] []
            ]
        ]

    -- Code section
    , HH.section
        [ HP.id "code"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "section-title" ] ]
            [ HH.text "2b. Implementation with Simplified SimulationM" ]
        , HH.p_
            [ HH.text "The new SimulationM API simplifies force layout creation. Instead of manually calling multiple setup functions, pass everything to "
            , HH.code_ [ HH.text "init" ]
            , HH.text " as a configuration record:"
            ]
        , HH.pre
            [ HP.classes [ HH.ClassName "code-block" ] ]
            [ HH.code_
                [ HH.text """drawSimplified forceLibrary activeForces model selector = do
  (Tuple w h) <- liftEffect getWindowWidthHeight
  svg <- attach selector >>= appendTo _ Svg [viewBox ...]

  -- Initialize simulation with config record
  { nodes: nodesInSim, links: linksInSim } <- init
    { nodes: model.nodes
    , links: model.links
    , forces: forceLibrary        -- All available forces
    , activeForces: activeForces  -- Which ones to enable
    , config: { alpha: 1.0, alphaTarget: 0.0, ... }
    , keyFn: keyIsID_
    , ticks: Map.fromFoldable []  -- Empty for now
    }

  -- Join simulation-enhanced data to DOM
  nodesSelection <- simpleJoin svg Circle nodesInSim keyIsID_
  linksSelection <- simpleJoin svg Line linksInSim keyIsID_

  -- Add tick functions to update positions
  addTickFunction "nodes" $ Step nodesSelection [cx datum_.x, cy datum_.y]
  addTickFunction "links" $ Step linksSelection [x1 link_.source.x, ...]

  -- Start the animation
  start""" ]
            ]
        , HH.p_
            [ HH.text "Key insight: "
            , HH.code_ [ HH.text "init" ]
            , HH.text " returns simulation-enhanced data (nodes with x, y, vx, vy properties). Join this enhanced data to DOM, not the raw input data. This ensures tick functions can access the simulation state."
            ]
        ]
    ]

handleAction :: forall m.
  MonadState State m =>
  MonadAff m =>
  Action -> m Unit
handleAction = case _ of
  Initialize -> do
    -- Trigger Prism highlighting for code snippets
    triggerPrismHighlighting

    -- Set up General Update Pattern animation
    updateFn <- runGeneralUpdatePattern
    fiber <- H.liftAff $ forkAff $ forever $ runUpdate updateFn
    H.modify_ (\state -> state { gupFiber = Just fiber })

    -- Load data
    response <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    let graph = readGraphFromFileContents response

    -- Create force array and active forces set
    let forcesArray = [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]
        activeForces = Set.fromFoldable ["many body negative", "collision", "center", linksForceName_]

    -- Draw visualization
    runWithD3_Simulation do
      LesMis.drawSimplified forcesArray activeForces graph "div.lesmis-container"

  Finalize -> do
    -- Kill the GUP animation fiber
    maybeFiber <- H.gets _.gupFiber
    case maybeFiber of
      Nothing -> pure unit
      Just fiber -> H.liftAff $ killFiber (error "Cancelling GUP animation") fiber

-- Helper functions for GUP animation
runGeneralUpdatePattern :: forall m. MonadEffect m => m (Array Char -> Aff Unit)
runGeneralUpdatePattern = do
  log "General Update Pattern example"
  update <- H.liftEffect $ eval_D3M $ GUP.exGeneralUpdatePattern "div.gup-viz"
  pure (\letters -> H.liftEffect $ runD3M (update letters) *> pure unit)

runUpdate :: (Array Char -> Aff Unit) -> Aff Unit
runUpdate update = do
  letters <- H.liftEffect $ getLetters
  update letters
  delay (Milliseconds 2300.0)
  where
    -- | choose a string of random letters (no duplicates), ordered alphabetically
    getLetters :: Effect (Array Char)
    getLetters = do
      let
        letters = toCharArray "abcdefghijklmnopqrstuvwxyz"
        coinToss :: Char -> Effect (Maybe Char)
        coinToss c = do
          n <- random
          pure $ if n > 0.6 then Just c else Nothing

      choices <- sequence $ coinToss <$> letters
      pure $ catMaybes choices
