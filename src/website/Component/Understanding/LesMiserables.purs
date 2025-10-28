module PSD3.Understanding.LesMiserables where

import Prelude

import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.State (class MonadState, get)
import D3.Viz.LesMiserables as LesMis
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import PSD3.Internal.FFI (linksForceName_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3.Interpreter.D3 (runWithD3_Simulation)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.ExamplesNav as ExamplesNav
import PSD3.Website.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | State
type State = {
  simulation :: D3SimulationState_
}

-- | Actions
data Action = Initialize

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
  { initialState: \_ -> { simulation: initialSimulationState forceLibrary }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ -- TOC Panel (LHS)
      HH.div
        [ HP.classes [ HH.ClassName "toc-panel" ] ]
        [ HH.img
            [ HP.src "bookmark.jpeg"
            , HP.alt ""
            , HP.classes [ HH.ClassName "toc-panel__bookmark-pin" ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "toc-panel__main" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "floating-panel__header" ] ]
                [ HH.h3
                    [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                    [ HH.text "Contents" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "floating-panel__toggle" ]
                    , HP.type_ HP.ButtonButton
                    ]
                    [ HH.text "−" ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "floating-panel__content", HH.ClassName "toc-panel__content" ] ]
                [ HH.nav
                    [ HP.classes [ HH.ClassName "toc-nav" ] ]
                    [ HH.a [ HP.href "#example", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "1. Example" ]
                    , HH.a [ HP.href "#code", HP.classes [ HH.ClassName "toc-nav__item" ] ] [ HH.text "2. Code" ]
                    ]
                ]
            ]
        ]

    -- Navigation Panel (RHS)
    , HH.slot_ _examplesNav unit ExamplesNav.component LesMiserables

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Force-Directed Graph: Les Misérables Character Network" ]
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
            [ HH.text "1. Interactive Force Layout" ]
        , HH.p_
            [ HH.text "Drag nodes to see the force simulation respond. The simulation applies multiple forces: center (pulls toward middle), charge (nodes repel), collision (prevents overlap), and link (pulls connected nodes together)." ]
        , HH.div
            [ HP.classes [ HH.ClassName "viz-container" ] ]
            [ HH.div [ HP.classes [ HH.ClassName "lesmis-container" ] ] [] ]
        ]

    -- Code section
    , HH.section
        [ HP.id "code"
        , HP.classes [ HH.ClassName "tutorial-section" ]
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "section-title" ] ]
            [ HH.text "2. Implementation with Simplified SimulationM" ]
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
    -- Load data
    response <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    let graph = readGraphFromFileContents response

    -- Create force array and active forces set
    let forcesArray = [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]
        activeForces = Set.fromFoldable ["many body negative", "collision", "center", linksForceName_]

    -- Draw visualization
    runWithD3_Simulation do
      LesMis.drawSimplified forcesArray activeForces graph "div.lesmis-container"
