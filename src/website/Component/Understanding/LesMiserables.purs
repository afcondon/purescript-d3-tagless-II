module PSD3.Understanding.Movement where -- TODO add additional movement examples on this page

import Prelude

import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (class MonadState)
import D3.Viz.AnimatedRadialTree as AnimatedRadialTree
import D3.Viz.GUP as GUP
import D3.Viz.LesMiserables as LesMis
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import D3.Viz.ThreeLittleCirclesTransition as CirclesTransition
import Data.Either (Either(..))
import Data.Array (catMaybes)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)
import PSD3.Data.Tree (TreeJson_, TreeType(..))
import PSD3.Internal.FFI (linksForceName_)
import PSD3.Internal.Hierarchical (getTreeViaAJAX)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3.Internal.Types (D3Selection_, Datum_)
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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Shared.ExamplesNav as ExamplesNav
import PSD3.Shared.ZoomSticker as ZoomSticker
import PSD3.Website.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | State
type State = {
  simulation :: D3SimulationState_
, gupFiber :: Maybe (Fiber Unit)
, circlesSelection :: Maybe D3Selection_
, radialTreeData :: Maybe TreeJson_
, radialTreeType :: TreeType
, radialRotation :: Number
, radialRotationGroup :: Maybe D3Selection_
, radialLinksGroup :: Maybe D3Selection_
, radialNodesGroup :: Maybe D3Selection_
, radialHierarchyRoot :: Maybe Datum_  -- The hierarchy root (reused for transitions)
}

-- | Actions
data Action =
    Initialize
  | Finalize
  | TriggerCirclesTransition
  | ToggleRadialTreeLayout
  | RotateRadialTree

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
      , circlesSelection: Nothing
      , radialTreeData: Nothing
      , radialTreeType: TidyTree
      , radialRotation: 0.0
      , radialRotationGroup: Nothing
      , radialLinksGroup: Nothing
      , radialNodesGroup: Nothing
      , radialHierarchyRoot: Nothing
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
            [ tocAnchor "section-1" "1. Color Mixing" 0
            , tocAnchor "section-2" "2. General Update Pattern" 0
            , tocRoute (Explore "GUP") "→ How-to guide" 1
            , tocAnchor "section-3" "3. Force-Directed Graph" 0
            , tocAnchor "example" "3a. Interactive Layout" 1
            , tocAnchor "code" "3b. Implementation" 1
            , tocAnchor "section-4" "4. Animated Radial Tree" 0
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
            [ HH.text "This page explores transitions and movement in D3 visualizations, from simple color and position changes to complex physics simulations with the General Update Pattern." ]
        ]

    -- Section 1: Three Circles Color Mixing
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-1"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "1. Basic Transitions: Color Mixing" ]
        , HH.p_
            [ HH.text "Transitions allow smooth animated changes to visual properties. This simple example shows three circles transitioning from green to RGB primary colors (red, green, blue) with 50% opacity. The circles reposition to overlap, demonstrating additive color mixing where overlapping colors create secondary colors: cyan (green + blue), magenta (red + blue), and yellow (red + green)." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "three-circles-viz" ] ]
                []
            , HH.button
                [ HP.classes [ HH.ClassName "transition-button" ]
                , HE.onClick \_ -> TriggerCirclesTransition
                ]
                [ HH.text "Mix Colors" ]
            ]
        ]

    -- Section 2: General Update Pattern
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-2"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "2. The General Update Pattern" ]
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

    -- Section 3: Les Misérables Force Layout
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-3"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "3. Force-Directed Graph: Les Misérables" ]
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

    -- Section 4: Animated Radial Tree
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ]
        , HP.id "section-4"
        ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "4. Animated Radial Tree" ]
        , HH.p_
            [ HH.text "This demonstration shows smooth transitions between Tidy Tree and Dendrogram layouts in a radial arrangement. The same nodes smoothly animate to their new positions—no enter/exit, just updates. The rotate button helps with label readability." ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ ZoomSticker.render
            , HH.div [ HP.classes [ HH.ClassName "radial-tree-viz" ] ] []
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-button-group" ] ]
            [ HH.button
                [ HP.classes [ HH.ClassName "tutorial-button" ]
                , HE.onClick \_ -> ToggleRadialTreeLayout
                ]
                [ HH.text "Toggle Tidy/Dendrogram" ]
            , HH.button
                [ HP.classes [ HH.ClassName "tutorial-button" ]
                , HE.onClick \_ -> RotateRadialTree
                ]
                [ HH.text "Rotate 90°" ]
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

    -- Initialize three circles transition example
    { circles } <- H.liftEffect $ eval_D3M $ CirclesTransition.drawThreeCirclesTransition "div.three-circles-viz"
    H.modify_ (\state -> state { circlesSelection = Just circles })

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

    -- Load tree data for animated radial tree
    treeResponse <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"
    case treeResponse of
      Left _ -> pure unit  -- Failed to load, skip
      Right treeData -> do
        -- Initialize the animated radial tree
        { rotationGroup, linksGroup, nodesGroup, root } <-
          H.liftEffect $ eval_D3M $ AnimatedRadialTree.drawAnimatedRadialTree TidyTree treeData "div.radial-tree-viz"

        -- Store in state (including hierarchy root for later transitions)
        H.modify_ (\state -> state {
          radialTreeData = Just treeData,
          radialRotationGroup = Just rotationGroup,
          radialLinksGroup = Just linksGroup,
          radialNodesGroup = Just nodesGroup,
          radialHierarchyRoot = Just root
        })

  Finalize -> do
    -- Kill the GUP animation fiber
    maybeFiber <- H.gets _.gupFiber
    case maybeFiber of
      Nothing -> pure unit
      Just fiber -> H.liftAff $ killFiber (error "Cancelling GUP animation") fiber

  TriggerCirclesTransition -> do
    -- Run the color mixing transition
    maybeCircles <- H.gets _.circlesSelection
    case maybeCircles of
      Nothing -> pure unit
      Just circles -> H.liftEffect $ eval_D3M $ CirclesTransition.transitionToColorMixing circles

  ToggleRadialTreeLayout -> do
    -- Toggle between TidyTree and Dendrogram
    state <- H.get
    let newType = case state.radialTreeType of
          TidyTree -> Dendrogram
          Dendrogram -> TidyTree

    -- Update state
    H.modify_ _ { radialTreeType = newType }

    -- Update visualization if we have the hierarchy root and groups
    case state.radialHierarchyRoot, state.radialLinksGroup, state.radialNodesGroup of
      Just root, Just linksGroup, Just nodesGroup ->
        H.liftEffect $ eval_D3M $ AnimatedRadialTree.updateToLayout newType root linksGroup nodesGroup
      _, _, _ -> pure unit

  RotateRadialTree -> do
    -- Rotate the tree by 90 degrees
    state <- H.get
    let newRotation = state.radialRotation + 90.0

    -- Update state
    H.modify_ _ { radialRotation = newRotation }

    -- Apply rotation if we have the rotation group
    case state.radialRotationGroup of
      Nothing -> pure unit
      Just rotationGroup ->
        H.liftEffect $ eval_D3M $ AnimatedRadialTree.rotateTree state.radialRotation 90.0 rotationGroup

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
