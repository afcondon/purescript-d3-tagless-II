module PSD3.Understanding.Movement where -- TODO add additional movement examples on this page

import Prelude

import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Rec.Class (forever)
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
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Shared.ZoomSticker as ZoomSticker
import PSD3.Website.Types (Route(..))

-- | State
type State = {
  simulation :: D3SimulationState_ Unit
, gupFiber :: Maybe (Fiber Unit)
, circlesSelection :: Maybe (D3Selection_ Int)
, radialTreeData :: Maybe TreeJson_
, radialTreeType :: TreeType
, radialRotation :: Number
, radialRotationGroup :: Maybe (D3Selection_ Unit)
, radialLinksGroup :: Maybe (D3Selection_ Unit)
, radialNodesGroup :: Maybe (D3Selection_ Unit)
, radialHierarchyRoot :: Maybe Datum_  -- The hierarchy root (reused for transitions)
}

-- | Actions
data Action =
    Initialize
  | Finalize
  | TriggerCirclesTransition
  | ToggleRadialTreeLayout
  | RotateRadialTree


-- | Forces configuration
forceLibrary :: Map String (Force Unit)
forceLibrary = initialize [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]

forces :: { center :: Force Unit, collision :: Force Unit, links :: Force Unit, manyBodyNeg :: Force Unit }
forces = {
    manyBodyNeg: createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-40.0) ]
  , collision:   createForce "collision"          (RegularForce ForceCollide)  allNodes [ F.radiusVal 4.0 ]
  , center:      createForce "center"             (RegularForce ForceCenter)   allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 1.0 ]
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

render :: State -> H.ComponentHTML Action () Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "example-page" ] ]
    [ -- Navigation Header
      TutorialNav.renderHeader Movement

    -- Page content
    , HH.main
        [ HP.classes [ HH.ClassName "tutorial-content" ] ]
        [ HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "5. Movement & Transition" ]
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
        , HH.p_
            [ HH.a
                [ HP.href "#/example/general-update-pattern"
                , HP.classes [ HH.ClassName "tutorial-link" ]
                ]
                [ HH.text "View interactive example with full source code →" ]
            ]
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
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ ZoomSticker.render
            , HH.div [ HP.classes [ HH.ClassName "lesmis-container" ] ] []
            ]
        , HH.p_
            [ HH.text "Drag nodes to see the force simulation respond. The simulation applies multiple forces: center (pulls toward middle), charge (nodes repel), collision (prevents overlap), and link (pulls connected nodes together)." ]
        , HH.p_
            [ HH.a
                [ HP.href "#/example/lesmis-force"
                , HP.classes [ HH.ClassName "tutorial-link" ]
                ]
                [ HH.text "View interactive example with full source code →" ]
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
    ]

handleAction :: forall o m.
  MonadAff m =>
  Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Initialize three circles transition example
    { circles } <- H.liftEffect $ eval_D3M $ CirclesTransition.drawThreeCirclesTransition "div.three-circles-viz"
    H.modify_ (\state -> state { circlesSelection = Just circles })

    -- Set up General Update Pattern animation
    updateFn <- runGeneralUpdatePattern
    fiber <- H.liftAff $ forkAff $ forever $ runUpdate updateFn
    H.modify_ (\state -> state { gupFiber = Just fiber })

    -- ARCHIVED: Force simulation disabled during phantom type integration
    -- response <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    -- let graph = readGraphFromFileContents response
    --
    -- -- Create force array and active forces set
    -- let forcesArray = [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]
    --     activeForces = Set.fromFoldable ["many body negative", "collision", "center", linksForceName_]
    --
    -- -- Draw visualization
    -- runWithD3_Simulation do
    --   LesMis.drawSimplified forcesArray activeForces graph "div.lesmis-container"

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
