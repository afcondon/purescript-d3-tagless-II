-- | Force-Directed Visualization for Simpson's Paradox
-- |
-- | The star feature: animated cohorts showing the paradox.
-- | Each dot represents an applicant. Dots cluster by department
-- | when separated, or merge when combined.
-- |
-- | Animation: toggle between separated (6 department columns) and
-- | combined (single column) to reveal the paradox.
module D3.Viz.Simpsons.ForceViz
  ( initForceViz
  , ForceVizHandle
  ) where

import Prelude

import D3.Viz.Simpsons.ForceDirected (ForceConfig, createApplicants, departmentX, combinedX, genderY, defaultConfig)
import D3.Viz.Simpsons.Types (Gender(..), blue, red, green, purple, black)
import Data.Array (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (random)
import Effect.Ref as Ref
import PSD3.ForceEngine.Core as Core
import PSD3.ForceEngine.Simulation as Sim
import PSD3.ForceEngine.Simulation (SimulationNode)
import PSD3.ForceEngine.Types (ForceSpec(..), defaultCollide)
-- v3 Integration: all attributes via v3Attr/v3AttrStr (no ToAttr typeclass)
import PSD3v3.Integration (v3Attr, v3AttrStr, v3AttrFn, v3AttrFnStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3v2.VizTree.Tree as T
import Web.DOM.Element (Element)

-- =============================================================================
-- Types
-- =============================================================================

-- | Simulation node for an applicant
-- | Uses gridX/gridY for the optimized Grid forces
type ApplicantNode = SimulationNode
  ( gender :: Gender
  , department :: Int
  , accepted :: Boolean
  , gridX :: Number  -- Target X for ForceXGrid
  , gridY :: Number  -- Target Y for ForceYGrid
  )

-- | Handle returned from initialization
type ForceVizHandle =
  { toggle :: Effect Unit
  , stop :: Effect Unit
  , isCombined :: Effect Boolean
  }

-- =============================================================================
-- Initialization
-- =============================================================================

-- | Initialize the force visualization
-- | Returns a handle with toggle function and cleanup
initForceViz :: String -> Effect ForceVizHandle
initForceViz selector = do
  let config = defaultConfig

  -- Create applicant nodes with jitter
  nodes <- createApplicantNodes config

  -- State: is combined mode?
  isCombinedRef <- Ref.new false

  -- State ref for tick updates
  stateRef <- Ref.new { nodes, config }

  -- Create simulation
  sim <- Sim.create Sim.defaultConfig
  Sim.setNodes nodes sim

  -- Add forces using the Grid-optimized versions (read gridX/gridY from nodes)
  -- ForceXGrid reads node.gridX directly
  let forceXHandle = Core.createForceXGrid 0.15
  _ <- Core.initializeForce forceXHandle nodes
  Ref.modify_ (Map.insert "forceX" forceXHandle) sim.forces

  -- ForceYGrid reads node.gridY directly
  let forceYHandle = Core.createForceYGrid 0.15
  _ <- Core.initializeForce forceYHandle nodes
  Ref.modify_ (Map.insert "forceY" forceYHandle) sim.forces

  -- Collision prevents overlap (using standard collide)
  Sim.addForce (Collide "collide" defaultCollide { radius = config.nodeRadius + 0.5, strength = 0.7, iterations = 2 }) sim

  -- Render initial DOM structure using TreeAPI
  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)
    let containerTree :: T.Tree Unit
        containerTree =
          T.named SVG "svg"
            [ v3Attr "width" (lit config.width)
            , v3Attr "height" (lit config.height)
            , v3AttrStr "viewBox" (str ("0 0 " <> show config.width <> " " <> show config.height))
            , v3AttrStr "class" (str "force-viz-svg")
            ]
            `T.withChildren`
              [ T.elem Group []
                  `T.withChildren`
                    [ T.named Group "dept-labels" [ v3AttrStr "class" (str "dept-labels"), v3AttrStr "id" (str "force-viz-dept-labels") ]
                    , T.named Group "gender-labels" [ v3AttrStr "class" (str "gender-labels"), v3AttrStr "id" (str "force-viz-gender-labels") ]
                    , T.named Group "applicants" [ v3AttrStr "class" (str "applicants"), v3AttrStr "id" (str "force-viz-applicants") ]
                    ]
              ]
    _ <- renderTree container containerTree
    pure unit

  -- Render initial labels and nodes
  runD3v2M do
    deptLabelsParent <- select "#force-viz-dept-labels" :: _ (D3v2Selection_ SEmpty Element Unit)
    liftEffect $ renderDepartmentLabels config deptLabelsParent

    genderLabelsParent <- select "#force-viz-gender-labels" :: _ (D3v2Selection_ SEmpty Element Unit)
    liftEffect $ renderGenderLabels config genderLabelsParent

  -- Tick handler - update circle positions using TreeAPI
  Sim.onTick (tick stateRef) sim
  Sim.start sim

  -- Toggle function with multi-phase animation
  let
    -- Offsets for different phases
    smallOffset = 12.0    -- Small displacement when merged (keeps blue/red slightly apart)
    largeOffset = 60.0    -- Large separation for 4-group intermediate phase
    phaseDelay = 1200     -- Milliseconds between phases

    -- Phase 1: 4 groups (accepted/rejected separated by gender)
    -- Women: accepted at center-largeOffset, rejected at center+largeOffset
    -- Men: same pattern
    fourGroupsXFn :: ApplicantNode -> Number
    fourGroupsXFn node =
      let baseX = combinedX config
      in if node.accepted then baseX - largeOffset else baseX + largeOffset

    -- Phase 2: 2 groups (merged by gender, slight accepted/rejected displacement)
    twoGroupsXFn :: ApplicantNode -> Number
    twoGroupsXFn node =
      let baseX = combinedX config
      in if node.accepted then baseX - smallOffset else baseX + smallOffset

    -- Separated: 6 departments with accepted/rejected displacement
    separatedXFn :: ApplicantNode -> Number
    separatedXFn node =
      let baseX = departmentX config node.department
      in if node.accepted then baseX - smallOffset else baseX + smallOffset

    -- Use the library helper that handles force re-initialization
    updateX xFn = Sim.updateGridXYAndReinit (Just xFn) Nothing forceXHandle Nothing sim

    toggle = do
      isCombined <- Ref.read isCombinedRef
      let newMode = not isCombined
      Ref.write newMode isCombinedRef

      if newMode
        then do
          -- Combining: separated → 4 groups → 2 groups
          -- Phase 1: Move to 4 groups
          updateX fourGroupsXFn

          -- Phase 2: After delay, merge to 2 groups
          _ <- setTimeout_ (do
            -- Only proceed if still in combined mode
            stillCombined <- Ref.read isCombinedRef
            when stillCombined do
              updateX twoGroupsXFn
          ) phaseDelay
          pure unit

        else do
          -- Separating: 2 groups → 4 groups → separated
          -- Phase 1: Move to 4 groups
          updateX fourGroupsXFn

          -- Phase 2: After delay, spread to 6 departments
          _ <- setTimeout_ (do
            -- Only proceed if still in separated mode
            stillSeparated <- not <$> Ref.read isCombinedRef
            when stillSeparated do
              updateX separatedXFn
          ) phaseDelay
          pure unit

  pure
    { toggle
    , stop: Sim.stop sim
    , isCombined: Ref.read isCombinedRef
    }

-- | Tick handler - renders with current positions using TreeAPI
tick :: Ref.Ref { nodes :: Array ApplicantNode, config :: ForceConfig } -> Effect Unit
tick stateRef = runD3v2M do
  state <- liftEffect $ Ref.read stateRef

  -- Select applicants group
  nodesGroup <- select "#force-viz-applicants" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Render applicant circles
  let nodesTree = T.joinData "applicants" "circle" state.nodes $ \_ ->
        T.elem Circle
          [ v3AttrFn "cx" (_.x :: ApplicantNode -> Number)
          , v3AttrFn "cy" (_.y :: ApplicantNode -> Number)
          , v3Attr "r" (lit state.config.nodeRadius)
          , v3AttrFnStr "fill" nodeColor
          , v3AttrStr "stroke" (str "white")
          , v3Attr "stroke-width" (lit 0.5)
          ]
  _ <- renderTree nodesGroup nodesTree
  pure unit

  where
  nodeColor :: ApplicantNode -> String
  nodeColor node = if node.accepted then blue else red

-- | Create applicant nodes from data with jitter
createApplicantNodes :: ForceConfig -> Effect (Array ApplicantNode)
createApplicantNodes config = do
  let baseApplicants = createApplicants config
  let jitterRange = 40.0

  -- Add jitter and convert to simulation nodes
  traverse (\app -> do
    dx <- (\r -> (r - 0.5) * jitterRange) <$> random
    dy <- (\r -> (r - 0.5) * jitterRange) <$> random
    pure
      { id: app.id
      , x: app.x + dx
      , y: app.y + dy
      , vx: 0.0
      , vy: 0.0
      , fx: null
      , fy: null
      , gender: app.gender
      , department: app.department
      , accepted: app.accepted
      , gridX: app.targetX  -- Initial target = separated by department
      , gridY: app.targetY
      }
  ) baseApplicants

-- | Department label datum type
type DeptLabel = { idx :: Int, name :: String }

-- | Render department labels (A-F) using TreeAPI
renderDepartmentLabels :: ForceConfig -> D3v2Selection_ SEmpty Element Unit -> Effect Unit
renderDepartmentLabels config parent = runD3v2M do
  let
    deptNames = ["A", "B", "C", "D", "E", "F"]
    labelData = mapWithIndex (\i name -> { idx: i, name }) deptNames

  let labelsTree = T.joinData "dept-labels" "text" labelData $ \_ ->
        T.elem Text
          [ v3AttrFn "x" (\(d :: DeptLabel) -> departmentX config d.idx)
          , v3Attr "y" (lit (config.marginTop - 10.0))
          , v3AttrStr "text-anchor" (str "middle")
          , v3Attr "font-size" (lit 12.0)
          , v3AttrStr "fill" (str black)
          , v3AttrFnStr "textContent" (_.name :: DeptLabel -> String)
          ]
  _ <- renderTree parent labelsTree
  pure unit

-- | Gender label datum type
type GenderLabel = { gender :: Gender, label :: String }

-- | Render gender labels using TreeAPI
renderGenderLabels :: ForceConfig -> D3v2Selection_ SEmpty Element Unit -> Effect Unit
renderGenderLabels config parent = runD3v2M do
  let genders = [{ gender: Female, label: "Women" }, { gender: Male, label: "Men" }]

  let labelsTree = T.joinData "gender-labels" "text" genders $ \_ ->
        T.elem Text
          [ v3Attr "x" (lit 15.0)
          , v3AttrFn "y" (\(d :: GenderLabel) -> genderY config d.gender)
          , v3AttrStr "text-anchor" (str "start")
          , v3Attr "font-size" (lit 14.0)
          , v3AttrFnStr "fill" (\(d :: GenderLabel) -> if d.gender == Female then green else purple)
          , v3AttrFnStr "textContent" (_.label :: GenderLabel -> String)
          ]
  _ <- renderTree parent labelsTree
  pure unit

-- =============================================================================
-- FFI for setTimeout
-- =============================================================================

foreign import setTimeout_ :: Effect Unit -> Int -> Effect Int
