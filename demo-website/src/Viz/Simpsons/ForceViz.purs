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
import PSD3v2.Attribute.Types (cx, cy, fill, stroke, strokeWidth, radius, viewBox, width, height, class_, textAnchor, textContent, fontSize, x, y)
import PSD3v2.Capabilities.Selection (select, appendChild, appendData, setAttrs)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_, D3v2M)
import PSD3v2.Selection.Types (ElementType(..), SEmpty, SBoundOwns)
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

  -- Render DOM structure
  nodeSel <- runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

    -- Create SVG
    svg <- appendChild SVG
      [ width config.width
      , height config.height
      , viewBox ("0 0 " <> show config.width <> " " <> show config.height)
      , class_ "force-viz-svg"
      ] container

    -- Create main group
    g <- appendChild Group [] svg

    -- Add department labels group
    _ <- appendChild Group [ class_ "dept-labels" ] g
    deptLabelsParent <- select ".force-viz-svg .dept-labels" :: _ (D3v2Selection_ SEmpty Element Unit)
    renderDepartmentLabels config deptLabelsParent

    -- Add gender labels group
    _ <- appendChild Group [ class_ "gender-labels" ] g
    genderLabelsParent <- select ".force-viz-svg .gender-labels" :: _ (D3v2Selection_ SEmpty Element Unit)
    renderGenderLabels config genderLabelsParent

    -- Create nodes group
    _ <- appendChild Group [ class_ "applicants" ] g
    nodesGroup <- select ".force-viz-svg .applicants" :: _ (D3v2Selection_ SEmpty Element Unit)

    -- Get nodes from simulation for rendering
    currentNodes <- liftEffect $ Sim.getNodes sim

    -- Render applicant circles
    nodeSel <- appendData Circle currentNodes
      [ cx (_.x :: ApplicantNode -> Number)
      , cy (_.y :: ApplicantNode -> Number)
      , radius config.nodeRadius
      , fill nodeColor
      , stroke "white"
      , strokeWidth 0.5
      ] nodesGroup

    pure nodeSel

  -- Tick handler - update circle positions
  Sim.onTick (tick nodeSel) sim
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

  where
  -- Get fill color from node
  nodeColor :: ApplicantNode -> String
  nodeColor node = if node.accepted then blue else red

  -- Update tick handler
  tick :: D3v2Selection_ SBoundOwns Element ApplicantNode -> Effect Unit
  tick nodeSel = runD3v2M do
    _ <- setAttrs
      [ cx (_.x :: ApplicantNode -> Number)
      , cy (_.y :: ApplicantNode -> Number)
      ] nodeSel
    pure unit

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

-- | Render department labels (A-F)
renderDepartmentLabels :: ForceConfig -> D3v2Selection_ SEmpty Element Unit -> D3v2M Unit
renderDepartmentLabels config parent = do
  let
    deptNames = ["A", "B", "C", "D", "E", "F"]
    labelData = mapWithIndex (\i name -> { idx: i, name }) deptNames
  _ <- appendData Text labelData
    [ x (\(d :: DeptLabel) -> departmentX config d.idx)
    , y (config.marginTop - 10.0)
    , textAnchor "middle"
    , fontSize 12.0
    , fill black
    , textContent (_.name :: DeptLabel -> String)
    ] parent
  pure unit

-- | Gender label datum type
type GenderLabel = { gender :: Gender, label :: String }

-- | Render gender labels
renderGenderLabels :: ForceConfig -> D3v2Selection_ SEmpty Element Unit -> D3v2M Unit
renderGenderLabels config parent = do
  let genders = [{ gender: Female, label: "Women" }, { gender: Male, label: "Men" }]
  _ <- appendData Text genders
    [ x 15.0
    , y (\(d :: GenderLabel) -> genderY config d.gender)
    , textAnchor "start"
    , fontSize 14.0
    , fill (\(d :: GenderLabel) -> if d.gender == Female then green else purple)
    , textContent (_.label :: GenderLabel -> String)
    ] parent
  pure unit

-- =============================================================================
-- FFI for setTimeout
-- =============================================================================

foreign import setTimeout_ :: Effect Unit -> Int -> Effect Int
