-- | Force-Directed Department Visualization
-- |
-- | The star feature: animated cohorts showing Simpson's Paradox.
-- | Each dot represents an applicant. Dots cluster by department
-- | when separated, or by gender when combined.
-- |
-- | Based on: https://setosa.io/simpsons/
module D3.Viz.Simpsons.ForceDirected
  ( ForceConfig
  , defaultConfig
  , Applicant
  , createApplicants
  , departmentX
  , combinedX
  , genderY
  , applicantColor
  ) where

import Prelude

import D3.Viz.Simpsons.Types (blue, red, Gender(..), womenByDept, menByDept, DeptData)
import Data.Array (concat, mapWithIndex, replicate)
import Data.Int as Int

-- =============================================================================
-- Configuration
-- =============================================================================

type ForceConfig =
  { width :: Number
  , height :: Number
  , nodeRadius :: Number
  , marginLeft :: Number
  , marginRight :: Number
  , marginTop :: Number
  , marginBottom :: Number
  }

defaultConfig :: ForceConfig
defaultConfig =
  { width: 600.0
  , height: 400.0
  , nodeRadius: 2.5
  , marginLeft: 50.0
  , marginRight: 50.0
  , marginTop: 50.0
  , marginBottom: 50.0
  }

-- =============================================================================
-- Applicant Node Type
-- =============================================================================

-- | An applicant node for the force simulation
type Applicant =
  { id :: Int
  , gender :: Gender
  , department :: Int      -- 0-5 for departments A-F
  , accepted :: Boolean
  -- Position fields (will be filled by simulation)
  , x :: Number
  , y :: Number
  -- Target positions for ForceX/ForceY
  , targetX :: Number
  , targetY :: Number
  }

-- =============================================================================
-- Create Applicants from Berkeley Data
-- =============================================================================

-- | Create all applicants from the Berkeley data
-- | Note: Initial positions are set to target positions.
-- | Actual jittering should be done in Effect for proper randomness.
createApplicants :: ForceConfig -> Array Applicant
createApplicants config =
  let
    -- Offset to separate accepted from rejected within a cluster
    -- Accepted dots cluster slightly left, rejected slightly right
    acceptedOffset = -12.0

    -- Create applicants for one department
    createDeptApplicants :: Gender -> Int -> DeptData -> Array Applicant
    createDeptApplicants gender deptIdx deptData =
      let
        accepted = replicate deptData.accepted true
        rejected = replicate (deptData.applied - deptData.accepted) false
        allStatus = accepted <> rejected
        baseY = genderY config gender
        baseX = departmentX config deptIdx
      in
        map (\isAccepted ->
          let
            -- Offset X based on acceptance status to keep clusters separate
            targetX' = if isAccepted
              then baseX + acceptedOffset
              else baseX - acceptedOffset
          in
            { id: 0  -- Will be set later
            , gender
            , department: deptIdx
            , accepted: isAccepted
            , x: targetX'  -- Initial position = target
            , y: baseY
            , targetX: targetX'
            , targetY: baseY
            }) allStatus

    -- Create all women applicants
    womenApplicants = concat $ mapWithIndex (createDeptApplicants Female) womenByDept

    -- Create all men applicants
    menApplicants = concat $ mapWithIndex (createDeptApplicants Male) menByDept

    -- Combine and assign IDs
    allApplicants = womenApplicants <> menApplicants
  in
    mapWithIndex (\idx app -> app { id = idx }) allApplicants

-- =============================================================================
-- Position Calculations
-- =============================================================================

-- | X position for a department (0-5) in separated mode
departmentX :: ForceConfig -> Int -> Number
departmentX config deptIdx =
  let
    innerWidth = config.width - config.marginLeft - config.marginRight
    numDepts = 6
    spacing = innerWidth / Int.toNumber (numDepts + 1)
  in
    config.marginLeft + spacing * Int.toNumber (deptIdx + 1)

-- | X position for combined mode (all departments merged)
combinedX :: ForceConfig -> Number
combinedX config =
  config.marginLeft + (config.width - config.marginLeft - config.marginRight) / 2.0

-- | Y position for a gender row
genderY :: ForceConfig -> Gender -> Number
genderY config gender =
  let
    innerHeight = config.height - config.marginTop - config.marginBottom
  in
    case gender of
      Female -> config.marginTop + innerHeight * 0.25
      Male -> config.marginTop + innerHeight * 0.75

-- | Color for an applicant (based on accepted/rejected)
applicantColor :: Applicant -> String
applicantColor app =
  if app.accepted then blue else red
