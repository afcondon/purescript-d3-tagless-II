-- | Tests for Scene Rules System
-- |
-- | Tests the pure computation aspects of the rules system:
-- | - Rule building (rule, ruleAll)
-- | - Transform functions (pinAt, pinAtCurrent, unpin, setPosition)
-- | - Rule application (applyRules, applyFirstMatch)
-- | - First-match-wins semantics (CSS-like cascade)
module Test.Scene.RulesSpec where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')

-- Import the module under test
import PSD3.Scene.Rules as Rules
import PSD3.Scene.Types (NodeRule)

-- =============================================================================
-- Test Node Type
-- =============================================================================

-- | A simple node type for testing
-- | Has the fields required by the transform functions
type TestNode =
  { id :: String
  , group :: String
  , x :: Number
  , y :: Number
  , fx :: Nullable Number
  , fy :: Nullable Number
  }

-- | Create a test node
mkNode :: String -> String -> Number -> Number -> TestNode
mkNode id group x y =
  { id
  , group
  , x
  , y
  , fx: Nullable.null
  , fy: Nullable.null
  }

-- =============================================================================
-- Tests
-- =============================================================================

runTests :: Effect Unit
runTests = do
  log "\n--- Scene Rules Tests ---"

  testRuleBuilding
  testPinAtCurrent
  testPinAt
  testUnpin
  testSetPosition
  testApplyFirstMatch
  testApplyRules
  testRuleOrdering
  testNoMatchingRules

-- | Test: Rule building functions
testRuleBuilding :: Effect Unit
testRuleBuilding = do
  log "\n  Rule Building:"

  -- Build a rule with selector and transform
  let pinFixed :: NodeRule TestNode
      pinFixed = Rules.rule "pinFixed" (\n -> n.id == "fixed") (Rules.pinAt 100.0 200.0)

  -- Test that the rule has the correct name
  assert' "Rule should have name 'pinFixed'" (pinFixed.name == "pinFixed")
  log $ "    rule creates rule with name: " <> show pinFixed.name

  -- Test that selector works
  let fixedNode = mkNode "fixed" "A" 0.0 0.0
  let otherNode = mkNode "other" "A" 0.0 0.0
  assert' "Selector should match node with id 'fixed'" (pinFixed.select fixedNode)
  assert' "Selector should not match node with id 'other'" (not $ pinFixed.select otherNode)
  log "    rule selector works correctly"

  -- Build a ruleAll (matches everything)
  let pinAll :: NodeRule TestNode
      pinAll = Rules.ruleAll "pinAll" Rules.pinAtCurrent

  assert' "Rule should have name 'pinAll'" (pinAll.name == "pinAll")
  assert' "ruleAll should match any node" (pinAll.select fixedNode)
  assert' "ruleAll should match any node" (pinAll.select otherNode)
  log "    ruleAll matches all nodes"

  log "  ✓ Rule building works correctly"

-- | Test: pinAtCurrent transform
testPinAtCurrent :: Effect Unit
testPinAtCurrent = do
  log "\n  pinAtCurrent Transform:"

  let node = mkNode "test" "A" 50.0 75.0
  let pinned = Rules.pinAtCurrent node

  -- fx and fy should be set to current x and y
  case Nullable.toMaybe pinned.fx of
    Just fx -> do
      assert' ("fx should be 50.0, got " <> show fx) (fx == 50.0)
      log $ "    pinAtCurrent sets fx = x = " <> show fx
    Nothing ->
      assert' "fx should be set" false

  case Nullable.toMaybe pinned.fy of
    Just fy -> do
      assert' ("fy should be 75.0, got " <> show fy) (fy == 75.0)
      log $ "    pinAtCurrent sets fy = y = " <> show fy
    Nothing ->
      assert' "fy should be set" false

  -- x and y should remain unchanged
  assert' "x should remain 50.0" (pinned.x == 50.0)
  assert' "y should remain 75.0" (pinned.y == 75.0)

  log "  ✓ pinAtCurrent works correctly"

-- | Test: pinAt transform
testPinAt :: Effect Unit
testPinAt = do
  log "\n  pinAt Transform:"

  let node = mkNode "test" "A" 50.0 75.0
  let pinned = Rules.pinAt 100.0 200.0 node

  -- x and y should be updated to target
  assert' ("x should be 100.0, got " <> show pinned.x) (pinned.x == 100.0)
  assert' ("y should be 200.0, got " <> show pinned.y) (pinned.y == 200.0)
  log $ "    pinAt sets x = " <> show pinned.x <> ", y = " <> show pinned.y

  -- fx and fy should be set to target
  case Nullable.toMaybe pinned.fx of
    Just fx -> do
      assert' ("fx should be 100.0, got " <> show fx) (fx == 100.0)
      log $ "    pinAt sets fx = " <> show fx
    Nothing ->
      assert' "fx should be set" false

  case Nullable.toMaybe pinned.fy of
    Just fy -> do
      assert' ("fy should be 200.0, got " <> show fy) (fy == 200.0)
      log $ "    pinAt sets fy = " <> show fy
    Nothing ->
      assert' "fy should be set" false

  log "  ✓ pinAt works correctly"

-- | Test: unpin transform
testUnpin :: Effect Unit
testUnpin = do
  log "\n  unpin Transform:"

  -- Start with a pinned node
  let pinnedNode = (mkNode "test" "A" 50.0 75.0)
        { fx = Nullable.notNull 50.0
        , fy = Nullable.notNull 75.0
        }

  -- Verify it's pinned first
  assert' "Node should start pinned (fx not null)" (Nullable.toMaybe pinnedNode.fx /= Nothing)

  let unpinned = Rules.unpin pinnedNode

  -- fx and fy should be null after unpinning
  case Nullable.toMaybe unpinned.fx of
    Just _ -> assert' "fx should be null after unpin" false
    Nothing -> log "    unpin clears fx"

  case Nullable.toMaybe unpinned.fy of
    Just _ -> assert' "fy should be null after unpin" false
    Nothing -> log "    unpin clears fy"

  -- x and y should remain unchanged
  assert' "x should remain 50.0" (unpinned.x == 50.0)
  assert' "y should remain 75.0" (unpinned.y == 75.0)

  log "  ✓ unpin works correctly"

-- | Test: setPosition transform
testSetPosition :: Effect Unit
testSetPosition = do
  log "\n  setPosition Transform:"

  let node = mkNode "test" "A" 50.0 75.0
  let moved = Rules.setPosition 100.0 200.0 node

  -- x and y should be updated
  assert' ("x should be 100.0, got " <> show moved.x) (moved.x == 100.0)
  assert' ("y should be 200.0, got " <> show moved.y) (moved.y == 200.0)
  log $ "    setPosition sets x = " <> show moved.x <> ", y = " <> show moved.y

  -- fx and fy should remain unchanged (null)
  case Nullable.toMaybe moved.fx of
    Just _ -> assert' "fx should remain null" false
    Nothing -> log "    setPosition leaves fx unchanged"

  case Nullable.toMaybe moved.fy of
    Just _ -> assert' "fy should remain null" false
    Nothing -> log "    setPosition leaves fy unchanged"

  log "  ✓ setPosition works correctly"

-- | Test: applyFirstMatch
testApplyFirstMatch :: Effect Unit
testApplyFirstMatch = do
  log "\n  applyFirstMatch:"

  -- Define some rules
  let pinFixedRule :: NodeRule TestNode
      pinFixedRule = Rules.rule "pinFixed" (\n -> n.id == "fixed") (Rules.pinAt 100.0 200.0)

  let pinGroupARule :: NodeRule TestNode
      pinGroupARule = Rules.rule "pinGroupA" (\n -> n.group == "A") (Rules.setPosition 50.0 50.0)

  let unpinAllRule :: NodeRule TestNode
      unpinAllRule = Rules.ruleAll "unpinAll" Rules.unpin

  let rules = [pinFixedRule, pinGroupARule, unpinAllRule]

  -- Node with id "fixed" should match first rule
  let fixedNode = mkNode "fixed" "B" 0.0 0.0
  let fixedResult = Rules.applyFirstMatch rules fixedNode
  assert' ("Fixed node x should be 100.0, got " <> show fixedResult.x) (fixedResult.x == 100.0)
  log "    applyFirstMatch: 'fixed' node matches pinFixed rule"

  -- Node in group "A" (but not id "fixed") should match second rule
  let groupANode = mkNode "other" "A" 0.0 0.0
  let groupAResult = Rules.applyFirstMatch rules groupANode
  assert' ("Group A node x should be 50.0, got " <> show groupAResult.x) (groupAResult.x == 50.0)
  log "    applyFirstMatch: group 'A' node matches pinGroupA rule"

  -- Node that doesn't match first two rules should match ruleAll
  let otherNode = (mkNode "random" "B" 25.0 35.0)
        { fx = Nullable.notNull 25.0
        , fy = Nullable.notNull 35.0
        }
  let otherResult = Rules.applyFirstMatch rules otherNode
  case Nullable.toMaybe otherResult.fx of
    Just _ -> assert' "Other node fx should be null after unpinAll" false
    Nothing -> pure unit
  log "    applyFirstMatch: other node matches unpinAll rule"

  log "  ✓ applyFirstMatch works correctly"

-- | Test: applyRules (to array of nodes)
testApplyRules :: Effect Unit
testApplyRules = do
  log "\n  applyRules (array):"

  let pinFixedRule :: NodeRule TestNode
      pinFixedRule = Rules.rule "pinFixed" (\n -> n.id == "fixed") (Rules.pinAt 100.0 200.0)

  let setPositionForGroupA :: NodeRule TestNode
      setPositionForGroupA = Rules.rule "positionA" (\n -> n.group == "A") (Rules.setPosition 50.0 50.0)

  let rules = [pinFixedRule, setPositionForGroupA]

  let nodes =
        [ mkNode "fixed" "B" 0.0 0.0
        , mkNode "node1" "A" 10.0 10.0
        , mkNode "node2" "A" 20.0 20.0
        , mkNode "node3" "B" 30.0 30.0  -- No matching rule
        ]

  let results = Rules.applyRules rules nodes
  assert' "Should have 4 results" (Array.length results == 4)

  -- Check each result
  case Array.index results 0 of
    Just r -> assert' ("First node x should be 100.0, got " <> show r.x) (r.x == 100.0)
    Nothing -> assert' "Should have first result" false

  case Array.index results 1 of
    Just r -> assert' ("Second node x should be 50.0, got " <> show r.x) (r.x == 50.0)
    Nothing -> assert' "Should have second result" false

  case Array.index results 2 of
    Just r -> assert' ("Third node x should be 50.0, got " <> show r.x) (r.x == 50.0)
    Nothing -> assert' "Should have third result" false

  -- Node with no matching rule stays unchanged
  case Array.index results 3 of
    Just r -> assert' ("Fourth node x should be 30.0 (unchanged), got " <> show r.x) (r.x == 30.0)
    Nothing -> assert' "Should have fourth result" false

  log "    applyRules applies rules to array of nodes"
  log "    Nodes with no matching rule stay unchanged"

  log "  ✓ applyRules works correctly"

-- | Test: Rule ordering (first match wins)
testRuleOrdering :: Effect Unit
testRuleOrdering = do
  log "\n  Rule Ordering (first match wins):"

  -- Both rules match same node, but first should win
  let rule1 :: NodeRule TestNode
      rule1 = Rules.rule "setTo100" (\n -> n.group == "A") (Rules.setPosition 100.0 100.0)

  let rule2 :: NodeRule TestNode
      rule2 = Rules.rule "setTo200" (\n -> n.group == "A") (Rules.setPosition 200.0 200.0)

  let node = mkNode "test" "A" 0.0 0.0

  -- Order 1: rule1 first
  let result1 = Rules.applyFirstMatch [rule1, rule2] node
  assert' ("With rule1 first, x should be 100.0, got " <> show result1.x) (result1.x == 100.0)
  log "    [rule1, rule2] → first rule wins (x = 100.0)"

  -- Order 2: rule2 first
  let result2 = Rules.applyFirstMatch [rule2, rule1] node
  assert' ("With rule2 first, x should be 200.0, got " <> show result2.x) (result2.x == 200.0)
  log "    [rule2, rule1] → first rule wins (x = 200.0)"

  log "  ✓ First-match-wins ordering works correctly"

-- | Test: No matching rules leaves node unchanged
testNoMatchingRules :: Effect Unit
testNoMatchingRules = do
  log "\n  No Matching Rules:"

  let narrowRule :: NodeRule TestNode
      narrowRule = Rules.rule "narrowRule" (\n -> n.id == "specific") (Rules.setPosition 999.0 999.0)

  let node = mkNode "different" "A" 42.0 84.0
  let result = Rules.applyFirstMatch [narrowRule] node

  assert' ("x should be unchanged at 42.0, got " <> show result.x) (result.x == 42.0)
  assert' ("y should be unchanged at 84.0, got " <> show result.y) (result.y == 84.0)
  log "    Node with no matching rule stays unchanged"

  -- Empty rules array
  let emptyResult = Rules.applyFirstMatch [] node
  assert' "x should be unchanged with empty rules" (emptyResult.x == 42.0)
  log "    Empty rules array leaves node unchanged"

  log "  ✓ No matching rules handling works correctly"
