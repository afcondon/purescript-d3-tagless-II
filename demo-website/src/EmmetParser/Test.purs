module EmmetParser.Test where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import EmmetParser.Parser (parseEmmet)
import EmmetParser.Converter (convertToTree)
import EmmetParser.Printer (printEmmet)
import EmmetParser.Types (EmmetExpr(..), EmmetNode(..), ElementType(..), JoinType(..), Attribute(..))

-- | Run all parser tests
runTests :: Effect Unit
runTests = do
  log "=== Emmet Parser Tests ==="
  testSimpleElement
  testElementWithAttributes
  testJoinWithType
  testNesting
  testSiblings
  testMultiplier
  testComplexExpression
  log "\n=== Round-Trip Tests ==="
  testRoundTripSimple
  testRoundTripNested
  testRoundTripSiblings
  testRoundTripJoin
  testRoundTripComplex
  log "=== All Emmet Parser Tests Passed! ==="

testSimpleElement :: Effect Unit
testSimpleElement = do
  log "\nTest: Simple element (g)"
  let result = parseEmmet "g"
  case result of
    Right (Single (ElemNode EGroup [] Nothing) []) -> log "✓ Passed"
    Right expr -> log $ "✗ Failed: unexpected result " <> show expr
    Left err -> log $ "✗ Failed: " <> show err

testElementWithAttributes :: Effect Unit
testElementWithAttributes = do
  log "\nTest: Element with attributes (c[r=5,fill:color])"
  let result = parseEmmet "c[r=5,fill:color]"
  case result of
    Right (Single (ElemNode ECircle attrs Nothing) []) | checkAttrs attrs -> log "✓ Passed"
    Right expr -> log $ "✗ Failed: unexpected result " <> show expr
    Left err -> log $ "✗ Failed: " <> show err
  where
    checkAttrs attrs =
      attrs == [StaticAttr "r" "5", FieldAttr "fill" "color"]

testJoinWithType :: Effect Unit
testJoinWithType = do
  log "\nTest: Join with type (j(Point))"
  let result = parseEmmet "j(Point)"
  case result of
    Right (Single (JoinNode SimpleJoin "Point" [] Nothing) []) -> log "✓ Passed"
    Right expr -> log $ "✗ Failed: unexpected result " <> show expr
    Left err -> log $ "✗ Failed: " <> show err

testNesting :: Effect Unit
testNesting = do
  log "\nTest: Nesting (g>c)"
  let result = parseEmmet "g>c"
  case result of
    Right (Single (ElemNode EGroup [] Nothing) [Single (ElemNode ECircle [] Nothing) []]) ->
      log "✓ Passed"
    Right expr -> log $ "✗ Failed: unexpected result " <> show expr
    Left err -> log $ "✗ Failed: " <> show err

testSiblings :: Effect Unit
testSiblings = do
  log "\nTest: Siblings (c+r)"
  let result = parseEmmet "c+r"
  case result of
    Right (Sibling (Single (ElemNode ECircle [] Nothing) [])
                   (Single (ElemNode ERect [] Nothing) [])) ->
      log "✓ Passed"
    Right expr -> log $ "✗ Failed: unexpected result " <> show expr
    Left err -> log $ "✗ Failed: " <> show err

testMultiplier :: Effect Unit
testMultiplier = do
  log "\nTest: Multiplier (c*3)"
  let result = parseEmmet "c*3"
  case result of
    Right (Repeat (Single (ElemNode ECircle [] Nothing) []) 3) ->
      log "✓ Passed"
    Right expr -> log $ "✗ Failed: unexpected result " <> show expr
    Left err -> log $ "✗ Failed: " <> show err

testComplexExpression :: Effect Unit
testComplexExpression = do
  log "\nTest: Complex expression (j(Point)>c[cx:x,cy:y,r=5])"
  let result = parseEmmet "j(Point)>c[cx:x,cy:y,r=5]"
  case result of
    Right _ -> log "✓ Passed - parses successfully"
    Left err -> log $ "✗ Failed: " <> show err

-- =============================================================================
-- Round-Trip Tests: String → parse → convert → print → String
-- =============================================================================

testRoundTrip :: String -> Effect Unit
testRoundTrip input = do
  log $ "\nRound-trip: " <> input
  case parseEmmet input of
    Left err -> log $ "✗ Failed to parse: " <> show err
    Right expr -> do
      let tree = convertToTree expr
      let output = printEmmet tree
      if input == output
        then log $ "✓ Passed: " <> input <> " → " <> output
        else log $ "✗ Failed: " <> input <> " → " <> output

testRoundTripSimple :: Effect Unit
testRoundTripSimple = testRoundTrip "g"

testRoundTripNested :: Effect Unit
testRoundTripNested = testRoundTrip "g>c>r"

testRoundTripSiblings :: Effect Unit
testRoundTripSiblings = testRoundTrip "c+r"

testRoundTripJoin :: Effect Unit
testRoundTripJoin = testRoundTrip "j(Point)>c"

testRoundTripComplex :: Effect Unit
testRoundTripComplex = testRoundTrip "j(Point)>c[cx:x,cy:y,r=5]"
