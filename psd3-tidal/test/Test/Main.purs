module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Tidal.AST.Pretty (pretty)
import Tidal.AST.Types (TPat)
import Tidal.Parse.Parser (parseTPat)

main :: Effect Unit
main = do
  log "=== psd3-tidal Parser Tests ==="
  log ""

  log "--- Basic Atoms ---"
  testParse "bd" "single atom"
  testParse "bd sn" "two atoms"
  testParse "bd sn hh" "three atoms"
  testParse "bd:2" "atom with variant"
  testParse "808.wav" "atom with dot"

  log ""
  log "--- Silence ---"
  testParse "bd ~ sn" "with silence"
  testParse "~ ~ ~" "all silence"

  log ""
  log "--- Speed Modifiers ---"
  testParse "bd*2" "fast"
  testParse "bd/2" "slow"
  testParse "bd*2 sn" "fast in sequence"
  testParse "[bd sn]*2" "fast group"

  log ""
  log "--- Degradation ---"
  testParse "bd?" "degrade default"
  testParse "bd?0.25" "degrade with prob"

  log ""
  log "--- Repetition ---"
  testParse "bd!" "repeat once (doubles)"
  testParse "bd!3" "repeat 3 times"

  log ""
  log "--- Elongation ---"
  testParse "bd@2" "elongate"
  testParse "bd_" "underscore elongate"

  log ""
  log "--- Grouping ---"
  testParse "[bd sn]" "simple group"
  testParse "[bd sn hh]" "group of 3"
  testParse "[[bd sn] hh]" "nested groups"

  log ""
  log "--- Stack ---"
  testParse "bd, sn" "stack of 2"
  testParse "bd sn, hh" "stack with sequence"

  log ""
  log "--- Polyrhythm ---"
  testParse "{bd sn, hh hh hh}" "polyrhythm"
  testParse "<bd sn hh>" "alternating"

  log ""
  log "--- Euclidean ---"
  testParse "bd(3,8)" "euclidean"
  testParse "bd(5,8,2)" "euclidean with offset"

  log ""
  log "--- Choose ---"
  testParse "bd | sn" "choose"
  testParse "bd | sn | hh" "choose 3"

  log ""
  log "--- Variables ---"
  testParse "^pattern" "variable"
  testParse "bd ^fill sn" "variable in sequence"

  log ""
  log "--- Round-trip Tests ---"
  testRoundTrip "bd sn hh"
  testRoundTrip "[bd sn]*2"
  testRoundTrip "bd, sn, hh"
  testRoundTrip "<bd sn hh>"
  testRoundTrip "{bd sn, hh hh hh}"
  testRoundTrip "bd(3,8)"

  log ""
  log "=== All tests completed ==="

testParse :: String -> String -> Effect Unit
testParse input desc = do
  let result = parseTPat input :: Either _ (TPat String)
  case result of
    Right _ -> log $ "  ✓ " <> desc <> ": \"" <> input <> "\""
    Left err -> log $ "  ✗ " <> desc <> ": \"" <> input <> "\" - " <> show err

testRoundTrip :: String -> Effect Unit
testRoundTrip input = do
  let result1 = parseTPat input :: Either _ (TPat String)
  case result1 of
    Left err -> log $ "  ✗ Round-trip \"" <> input <> "\" - parse failed: " <> show err
    Right ast1 -> do
      let printed = pretty ast1
      let result2 = parseTPat printed :: Either _ (TPat String)
      case result2 of
        Left err -> log $ "  ✗ Round-trip \"" <> input <> "\" -> \"" <> printed <> "\" - reparse failed: " <> show err
        Right _ ->
          log $ "  ✓ Round-trip: \"" <> input <> "\" -> \"" <> printed <> "\""
