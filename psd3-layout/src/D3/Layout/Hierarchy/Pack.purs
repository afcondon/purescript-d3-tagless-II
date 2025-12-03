module D3.Layout.Hierarchy.Pack
  ( CircleId
  , Circle
  , PackState
  , epsilon
  , epsilon6
  , enclosesWeak
  , intersects
  , place
  , packSiblingsMap
  , packEnclose
  , PackNode(..)
  , PackConfig
  , defaultPackConfig
  , HierarchyData(..)
  , hierarchy
  , pack
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, minimumBy)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt, abs)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console (log) as Console
import Data.Traversable (traverse_) as Array

-- | Circle ID for map-based storage
type CircleId = Int

-- | Circle type for geometric operations
type Circle =
  { x :: Number
  , y :: Number
  , r :: Number
  }

-- | State for map-based pack algorithm
type PackState =
  { circles :: Map CircleId Circle -- All circle data
  , frontChain :: Array CircleId -- Front-chain as circular array of IDs
  , nextId :: CircleId -- Next available ID
  }

-- ============================================================================
-- GEOMETRIC PRIMITIVES
-- ============================================================================

epsilon :: Number
epsilon = 1.0e-9

epsilon6 :: Number
epsilon6 = 1.0e-6

-- | Does circle a enclose circle b (with tolerance)?
enclosesWeak :: Circle -> Circle -> Boolean
enclosesWeak a b =
  let
    dr = a.r - b.r + (max a.r (max b.r 1.0)) * epsilon
    dx = b.x - a.x
    dy = b.y - a.y
  in
    dr > 0.0 && dr * dr > dx * dx + dy * dy

-- | Do circles a and b intersect?
intersects :: Circle -> Circle -> Boolean
intersects a b =
  let
    dr = a.r + b.r - epsilon6
    dx = b.x - a.x
    dy = b.y - a.y
  in
    dr > 0.0 && dr * dr > dx * dx + dy * dy

-- ============================================================================
-- PLACE FUNCTION (TANGENT CIRCLE POSITIONING)
-- ============================================================================

-- | Position circle c tangent to circles a and b
place :: Circle -> Circle -> Circle -> Circle
place b a c =
  let
    dx = b.x - a.x
    dy = b.y - a.y
    d2 = dx * dx + dy * dy
  in
    if d2 /= 0.0 then
      let
        a2 = (a.r + c.r) * (a.r + c.r)
        b2 = (b.r + c.r) * (b.r + c.r)
      in
        if a2 > b2 then
          -- Use b-relative positioning
          let
            x = (d2 + b2 - a2) / (2.0 * d2)
            y = sqrt (max 0.0 (b2 / d2 - x * x))
          in
            { x: b.x - x * dx - y * dy
            , y: b.y - x * dy + y * dx
            , r: c.r
            }
        else
          -- Use a-relative positioning
          let
            x = (d2 + a2 - b2) / (2.0 * d2)
            y = sqrt (max 0.0 (a2 / d2 - x * x))
          in
            { x: a.x + x * dx - y * dy
            , y: a.y + x * dy + y * dx
            , r: c.r
            }
    else
      -- a and b coincident
      { x: a.x + c.r
      , y: a.y
      , r: c.r
      }

-- ============================================================================
-- FRONT-CHAIN HELPERS
-- ============================================================================

-- | Calculate score for a pair of circles (weighted centroid distance from origin)
calculateScore :: Circle -> Circle -> Number
calculateScore a b =
  let
    ab = a.r + b.r
    dx = (a.x * b.r + b.x * a.r) / ab
    dy = (a.y * b.r + b.y * a.r) / ab
  in
    dx * dx + dy * dy

-- | Get next ID in circular chain
getNextId :: CircleId -> Array CircleId -> Maybe CircleId
getNextId nodeId chain =
  case Array.elemIndex nodeId chain of
    Nothing -> Nothing
    Just idx ->
      let
        nextIdx = (idx + 1) `mod` Array.length chain
      in
        Array.index chain nextIdx

-- | Score a node (based on pair of node and node.next)
scoreNode :: CircleId -> PackState -> Maybe { nodeId :: CircleId, score :: Number }
scoreNode nodeId state = do
  node <- Map.lookup nodeId state.circles
  nextId <- getNextId nodeId state.frontChain
  next <- Map.lookup nextId state.circles
  pure { nodeId, score: calculateScore node next }

-- | Find front pair with minimum score (D3's approach)
-- D3 scores each node (based on node + node.next pair), then returns (minNode, minNode.next)
findBestPair :: PackState -> Maybe (Tuple CircleId CircleId)
findBestPair state =
  let
    -- Score each node in the chain
    scored = Array.mapMaybe (\nodeId -> scoreNode nodeId state) state.frontChain

    -- Find node with minimum score
    bestNode = minimumBy (comparing _.score) scored
  in
    case bestNode of
      Nothing -> Nothing
      Just best -> do
        nextId <- getNextId best.nodeId state.frontChain
        pure (Tuple best.nodeId nextId)

-- | Insert a circle ID between two IDs in the chain
insertInChain :: CircleId -> CircleId -> CircleId -> Array CircleId -> Array CircleId
insertInChain aId _bId newId chain =
  case Array.elemIndex aId chain of
    Nothing -> chain
    Just aIdx ->
      -- Insert newId after aId
      fromMaybe chain $ Array.insertAt (aIdx + 1) newId chain

-- | Create new chain with shortcut (removes IDs between aId and jId)
-- | Shorten chain by removing circles between aId and jId (for retries)
-- | CRITICAL: In a circular chain, we remove everything BETWEEN a and j
-- | If j comes "after" a in circular order (jIdx > aIdx), remove elements between them
-- | If j comes "before" a in array (jIdx < aIdx), it's a wraparound - keep only j to a
shortenChainOnly :: CircleId -> CircleId -> Array CircleId -> Array CircleId
shortenChainOnly aId jId chain =
  case Array.elemIndex aId chain, Array.elemIndex jId chain of
    Just aIdx, Just jIdx ->
      if jIdx > aIdx then
        -- Normal case: j is after a in array, keep [0..a] ++ [j..end]
        let
          before = Array.take (aIdx + 1) chain -- Include aId
          after = Array.drop jIdx chain -- Include jId
        in
          before <> after
      else if jIdx < aIdx then
        -- Wraparound case: j is before a in array, keep only [j..a]
        Array.slice jIdx (aIdx + 1) chain
      else
        -- Same element, no change
        chain
    _, _ -> chain

-- | Shorten chain and insert new circle (for final insertion after collision)
-- shortenChain :: CircleId -> CircleId -> CircleId -> Array CircleId -> Array CircleId
-- shortenChain aId jId newId chain =
--   case Array.elemIndex aId chain, Array.elemIndex jId chain of
--     Just aIdx, Just jIdx ->
--       let
--         before = Array.take aIdx chain
--         after = Array.drop (jIdx + 1) chain
--       in
--         before <> [ aId, newId, jId ] <> after
--     _, _ -> chain

-- ============================================================================
-- COLLISION DETECTION (D3-style bidirectional search)
-- ============================================================================

-- | Result of bidirectional collision search
-- | Includes which direction the collision was found (affects how we update a/b)
data CollisionResult
  = NoCollision
  | CollisionFromJ CircleId  -- Collision found walking forward (b becomes j)
  | CollisionFromK CircleId  -- Collision found walking backward (a becomes k)

-- | Get next ID in circular chain (wrapping)
getNextInChain :: CircleId -> Array CircleId -> Maybe CircleId
getNextInChain nodeId chain =
  case Array.elemIndex nodeId chain of
    Nothing -> Nothing
    Just idx ->
      let nextIdx = (idx + 1) `mod` Array.length chain
      in Array.index chain nextIdx

-- | Get previous ID in circular chain (wrapping)
getPrevInChain :: CircleId -> Array CircleId -> Maybe CircleId
getPrevInChain nodeId chain =
  case Array.elemIndex nodeId chain of
    Nothing -> Nothing
    Just idx ->
      let n = Array.length chain
          prevIdx = (idx - 1 + n) `mod` n
      in Array.index chain prevIdx

-- | D3-style bidirectional collision search
-- | Walks both directions (j forward from b, k backward from a) simultaneously
-- | Uses accumulated radii to decide which direction to check next
-- | Returns collision info including direction (so caller knows to update a or b)
-- | IMPORTANT: Uses do-while semantics - check at least one circle before stopping
bidirectionalCollisionSearch :: CircleId -> CircleId -> Circle -> PackState -> CollisionResult
bidirectionalCollisionSearch aId bId newCircle state =
  case Map.lookup aId state.circles, Map.lookup bId state.circles of
    Just a, Just b ->
      -- Initialize: j = b.next, k = a.previous
      case getNextInChain bId state.frontChain, getPrevInChain aId state.frontChain of
        Just jInit, Just kInit ->
          -- Initial accumulated radii: sj = b.r, sk = a.r
          -- D3 uses do-while, so we run the body FIRST, then check stop condition
          doWhileStep jInit kInit b.r a.r
        _, _ -> NoCollision
    _, _ -> NoCollision
  where
  -- D3's do-while loop: run body first, then check if j === k.next
  doWhileStep :: CircleId -> CircleId -> Number -> Number -> CollisionResult
  doWhileStep j k sj sk =
    -- Run loop body first (check collision)
    if sj <= sk then
      -- Check collision at j
      case Map.lookup j state.circles of
        Just jCircle ->
          if intersects jCircle newCircle then
            CollisionFromJ j
          else
            -- Advance j forward, add j.r to sj
            case getNextInChain j state.frontChain of
              Just jNext ->
                -- NOW check stop condition: j (after advancing) === k.next?
                case getNextInChain k state.frontChain of
                  Just kNext | jNext == kNext -> NoCollision  -- Pointers met, done
                  _ -> doWhileStep jNext k (sj + jCircle.r) sk  -- Continue
              Nothing -> NoCollision
        Nothing -> NoCollision
    else
      -- Check collision at k
      case Map.lookup k state.circles of
        Just kCircle ->
          if intersects kCircle newCircle then
            CollisionFromK k
          else
            -- Advance k backward, add k.r to sk
            case getPrevInChain k state.frontChain of
              Just kPrev ->
                -- NOW check stop condition: j === kPrev.next?
                case getNextInChain kPrev state.frontChain of
                  Just kPrevNext | j == kPrevNext -> NoCollision  -- Pointers met, done
                  _ -> doWhileStep j kPrev sj (sk + kCircle.r)  -- Continue
              Nothing -> NoCollision
        Nothing -> NoCollision

-- ============================================================================
-- STATE MANIPULATION
-- ============================================================================

-- | Insert circle into state between aId and bId (no collision)
insertBetween :: CircleId -> CircleId -> Circle -> PackState -> PackState
insertBetween aId bId newCircle state =
  let
    newId = state.nextId
    newCircles = Map.insert newId newCircle state.circles
    newChain = insertInChain aId bId newId state.frontChain
  in
    state
      { circles = newCircles
      , frontChain = newChain
      , nextId = newId + 1
      }

-- | Insert circle with shortcut (collision found at jId)
-- insertWithShortcut :: CircleId -> CircleId -> Circle -> PackState -> PackState -- TODO not used
-- insertWithShortcut aId jId newCircle state =
--   let
--     newId = state.nextId
--     newCircles = Map.insert newId newCircle state.circles
--     newChain = shortenChain aId jId newId state.frontChain
--   in
--     state
--       { circles = newCircles
--       , frontChain = newChain
--       , nextId = newId + 1
--       }

-- ============================================================================
-- MAIN ALGORITHM
-- ============================================================================

-- | Initialize state with first 3 circles
initState :: Circle -> Circle -> Circle -> PackState
initState c0 c1 c2 =
  let
    -- Place first two circles (D3 moves first circle to x = -b.r)
    c0' = c0 { x = -c1.r, y = 0.0 }
    c1' = c1 { x = c0.r, y = 0.0 }

    -- Place third circle tangent to first two
    c2' = place c1' c0' c2

    circles = Map.fromFoldable
      [ Tuple 0 c0'
      , Tuple 1 c1'
      , Tuple 2 c2'
      ]

    _ = unsafePerformEffect $ Console.log $
      "   Initial positions: c0=(" <> show c0'.x <> "," <> show c0'.y <> ") c1=(" <> show c1'.x <> "," <> show c1'.y <> ") c2=(" <> show c2'.x <> "," <> show c2'.y <> ")"
  in
    { circles
    , frontChain: [ 0, 2, 1 ] -- D3 order: a -> c -> b -> a (counterclockwise around boundary)
    , nextId: 3
    }

-- | Add one circle to the pack
-- | Try to place a circle, retrying with new front pairs until no collision
-- | maxRetries parameter prevents infinite recursion
-- | D3's algorithm: on collision from j-direction, b=j; on collision from k-direction, a=k
tryPlaceCircleWithLimit :: Int -> Circle -> CircleId -> CircleId -> PackState -> PackState
tryPlaceCircleWithLimit maxRetries newCircle aId bId state
  | maxRetries <= 0 =
      -- CRITICAL: We've exhausted retries! This means our retry logic is broken.
      let
        _ = unsafePerformEffect $ Console.log $ "❌ RETRY LIMIT HIT! Failed to place circle r=" <> show newCircle.r
      in
        insertBetween aId bId newCircle state -- Insert anyway to avoid dropping it
  | otherwise =
      case Map.lookup aId state.circles, Map.lookup bId state.circles of
        Just a, Just b ->
          let
            -- Place circle tangent to pair
            positioned = place b a newCircle

            -- Log placement attempt (reduced verbosity)
            _ = unsafePerformEffect $ Console.log $
              "  Try r=" <> show newCircle.r <> " at (" <> show aId <> "," <> show bId <> ")"

            -- D3-style bidirectional collision search
            collisionResult = bidirectionalCollisionSearch aId bId positioned state
          in
            case collisionResult of
              NoCollision ->
                let
                  _ = unsafePerformEffect $ Console.log $ "    ✅ Placed at (" <> show positioned.x <> ", " <> show positioned.y <> ")"
                in
                  insertBetween aId bId positioned state

              CollisionFromJ jId ->
                -- Collision walking forward: b = j, retry with (a, j)
                -- D3: a.next = b, b.previous = a (splice out nodes between old b and j)
                let
                  _ = unsafePerformEffect $ Console.log $
                    "    ⚠️ Collision at j=" <> show jId <> " - b:=" <> show jId
                  -- Shorten chain: keep a, skip to j
                  shortenedChain = shortenChainOnly aId jId state.frontChain
                  stateWithShortenedChain = state { frontChain = shortenedChain }
                in
                  tryPlaceCircleWithLimit (maxRetries - 1) newCircle aId jId stateWithShortenedChain

              CollisionFromK kId ->
                -- Collision walking backward: a = k, retry with (k, b)
                -- D3: a.next = b, b.previous = a (splice out nodes between k and old a)
                let
                  _ = unsafePerformEffect $ Console.log $
                    "    ⚠️ Collision at k=" <> show kId <> " - a:=" <> show kId
                  -- Shorten chain: skip from k to b
                  shortenedChain = shortenChainOnly kId bId state.frontChain
                  stateWithShortenedChain = state { frontChain = shortenedChain }
                in
                  tryPlaceCircleWithLimit (maxRetries - 1) newCircle kId bId stateWithShortenedChain
        _, _ -> state

tryPlaceCircle :: Circle -> CircleId -> CircleId -> PackState -> PackState
tryPlaceCircle = tryPlaceCircleWithLimit 100 -- Allow up to 100 retries

addCircle :: Circle -> PackState -> PackState
addCircle newCircle state =
  let
    _ = unsafePerformEffect $ Console.log $
      "\n➤ Adding circle r=" <> show newCircle.r
        <> " (total circles so far: "
        <> show (Map.size state.circles)
        <> ")"
    _ = unsafePerformEffect $ Console.log $
      "  Current front chain: " <> show state.frontChain
  in
    case findBestPair state of
      Nothing -> state -- No pairs available (shouldn't happen)
      Just (Tuple aId bId) ->
        let
          _ = unsafePerformEffect $ Console.log $
            "  Best pair: (" <> show aId <> "," <> show bId <> ")"
          result = tryPlaceCircle newCircle aId bId state
        in
          result

-- | Pack siblings using map-based front-chain algorithm
packSiblingsMap :: Array Circle -> { circles :: Array Circle, radius :: Number }
packSiblingsMap inputCircles =
  let
    n = Array.length inputCircles
    _ = unsafePerformEffect $ Console.log $
      "📦 packSiblingsMap called with " <> show n <> " circles"
    _ = unsafePerformEffect $ Console.log $
      "   Radii: " <> show (map _.r inputCircles)
  in
    if n == 0 then { circles: [], radius: 0.0 }
    else if n == 1 then
      case Array.index inputCircles 0 of
        Just c -> { circles: [ c { x = 0.0, y = 0.0 } ], radius: c.r }
        Nothing -> { circles: [], radius: 0.0 }
    else if n == 2 then
      case Array.index inputCircles 0, Array.index inputCircles 1 of
        Just c0, Just c1 ->
          let
            c0' = c0 { x = -c1.r, y = 0.0 }
            c1' = c1 { x = c0.r, y = 0.0 }
          in
            { circles: [ c0', c1' ], radius: c0.r + c1.r }
        _, _ -> { circles: [], radius: 0.0 }
    else
      -- Three or more circles - use front-chain algorithm
      case Array.index inputCircles 0, Array.index inputCircles 1, Array.index inputCircles 2 of
        Just c0, Just c1, Just c2 ->
          let
            -- Initialize with first 3 circles
            initialState = initState c0 c1 c2

            -- Add remaining circles one by one
            remaining = Array.drop 3 inputCircles
            finalState = foldl (flip addCircle) initialState remaining

            -- Extract circles from map in original order
            -- Note: We extract ALL circles (0 to n-1), even those not in the final chain,
            -- because circles removed from the chain during shortening are still part of the pack
            positioned = Array.mapMaybe (\i -> Map.lookup i finalState.circles)
              (Array.range 0 (n - 1))

            -- Compute enclosing circle
            enclosing = packEnclose positioned

            -- Translate to center
            translated = map (\c -> c { x = c.x - enclosing.x, y = c.y - enclosing.y }) positioned

            -- Detect and report overlaps
            overlaps = detectOverlaps translated
            _ = if Array.length overlaps > 0
                then unsafePerformEffect $ Console.log $
                  "⚠️ OVERLAPS DETECTED: " <> show (Array.length overlaps) <> " pairs"
                else unit
            _ = unsafePerformEffect $ reportOverlaps overlaps
          in
            { circles: translated, radius: enclosing.r }
        _, _, _ -> { circles: [], radius: 0.0 }

-- | Detect all pairwise overlaps in final positioned circles
detectOverlaps :: Array Circle -> Array { i :: Int, j :: Int, overlap :: Number }
detectOverlaps circles =
  let
    n = Array.length circles
    pairs = do
      i <- Array.range 0 (n - 2)
      j <- Array.range (i + 1) (n - 1)
      pure (Tuple i j)
  in
    Array.mapMaybe (checkPair circles) pairs
  where
  checkPair :: Array Circle -> Tuple Int Int -> Maybe { i :: Int, j :: Int, overlap :: Number }
  checkPair cs (Tuple i j) = do
    ci <- Array.index cs i
    cj <- Array.index cs j
    let dx = cj.x - ci.x
        dy = cj.y - ci.y
        dist = sqrt (dx * dx + dy * dy)
        minDist = ci.r + cj.r
        overlap = minDist - dist
    -- Report if overlap > small tolerance (not just touching)
    if overlap > 0.1
      then Just { i, j, overlap }
      else Nothing

-- | Report overlaps to console
reportOverlaps :: Array { i :: Int, j :: Int, overlap :: Number } -> Effect Unit
reportOverlaps overlaps =
  Array.traverse_ (\o ->
    Console.log $ "  Overlap: circles " <> show o.i <> " and " <> show o.j
      <> " overlap by " <> show o.overlap <> " pixels"
  ) overlaps

-- ============================================================================
-- ENCLOSING CIRCLE (from Pack.purs)
-- ============================================================================

enclosesNot :: Circle -> Circle -> Boolean
enclosesNot a b =
  let
    dr = a.r - b.r
    dx = b.x - a.x
    dy = b.y - a.y
  in
    dr < 0.0 || dr * dr < dx * dx + dy * dy

enclosesWeakAll :: Circle -> Array Circle -> Boolean
enclosesWeakAll a bs = Array.all (enclosesWeak a) bs

encloseBasis1 :: Circle -> Circle
encloseBasis1 a = { x: a.x, y: a.y, r: a.r }

encloseBasis2 :: Circle -> Circle -> Circle
encloseBasis2 a b =
  let
    x1 = a.x
    y1 = a.y
    r1 = a.r
    x2 = b.x
    y2 = b.y
    r2 = b.r
    x21 = x2 - x1
    y21 = y2 - y1
    r21 = r2 - r1
    l = sqrt (x21 * x21 + y21 * y21)
  in
    { x: (x1 + x2 + x21 / l * r21) / 2.0
    , y: (y1 + y2 + y21 / l * r21) / 2.0
    , r: (l + r1 + r2) / 2.0
    }

encloseBasis3 :: Circle -> Circle -> Circle -> Circle
encloseBasis3 a b c =
  let
    x1 = a.x
    y1 = a.y
    r1 = a.r
    x2 = b.x
    y2 = b.y
    r2 = b.r
    x3 = c.x
    y3 = c.y
    r3 = c.r
    a2 = x1 - x2
    a3 = x1 - x3
    b2 = y1 - y2
    b3 = y1 - y3
    c2 = r2 - r1
    c3 = r3 - r1
    d1 = x1 * x1 + y1 * y1 - r1 * r1
    d2 = d1 - x2 * x2 - y2 * y2 + r2 * r2
    d3 = d1 - x3 * x3 - y3 * y3 + r3 * r3
    ab = a3 * b2 - a2 * b3
    xa = (b2 * d3 - b3 * d2) / (ab * 2.0) - x1
    xb = (b3 * c2 - b2 * c3) / ab
    ya = (a3 * d2 - a2 * d3) / (ab * 2.0) - y1
    yb = (a2 * c3 - a3 * c2) / ab
    bigA = xb * xb + yb * yb - 1.0
    bigB = 2.0 * (r1 + xa * xb + ya * yb)
    bigC = xa * xa + ya * ya - r1 * r1

    r =
      if abs bigA > epsilon6 then -(bigB + sqrt (bigB * bigB - 4.0 * bigA * bigC)) / (2.0 * bigA)
      else -bigC / bigB
  in
    { x: x1 + xa + xb * r
    , y: y1 + ya + yb * r
    , r: r
    }

encloseBasis :: Array Circle -> Circle
encloseBasis basis =
  case Array.length basis of
    1 -> fromMaybe { x: 0.0, y: 0.0, r: 0.0 } $ encloseBasis1 <$> Array.index basis 0
    2 -> fromMaybe { x: 0.0, y: 0.0, r: 0.0 } $
      encloseBasis2 <$> Array.index basis 0 <*> Array.index basis 1
    3 -> fromMaybe { x: 0.0, y: 0.0, r: 0.0 } $
      encloseBasis3 <$> Array.index basis 0 <*> Array.index basis 1 <*> Array.index basis 2
    _ -> { x: 0.0, y: 0.0, r: 0.0 }

extendBasis :: Array Circle -> Circle -> Array Circle
extendBasis basis p
  | enclosesWeakAll p basis = [ p ]
  | otherwise =
      case findTwoCircleBasis 0 of
        Just result -> result
        Nothing ->
          case findThreeCircleBasis 0 of
            Just result -> result
            Nothing -> basis
      where
      n = Array.length basis

      findTwoCircleBasis :: Int -> Maybe (Array Circle)
      findTwoCircleBasis i
        | i >= n = Nothing
        | otherwise =
            case Array.index basis i of
              Just bi ->
                let
                  candidate = encloseBasis2 bi p
                in
                  if enclosesNot p bi && enclosesWeakAll candidate basis then Just [ bi, p ]
                  else findTwoCircleBasis (i + 1)
              Nothing -> findTwoCircleBasis (i + 1)

      findThreeCircleBasis :: Int -> Maybe (Array Circle)
      findThreeCircleBasis i
        | i >= n - 1 = Nothing
        | otherwise =
            case findInnerPair i (i + 1) of
              Just result -> Just result
              Nothing -> findThreeCircleBasis (i + 1)

      findInnerPair :: Int -> Int -> Maybe (Array Circle)
      findInnerPair i j
        | j >= n = Nothing
        | otherwise =
            case Array.index basis i, Array.index basis j of
              Just bi, Just bj ->
                let
                  candidate = encloseBasis3 bi bj p
                  cij = encloseBasis2 bi bj
                  cip = encloseBasis2 bi p
                  cjp = encloseBasis2 bj p
                in
                  if enclosesNot cij p && enclosesNot cip bj && enclosesNot cjp bi && enclosesWeakAll candidate basis then Just [ bi, bj, p ]
                  else findInnerPair i (j + 1)
              _, _ -> findInnerPair i (j + 1)

packEnclose :: Array Circle -> Circle
packEnclose circles = go 0 [] Nothing
  where
  n = Array.length circles

  go :: Int -> Array Circle -> Maybe Circle -> Circle
  go i basis maybeE
    | i >= n =
        case maybeE of
          Just e -> e
          Nothing -> { x: 0.0, y: 0.0, r: 0.0 }
    | otherwise =
        case Array.index circles i of
          Nothing -> go (i + 1) basis maybeE
          Just p ->
            case maybeE of
              Just e | enclosesWeak e p ->
                go (i + 1) basis maybeE
              _ ->
                let
                  newBasis = extendBasis basis p
                  newE = encloseBasis newBasis
                in
                  go 0 newBasis (Just newE)

-- ============================================================================
-- HIERARCHY INTEGRATION
-- ============================================================================

-- | Pack node extends hierarchy with circle coordinates
data PackNode a = PackNode
  { data_ :: a
  , depth :: Int
  , height :: Int
  , value :: Number
  , children :: Array (PackNode a)
  , x :: Number
  , y :: Number
  , r :: Number
  }

derive instance eqPackNode :: Eq a => Eq (PackNode a)
derive instance ordPackNode :: Ord a => Ord (PackNode a)

instance showPackNode :: Show a => Show (PackNode a) where
  show (PackNode node) =
    "PackNode { data_: " <> show node.data_
      <> ", depth: "
      <> show node.depth
      <> ", height: "
      <> show node.height
      <> ", value: "
      <> show node.value
      <> ", x: "
      <> show node.x
      <> ", y: "
      <> show node.y
      <> ", r: "
      <> show node.r
      <> ", children: ["
      <> show (Array.length node.children)
      <> " children]"
      <> " }"

-- | Configuration for pack layout
type PackConfig a =
  { size :: { width :: Number, height :: Number }
  , padding :: Number
  , radius :: Maybe (a -> Number)
  }

-- | Default pack configuration
defaultPackConfig :: forall a. PackConfig a
defaultPackConfig =
  { size: { width: 1.0, height: 1.0 }
  , padding: 0.0
  , radius: Nothing
  }

-- | Simple hierarchy node for testing
newtype HierarchyData a = HierarchyData
  { data_ :: a
  , value :: Maybe Number
  , children :: Maybe (Array (HierarchyData a))
  }

-- | Create a hierarchy from data
-- Computes sum of values for each node
hierarchy :: forall a. HierarchyData a -> PackNode a
hierarchy (HierarchyData root) = go root 0
  where
  go :: { data_ :: a, value :: Maybe Number, children :: Maybe (Array (HierarchyData a)) } -> Int -> PackNode a
  go node depth =
    case node.children of
      Nothing ->
        -- Leaf node
        PackNode
          { data_: node.data_
          , depth: depth
          , height: 0
          , value: fromMaybe 0.0 node.value
          , children: []
          , x: 0.0
          , y: 0.0
          , r: 0.0
          }
      Just kids ->
        -- Internal node - recursively process children
        let
          childNodes = map (\(HierarchyData child) -> go child (depth + 1)) kids
          -- Sort children by value in descending order (matches D3 behavior)
          sortedChildren = Array.sortWith (\(PackNode c) -> negate c.value) childNodes
          childHeights = map (\(PackNode c) -> c.height) sortedChildren
          maxChildHeight = foldl max 0 childHeights
          sumValue = foldl (\acc (PackNode c) -> acc + c.value) 0.0 sortedChildren
        in
          PackNode
            { data_: node.data_
            , depth: depth
            , height: maxChildHeight + 1
            , value: sumValue
            , children: sortedChildren
            , x: 0.0
            , y: 0.0
            , r: 0.0
            }

-- | Apply pack layout to a hierarchy using the map-based algorithm
-- | Implements D3's two-pass algorithm for proper padding
pack :: forall a. PackConfig a -> PackNode a -> PackNode a
pack config root =
  let
    -- Step 1: Assign radii to leaves based on value
    withRadii = assignRadii config root

    -- Step 2a: First pack with k=1.0 (no padding) to compute unpadded radius
    -- IMPORTANT: Use padding=0 for first pack (matches D3's constantZero)
    firstPack = packChildren 1.0 (config { padding = 0.0 }) withRadii

    -- Step 2b: Compute k from unpadded root radius
    -- k = r / min(width, height) where r is the unpadded root radius
    PackNode firstRoot = firstPack
    k = firstRoot.r / min config.size.width config.size.height

    -- Step 2c: Second pack with padding scaled by k
    -- NOTE: Using withRadii (same radii as firstPack since padding=0 in first pass)
    packed = packChildren k config withRadii

    -- Step 3: Scale to fit size
    scaled = scaleToSize config packed
  in
    scaled

-- | Assign radii to leaf nodes based on their values
assignRadii :: forall a. PackConfig a -> PackNode a -> PackNode a
assignRadii config (PackNode node) =
  case node.children of
    [] ->
      -- Leaf node - assign radius
      let
        r = case config.radius of
          Just radiusFn -> radiusFn node.data_
          Nothing -> sqrt node.value
      in
        PackNode (node { r = max 0.0 r })
    _ ->
      -- Internal node - recursively assign to children
      PackNode (node { children = map (assignRadii config) node.children })

-- | Pack children at each level of the hierarchy using map-based algorithm
-- | Takes k parameter for padding scale (k=1.0 for first pass, computed k for second pass)
packChildren :: forall a. Number -> PackConfig a -> PackNode a -> PackNode a
packChildren k config (PackNode node) =
  case node.children of
    [] -> PackNode node -- Leaf node, nothing to pack
    kids ->
      -- Internal node - pack children
      let
        -- Scale padding by k (k=1.0 for first pack, k=1/(2*r) for second pack)
        paddingScaled = config.padding * k

        -- Recursively pack children's children first
        packedKids = map (packChildren k config) kids

        -- Create circles array from children, adding scaled padding to radii
        inputCircles = map (addPadding paddingScaled <<< nodeToCircle) packedKids

        -- Pack the circles using map-based algorithm (with padding)
        packed = packSiblingsMap inputCircles

        -- Remove padding from packed circles and update children positions
        circlesWithoutPadding = map (removePadding paddingScaled) packed.circles
        updatedKids = Array.zipWith updateNodePosition packedKids circlesWithoutPadding

        -- Update node radius: enclosing radius + padding (matches D3: node.r = e + r)
        nodeWithR = node { r = packed.radius + paddingScaled, children = updatedKids }
      in
        PackNode nodeWithR

  where
  nodeToCircle :: PackNode a -> Circle
  nodeToCircle (PackNode n) = { x: n.x, y: n.y, r: n.r }

  addPadding :: Number -> Circle -> Circle
  addPadding p c = c { r = c.r + p }

  removePadding :: Number -> Circle -> Circle
  removePadding p c = c { r = c.r - p }

  updateNodePosition :: PackNode a -> Circle -> PackNode a
  updateNodePosition (PackNode n) c = PackNode (n { x = c.x, y = c.y }) -- Don't update radius!

-- | Scale the packed layout to fit the configured size
scaleToSize :: forall a. PackConfig a -> PackNode a -> PackNode a
scaleToSize config (PackNode node) =
  let
    dx = config.size.width
    dy = config.size.height
    k = min dx dy / (2.0 * node.r)
    centerX = dx / 2.0
    centerY = dy / 2.0
    scaledRootR = k * node.r -- Scale the root's radius too!
  in
    PackNode (node { x = centerX, y = centerY, r = scaledRootR, children = map (scaleNode k centerX centerY) node.children })

  where
  scaleNode :: Number -> Number -> Number -> PackNode a -> PackNode a
  scaleNode scale parentX parentY (PackNode child) =
    let
      scaledX = parentX + scale * child.x
      scaledY = parentY + scale * child.y
      scaledR = scale * child.r
    in
      PackNode (child { x = scaledX, y = scaledY, r = scaledR, children = map (scaleNode scale scaledX scaledY) child.children })
