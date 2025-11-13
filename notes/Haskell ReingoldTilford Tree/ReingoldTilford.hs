{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE ViewPatterns  #-}

-- |
-- Contains just the unaugmented algorithm, with no extra
-- bells or whistles. Useful for understand what the algorithm
-- does, or just drawing a tree without doing anything else.

module ReingoldTilford.Algorithm
  ( render, petrify
  , render', spliceContours, scanContours
  )
where

import ReingoldTilford.Types

-- |
-- Assign child distances to each node in the tree, which can
-- be converted into (x, y) coordinates for each node by `petrify'.
render :: Distance -> BinTree a -> BinTree Distance
render minsep t =
  case go t of
    (t, _) -> t
  where
    go :: BinTree a -> (BinTree Distance, Contours)
    go = \case
      Leaf -> (Leaf, emptyContours)
      Node l _ r ->
        let (l', lctrs) = go l
            (r', rctrs) = go r
            scan = scanContours minsep lctrs rctrs
            rootDist = fromIntegral $ rootOffset scan
        in (Node l' rootDist r', spliceContours rootDist scan (lctrs, rctrs))

-- |
-- A version of `render' that returns the contours of the whole
-- tree. Useful for debugging, or if you're curious about the
-- intermediate data.
render' :: Distance -> BinTree a -> (BinTree Distance, Contours)
render' minsep = go
  where
    go :: BinTree a -> (BinTree Distance, Contours)
    go = \case
      Leaf -> (Leaf, emptyContours)
      Node l _ r ->
        let (l', lctrs) = go l
            (r', rctrs) = go r
            scan = scanContours minsep lctrs rctrs
            rootDist = fromIntegral $ rootOffset scan
        in (Node l' rootDist r', spliceContours rootDist scan (lctrs, rctrs))

petrify :: (XCoord, YCoord) -> BinTree Distance -> BinTree (XCoord, YCoord)
petrify _ Leaf = Leaf
petrify (x, y) (Node l dist r) =
  let l' = petrify (x - fromIntegral dist, y+1) l
      r' = petrify (x + fromIntegral dist, y+1) r
  in Node l' (x, y) r'

-- |
-- Given the contours for our two subtrees, construct the contours
-- for the whole tree, using the information we calculated from the contour scan.
spliceContours :: Distance -> ScanResult -> (Contours, Contours) -> Contours
spliceContours (fromIntegral -> rootOffset) scan (Contours ll lr, Contours rl rr) =
  Contours leftContour rightContour
  where
    compareLength :: [a] -> [b] -> Ordering
    compareLength [] [] = EQ
    compareLength [] _y = LT
    compareLength _x [] = GT
    compareLength (_:xs) (_:ys) = compareLength xs ys

    leftContour :: Contour
    leftContour = case (rl `compareLength` ll, ll, rl) of
      (_, [], []) -> [0]
      (EQ, _, _) -> negate rootOffset : ll
      (LT, _, _) -> negate rootOffset : ll
      (GT, [], _) -> rootOffset : rl
      (GT, _, _) ->
        negate rootOffset : take (length ll - 1) ll ++ (rloffsum scan - lloffsum scan) : drop (length ll) rl

    rightContour :: Contour
    rightContour = case (lr `compareLength` rr, rr, lr) of
      (_, [], []) -> [0]
      (EQ, _, _) -> rootOffset : rr
      (LT, _, _) -> rootOffset : rr
      (GT, [], _) -> negate rootOffset : lr
      (GT, _, _) ->
        rootOffset : take (length rr - 1) rr ++ (lroffsum scan - rroffsum scan) : drop (length rr) lr

-- |
-- Recurse down the contours of our subtrees, calculating the separation
-- at the root.
scanContours :: Distance -> Contours -> Contours -> ScanResult
scanContours (fromIntegral -> minsep) lctrs rctrs =
  let ScanResult rootSep lloffsum lroffsum rloffsum rroffsum =
        go minsep (ScanResult minsep 0 0 0 0) lctrs rctrs
      rootOffset = (rootSep + 1) `div` 2
  in ScanResult
       rootOffset
       (lloffsum-rootOffset)
       (lroffsum-rootOffset)
       (rloffsum+rootOffset)
       (rroffsum+rootOffset)
  where
    go :: Offset -> ScanResult -> Contours -> Contours -> ScanResult
    go cursep
       acc@(ScanResult rootSep lloffsum lroffsum rloffsum rroffsum)
       (Contours (x : ll) (y : lr))
       (Contours (z : rl) (w : rr)) =
      -- cursep always = distance between y and z (or rather, the nodes they represent)
      let (cursep', rootSep') = if cursep < minsep
            then let diff = minsep - cursep in (minsep + z - y, rootSep + fromIntegral diff)
            else (cursep + z - y, rootSep)
          (lloffsum', lroffsum', rloffsum', rroffsum') =
            (lloffsum+x, lroffsum+y, rloffsum+z, rroffsum+w)
          acc' = ScanResult rootSep' lloffsum' lroffsum' rloffsum' rroffsum'
      in go cursep' acc' (Contours ll lr) (Contours rl rr)
    go cursep acc _ _ =
      acc