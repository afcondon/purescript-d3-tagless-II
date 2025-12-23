module D3.Viz.PatternTree.Euclidean
  ( parseEuclideanLabel
  , euclideanPattern
  , bjorklund
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.String as String

-- | Parse euclidean label "(n,k)" to get n and k values
parseEuclideanLabel :: String -> { n :: Int, k :: Int }
parseEuclideanLabel label =
  -- Label format is "(n,k)" - extract the numbers
  let stripped = String.replaceAll (String.Pattern "(") (String.Replacement "") $
                 String.replaceAll (String.Pattern ")") (String.Replacement "") label
      parts = String.split (String.Pattern ",") stripped
  in case parts of
    [nStr, kStr] ->
      { n: fromMaybe 0 (Int.fromString nStr)
      , k: fromMaybe 0 (Int.fromString kStr)
      }
    _ -> { n: 0, k: 0 }

-- | Compute euclidean rhythm pattern using Bjorklund algorithm
-- | Returns array of booleans: true = hit, false = rest
euclideanPattern :: Int -> Int -> Array Boolean
euclideanPattern n k
  | n <= 0 || k <= 0 = []
  | n >= k = Array.replicate k true  -- All hits
  | otherwise = bjorklund n k

-- | Bjorklund algorithm for computing euclidean rhythms
-- | Distributes n hits across k slots as evenly as possible
bjorklund :: Int -> Int -> Array Boolean
bjorklund n k = go (Array.replicate n [true]) (Array.replicate (k - n) [false])
  where
  go :: Array (Array Boolean) -> Array (Array Boolean) -> Array Boolean
  go left right
    | Array.length right <= 1 = Array.concat (left <> right)
    | otherwise =
        let minLen = min (Array.length left) (Array.length right)
            -- Interleave: take minLen from each and combine
            combined = Array.zipWith (<>) (Array.take minLen left) (Array.take minLen right)
            leftRemainder = Array.drop minLen left
            rightRemainder = Array.drop minLen right
        in if Array.length leftRemainder > 0
           then go combined leftRemainder
           else if Array.length rightRemainder > 0
                then go combined rightRemainder
                else Array.concat combined
