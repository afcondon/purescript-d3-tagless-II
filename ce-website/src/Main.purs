module Main where

import Prelude

import Affjax.Web as AW
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Array (length, concat, mapWithIndex)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Nullable (null)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Viz.LesMis.Model (LesMisRawModel, LesMisModel, LesMisNode, LesMisLink, processRawModel)
import Viz.LesMis.GUPDemo (initGUPDemo)

-- | Scale factor: 13 copies = ~1000 nodes (77 * 13 = 1001)
scaleFactor :: Int
scaleFactor = 13

main :: Effect Unit
main = do
  log "[LesMis Scale Test] Starting..."
  log $ "[LesMis Scale Test] Build: 2024-12-01 15:10 (scale=" <> show scaleFactor <> "x)"
  launchAff_ do
    result <- AW.get ResponseFormat.json "/data/miserables.json"
    case result of
      Left err -> liftEffect $ log $ "[LesMis] Fetch error: " <> AW.printError err
      Right response -> do
        case decodeJson response.body of
          Left err -> liftEffect $ log $ "[LesMis] Decode error: " <> printJsonDecodeError err
          Right (rawModel :: LesMisRawModel) -> do
            let baseModel = processRawModel rawModel
            let scaledModel = scaleModel scaleFactor baseModel
            liftEffect $ log $ "[LesMis] Base: " <> show (length baseModel.nodes) <> " nodes, " <> show (length baseModel.links) <> " links"
            liftEffect $ log $ "[LesMis] Scaled: " <> show (length scaledModel.nodes) <> " nodes, " <> show (length scaledModel.links) <> " links"
            liftEffect $ void $ initGUPDemo scaledModel "#app"
            liftEffect $ log "[LesMis] Demo initialized"

-- | Scale up a model by replicating nodes and links N times
-- | Each replica gets offset IDs and slightly different initial positions
scaleModel :: Int -> LesMisModel -> LesMisModel
scaleModel n model =
  let
    baseNodeCount = length model.nodes

    -- Replicate nodes N times with offset IDs
    replicatedNodes = concat $ mapWithIndex (\copyIdx _ ->
      map (offsetNode copyIdx baseNodeCount) model.nodes
    ) (replicate n unit)

    -- Replicate links N times with offset source/target
    replicatedLinks = concat $ mapWithIndex (\copyIdx _ ->
      map (offsetLink copyIdx baseNodeCount) model.links
    ) (replicate n unit)
  in
    { nodes: replicatedNodes
    , links: replicatedLinks
    }

-- | Offset a node's ID and position for a replica
offsetNode :: Int -> Int -> LesMisNode -> LesMisNode
offsetNode copyIdx baseCount node =
  let
    newId = node.id <> "_" <> show copyIdx
    newIndex = node.index + (copyIdx * baseCount)
    -- Spread replicas in a grid pattern
    offsetX = toNumber (copyIdx `mod` 4) * 200.0 - 300.0
    offsetY = toNumber (copyIdx / 4) * 200.0 - 300.0
  in
    node { id = newId
         , index = newIndex
         , x = node.x + offsetX
         , y = node.y + offsetY
         , vx = 0.0
         , vy = 0.0
         , fx = null
         , fy = null
         }

-- | Offset a link's source/target for a replica
offsetLink :: Int -> Int -> LesMisLink -> LesMisLink
offsetLink copyIdx baseCount link =
  link { source = link.source + (copyIdx * baseCount)
       , target = link.target + (copyIdx * baseCount)
       }

-- | Create an array of N units
replicate :: forall a. Int -> a -> Array a
replicate n a = map (const a) (range 0 (n - 1))

-- | Create a range [start..end]
range :: Int -> Int -> Array Int
range start end
  | start > end = []
  | otherwise = go start []
  where
  go i acc
    | i > end = acc
    | otherwise = go (i + 1) (acc <> [i])
