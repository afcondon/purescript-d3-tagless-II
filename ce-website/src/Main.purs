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
import Viz.LesMis.GUPGridTest (initGridTest)
import Viz.SpagoGridTest (initSpagoGridTest)
import Component.SpagoGridApp as SpagoGridApp
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLElement (toElement, fromElement)
import Web.DOM.ParentNode (querySelector, QuerySelector(..))
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toParentNode)
import Data.Maybe (Maybe(..))

-- | Scale factor: 13 copies = ~1000 nodes (77 * 13 = 1001)
scaleFactor :: Int
scaleFactor = 13

main :: Effect Unit
main = do
  log "[Main] Starting with Halogen wrapper around SpagoGridTest..."
  log "[Main] IMPORTANT: Using selectElement to mount into #app (not body)"
  HA.runHalogenAff do
    -- Mount into #app, not body!
    mAppEl <- HA.selectElement (QuerySelector "#app")
    case mAppEl of
      Nothing -> liftEffect $ log "[Main] ERROR: Could not find #app element!"
      Just appEl -> do
        liftEffect $ log "[Main] Found #app, mounting Halogen..."
        _ <- runUI SpagoGridApp.component unit appEl
        pure unit

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
