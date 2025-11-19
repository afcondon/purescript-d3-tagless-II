module Component.LesMisQueryDemo where

-- | Component wrapper for LesMis Query Language Demo
-- |
-- | Demonstrates the PSD3v2 selection query language with interactive buttons
-- | that dynamically query and modify node groups.

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Array as Array
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import D3.Viz.LesMis.QueryDemo as QueryDemo
import D3.Viz.LesMiserables.Model (LesMisSimNode)
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import PSD3v2.Interpreter.D3v2 as D3v2
import PSD3.Internal.Simulation.Types (Force, ForceType(..), RegularForceType(..), allNodes, initialSimulationState)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import Data.Map as Map
import Affjax.Web as AJAX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))

type State = Unit

data Action = Initialize

-- | Forces configuration for LesMis
forces :: { center :: Force LesMisSimNode, collision :: Force LesMisSimNode, links :: Force LesMisSimNode, manyBodyNeg :: Force LesMisSimNode }
forces =
  { manyBodyNeg: createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strengthVal (-100.0) ]
  , collision: createForce "collision" (RegularForce ForceCollide) allNodes [ F.radiusVal 6.0 ]
  , center: createForce "center" (RegularForce ForceCenter) allNodes [ F.xVal 0.0, F.yVal 0.0, F.strengthVal 0.1 ]
  , links: createLinkForce allNodes []
  }

forceLibrary :: Map.Map String (Force LesMisSimNode)
forceLibrary = initialize [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> unit
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ HP.class_ (HH.ClassName "lesmis-query-demo-page") ]
    [ HH.div [ HP.class_ (HH.ClassName "page-header") ]
        [ HH.h1_ [ HH.text "Selection Query Language Demo" ]
        , HH.p [ HP.class_ (HH.ClassName "page-description") ]
            [ HH.text "Demonstrates the PSD3v2 selection query language (PSD3v2.Selection.Query). Click buttons to toggle group sizes using queryAll + filterByData." ]
        , HH.div [ HP.class_ (HH.ClassName "code-example") ]
            [ HH.pre_
                [ HH.code_
                    [ HH.text """-- Query all circles
allCircles <- queryAll "circle" selections

-- Filter to specific group using data predicate
groupNodes <- filterByData (\\d -> d.group == targetGroup) allCircles

-- Update attributes
setAttrs [radius 10.0] groupNodes"""
                    ]
                ]
            ]
        ]

    , HH.div
        [ HP.id "lesmis-query-demo-viz"
        , HP.class_ (HH.ClassName "viz-container")
        ]
        []
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    -- Load data
    result <- H.liftAff $ AJAX.get ResponseFormat.string "/data/lesmiserables.json"
    case result of
      Left err -> liftEffect $ do
        -- Log error but don't crash - helps with development
        pure unit

      Right response -> do
        let model = readGraphFromFileContents (Right response)
        let forcesArray = [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]
            activeForces = Set.fromFoldable [ "many body negative", "collision", "center", "links" ]

        -- Run visualization
        let initialState = { simulation: initialSimulationState forceLibrary }
        void $ H.liftAff $ D3v2.runD3v2SimM initialState $
          QueryDemo.drawLesMisQueryDemo forcesArray activeForces model "#lesmis-query-demo-viz"
