module PSD3.Examples.LesMisForce where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AJAX
import D3.Viz.LesMiserables as LesMis
import D3.Viz.LesMiserables.File (readGraphFromFileContents)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Internal.FFI (linksForceName_)
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce, initialize)
import PSD3.Internal.Simulation.Types (D3SimulationState_, RegularForceType(..), ForceType(..), allNodes, initialSimulationState)
import PSD3.Interpreter.D3 (runWithD3_Simulation)
import PSD3.RoutingDSL (routeToPath)
import PSD3.Website.Types (Route(..))

type State = { simulation :: D3SimulationState_ }

forces :: { center :: _, collision :: _, links :: _, manyBodyNeg :: _ }
forces = {
    manyBodyNeg: createForce "many body negative" (RegularForce ForceManyBody) allNodes [ F.strength (-40.0) ]
  , collision:   createForce "collision"          (RegularForce ForceCollide)  allNodes [ F.radius 4.0 ]
  , center:      createForce "center"             (RegularForce ForceCenter)   allNodes [ F.x 0.0, F.y 0.0, F.strength 1.0 ]
  , links:       createLinkForce Nothing []
}

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { simulation: initialSimulationState (initialize [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]) }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

data Action = Initialize

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    log "LesMisForceExample: Loading data"
    response <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    let graph = readGraphFromFileContents response
    log "LesMisForceExample: Drawing"
    let forcesArray = [ forces.manyBodyNeg, forces.collision, forces.center, forces.links ]
        activeForces = Set.fromFoldable ["many body negative", "collision", "center", linksForceName_]
    runWithD3_Simulation do
      LesMis.drawSimplified forcesArray activeForces graph "#lesmis-force-viz"
    log "LesMisForceExample: Complete"
    pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "example-page" ] ]
    [ HH.header
        [ HP.classes [ HH.ClassName "example-header" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath ExamplesGallery ]
            [ HH.text "← Examples Gallery" ]
        , HH.h1_ [ HH.text "Les Misérables Network" ]
        , HH.p_ [ HH.text "Character co-occurrence force-directed graph with physics simulation." ]
        ]
    , HH.section
        [ HP.classes [ HH.ClassName "example-viz-section" ] ]
        [ HH.h2_ [ HH.text "Visualization" ]
        , HH.div
            [ HP.id "lesmis-force-viz"
            , HP.classes [ HH.ClassName "example-viz" ]
            ]
            []
        ]
    ]
