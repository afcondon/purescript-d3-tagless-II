module PSD3.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import PSD3.About as About
import PSD3.Tutorial as Tutorial
import PSD3.SimpleCharts as SimpleCharts
import PSD3.ChordDiagram as ChordDiagram
import PSD3.BubbleChart as BubbleChart
import PSD3.SankeyDiagram as SankeyDiagram
import PSD3.Hierarchies as Hierarchies
import PSD3.Interpreters as Interpreters
import PSD3.RoutingDSL (routing, routeToPath)
import PSD3.CodeExplorerWrapper as CodeExplorer
import PSD3.Types (Route(..))
import Routing.Hash (matches, setHash)
import Type.Proxy (Proxy(..))

-- | Main application state
type State = {
  currentRoute :: Route
}

-- | Main application actions
data Action
  = Initialize
  | Navigate Route
  | RouteChanged (Maybe Route)

-- | Child component slots
type Slots =
  ( about :: forall q. H.Slot q Void Unit
  , tutorial :: forall q. H.Slot q Void Unit
  , simpleCharts :: forall q. H.Slot q Void Unit
  , chordDiagram :: forall q. H.Slot q Void Unit
  , bubbleChart :: forall q. H.Slot q Void Unit
  , sankeyDiagram :: forall q. H.Slot q Void Unit
  , hierarchies :: forall q. H.Slot q Void Unit
  , interpreters :: forall q. H.Slot q Void Unit
  , codeExplorer :: forall q. H.Slot q Void Unit
  )

_about = Proxy :: Proxy "about"
_tutorial = Proxy :: Proxy "tutorial"
_simpleCharts = Proxy :: Proxy "simpleCharts"
_chordDiagram = Proxy :: Proxy "chordDiagram"
_bubbleChart = Proxy :: Proxy "bubbleChart"
_sankeyDiagram = Proxy :: Proxy "sankeyDiagram"
_hierarchies = Proxy :: Proxy "hierarchies"
_interpreters = Proxy :: Proxy "interpreters"
_codeExplorer = Proxy :: Proxy "codeExplorer"

-- | Main application component
component :: forall q i. H.Component q i Void Aff
component = H.mkComponent
  { initialState: \_ -> { currentRoute: About } -- note, it really doesn't matter what's initialized here as Initialize reads the route from the hash
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "app" ] ]
    [ -- Main content area (no separate navigation component)
      HH.main
        [ HP.classes [ HH.ClassName "app__main" ] ]
        [ renderPage state.currentRoute ]
    ]

-- | Render the current page based on route
renderPage :: Route -> H.ComponentHTML Action Slots Aff
renderPage route = case spy "Route is" route of
  About ->
    HH.slot_ _about unit About.component unit

  Tutorial ->
    HH.slot_ _tutorial unit Tutorial.component unit

  SimpleCharts ->
    HH.slot_ _simpleCharts unit SimpleCharts.component unit

  ChordDiagram ->
    HH.slot_ _chordDiagram unit ChordDiagram.component unit

  BubbleChart ->
    HH.slot_ _bubbleChart unit BubbleChart.component unit

  SankeyDiagram ->
    HH.slot_ _sankeyDiagram unit SankeyDiagram.component unit

  Hierarchies ->
    HH.slot_ _hierarchies unit Hierarchies.component unit

  Interpreters ->
    HH.slot_ _interpreters unit Interpreters.component unit

  CodeExplorer ->
    HH.slot_ _codeExplorer unit CodeExplorer.component unit

  NotFound ->
    HH.div
      [ HP.classes [ HH.ClassName "not-found" ] ]
      [ HH.h1_ [ HH.text "404 - Page Not Found" ]
      , HH.p_ [ HH.text "The page you're looking for doesn't exist." ]
      , HH.a
          [ HP.href $ "#" <> routeToPath About ]
          [ HH.text "Go to About" ]
      ]

handleAction :: Action -> H.HalogenM State Action Slots Void Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Subscribe to route changes using purescript-routing
    -- This handles both initial route and hash changes (back/forward buttons)
    _ <- H.subscribe $ HS.makeEmitter \push -> do
      matches routing \_ newRoute -> push (RouteChanged (Just newRoute))
    pure unit

  Navigate route -> do
    -- Navigation is now handled by Routing.Hash.setHash
    -- which will trigger the matches subscription above
    H.liftEffect $ setHash (routeToPath route)

  RouteChanged maybeRoute -> do
    -- When route changes (initial load, back/forward, or manual navigation)
    case maybeRoute of
      Just route -> H.modify_ _ { currentRoute = route }
      Nothing -> H.modify_ _ { currentRoute = NotFound } -- Fallback if route doesn't match

-- | Entry point
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
