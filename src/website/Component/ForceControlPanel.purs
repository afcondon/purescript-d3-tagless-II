-- | ForceControlPanel - Halogen component for interactive force tuning
-- |
-- | Features:
-- | - Toggle forces on/off
-- | - Adjust force parameters via sliders
-- | - Collapsible sections per force
-- | - Real-time updates to simulation
module Component.ForceControlPanel where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Set as Set
import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | FFI imports
foreign import getForceNames_ :: Effect (Array String)
foreign import updateForceParam_ :: String -> String -> Number -> Effect Unit
foreign import toggleForce_ :: String -> Boolean -> Effect Unit
foreign import startSimulation_ :: Effect Unit
foreign import stopSimulation_ :: Effect Unit
foreign import reheatSimulation_ :: Effect Unit
foreign import getForceParam_ :: String -> String -> Effect (Nullable Number)
foreign import unsafeParseFloat :: String -> Number
foreign import isNaN :: Number -> Boolean
foreign import copyToClipboard_ :: String -> Effect Unit
foreign import getForceSettings_ :: Effect String

-- | Slider configuration
type SliderConfig =
  { label :: String
  , param :: String
  , min :: Number
  , max :: Number
  , step :: Number
  , defaultValue :: Number
  }

-- | Force info with its parameters
type ForceInfo =
  { name :: String
  , forceType :: String
  , sliders :: Array SliderConfig
  }

-- | Component state
type State =
  { forceNames :: Array String
  , activeForces :: Set.Set String
  , expandedForces :: Set.Set String
  , sliderValues :: Array { forceName :: String, param :: String, value :: Number }
  , isMinimized :: Boolean
  }

-- | Component actions
data Action
  = Initialize
  | RefreshForces
  | ToggleForce String
  | ExpandForce String
  | CollapseForce String
  | SetParam String String String  -- forceName, param, stringValue
  | ToggleMinimized
  | SimStart
  | SimStop
  | SimReheat
  | CopySettings

-- | Query type for parent to send commands
data Query a
  = Refresh a

-- | Component
component :: forall input m. MonadAff m => H.Component Query input Void m
component =
  H.mkComponent
    { initialState: \_ ->
        { forceNames: []
        , activeForces: Set.empty
        , expandedForces: Set.empty
        , sliderValues: []
        , isMinimized: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        }
    }

handleQuery :: forall a m. MonadAff m => Query a -> H.HalogenM State Action () Void m (Maybe a)
handleQuery = case _ of
  Refresh a -> do
    handleAction RefreshForces
    pure (Just a)

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    -- Wait a bit for simulation to initialize
    H.liftAff $ delay (Milliseconds 500.0)
    handleAction RefreshForces

  RefreshForces -> do
    names <- liftEffect getForceNames_
    H.modify_ _ { forceNames = names, activeForces = Set.fromFoldable names }

  ToggleForce name -> do
    state <- H.get
    let isActive = Set.member name state.activeForces
        newActive = if isActive
                    then Set.delete name state.activeForces
                    else Set.insert name state.activeForces
    H.modify_ _ { activeForces = newActive }
    liftEffect $ toggleForce_ name (not isActive)

  ExpandForce name -> do
    H.modify_ \s -> s { expandedForces = Set.insert name s.expandedForces }

  CollapseForce name -> do
    H.modify_ \s -> s { expandedForces = Set.delete name s.expandedForces }

  SetParam forceName param strValue -> do
    let value = unsafeParseFloat strValue
    unless (isNaN value) do
      liftEffect $ updateForceParam_ forceName param value

  ToggleMinimized -> do
    H.modify_ \s -> s { isMinimized = not s.isMinimized }

  SimStart -> liftEffect startSimulation_
  SimStop -> liftEffect stopSimulation_
  SimReheat -> liftEffect reheatSimulation_

  CopySettings -> do
    settings <- liftEffect getForceSettings_
    liftEffect $ copyToClipboard_ settings

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes
        [ HH.ClassName "force-control-panel"
        , HH.ClassName if state.isMinimized then "minimized" else "expanded"
        ]
    ]
    [ renderHeader state
    , if state.isMinimized
        then HH.text ""
        else renderBody state
    ]

renderHeader :: forall m. State -> H.ComponentHTML Action () m
renderHeader state =
  HH.div
    [ HP.class_ (HH.ClassName "force-panel-header")
    , HE.onClick \_ -> ToggleMinimized
    ]
    [ HH.h3
        [ HP.class_ (HH.ClassName "force-panel-title") ]
        [ HH.text $ "Force Controls " <> if state.isMinimized then "[+]" else "[-]" ]
    , HH.div
        [ HP.class_ (HH.ClassName "force-panel-summary") ]
        [ HH.text $ show (Set.size state.activeForces) <> "/" <> show (length state.forceNames) <> " active" ]
    ]

renderBody :: forall m. State -> H.ComponentHTML Action () m
renderBody state =
  HH.div
    [ HP.class_ (HH.ClassName "force-panel-body") ]
    [ renderSimControls
    , HH.div
        [ HP.class_ (HH.ClassName "force-list") ]
        (map (renderForceItem state) state.forceNames)
    ]

renderSimControls :: forall m. H.ComponentHTML Action () m
renderSimControls =
  HH.div
    [ HP.class_ (HH.ClassName "sim-controls") ]
    [ HH.button
        [ HP.class_ (HH.ClassName "sim-btn start")
        , HE.onClick \_ -> SimStart
        ]
        [ HH.text "Start" ]
    , HH.button
        [ HP.class_ (HH.ClassName "sim-btn stop")
        , HE.onClick \_ -> SimStop
        ]
        [ HH.text "Stop" ]
    , HH.button
        [ HP.class_ (HH.ClassName "sim-btn reset")
        , HE.onClick \_ -> SimReheat
        ]
        [ HH.text "Reheat" ]
    , HH.button
        [ HP.class_ (HH.ClassName "sim-btn copy")
        , HE.onClick \_ -> CopySettings
        ]
        [ HH.text "Copy" ]
    ]

renderForceItem :: forall m. State -> String -> H.ComponentHTML Action () m
renderForceItem state name =
  let
    isActive = Set.member name state.activeForces
    isExpanded = Set.member name state.expandedForces
    forceType = getForceType name
    sliders = getSlidersForForce forceType
  in
    HH.div
      [ HP.classes
          [ HH.ClassName "force-item"
          , HH.ClassName if isActive then "active" else "inactive"
          , HH.ClassName if isExpanded then "expanded" else "collapsed"
          ]
      ]
      [ -- Force header
        HH.div
          [ HP.class_ (HH.ClassName "force-item-header") ]
          [ HH.div
              [ HP.class_ (HH.ClassName "force-toggle")
              , HE.onClick \_ -> ToggleForce name
              ]
              [ HH.span
                  [ HP.class_ (HH.ClassName "toggle-indicator") ]
                  [ HH.text if isActive then "ON" else "OFF" ]
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "force-info")
              , HE.onClick \_ -> if isExpanded then CollapseForce name else ExpandForce name
              ]
              [ HH.span [ HP.class_ (HH.ClassName "force-name") ] [ HH.text name ]
              , HH.span [ HP.class_ (HH.ClassName "force-type") ] [ HH.text forceType ]
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "expand-toggle")
              , HE.onClick \_ -> if isExpanded then CollapseForce name else ExpandForce name
              ]
              [ HH.text if isExpanded then "v" else ">" ]
          ]
      -- Sliders when expanded
      , if isExpanded
          then HH.div
            [ HP.class_ (HH.ClassName "force-sliders") ]
            (map (renderSlider name) sliders)
          else HH.text ""
      ]

-- | Determine force type from name
getForceType :: String -> String
getForceType name
  | contains (Pattern "charge") name = "charge"
  | contains (Pattern "collid") name = "collide"
  | contains (Pattern "center") name = "center"
  | contains (Pattern "link") name = "link"
  | contains (Pattern "Orbit") name = "radial"
  | contains (Pattern "cluster") name = "position"
  | otherwise = "unknown"

-- | Get sliders for a force type
getSlidersForForce :: String -> Array SliderConfig
getSlidersForForce forceType = case forceType of
  "charge" ->
    [ { label: "Strength", param: "strength", min: -500.0, max: 100.0, step: 10.0, defaultValue: -100.0 }
    , { label: "Theta", param: "theta", min: 0.0, max: 2.0, step: 0.1, defaultValue: 0.9 }
    , { label: "Dist Min", param: "distanceMin", min: 0.0, max: 100.0, step: 1.0, defaultValue: 1.0 }
    , { label: "Dist Max", param: "distanceMax", min: 0.0, max: 1000.0, step: 50.0, defaultValue: 400.0 }
    ]

  "collide" ->
    [ { label: "Radius", param: "radius", min: 0.0, max: 100.0, step: 1.0, defaultValue: 10.0 }
    , { label: "Strength", param: "strength", min: 0.0, max: 2.0, step: 0.1, defaultValue: 1.0 }
    , { label: "Iterations", param: "iterations", min: 1.0, max: 10.0, step: 1.0, defaultValue: 3.0 }
    ]

  "center" ->
    [ { label: "Strength", param: "strength", min: 0.0, max: 1.0, step: 0.05, defaultValue: 0.5 }
    , { label: "X", param: "x", min: -500.0, max: 500.0, step: 10.0, defaultValue: 0.0 }
    , { label: "Y", param: "y", min: -500.0, max: 500.0, step: 10.0, defaultValue: 0.0 }
    ]

  "link" ->
    [ { label: "Distance", param: "distance", min: 0.0, max: 200.0, step: 5.0, defaultValue: 50.0 }
    , { label: "Iterations", param: "iterations", min: 1.0, max: 10.0, step: 1.0, defaultValue: 1.0 }
    ]

  "radial" ->
    [ { label: "Strength", param: "strength", min: 0.0, max: 1.0, step: 0.05, defaultValue: 0.7 }
    , { label: "Radius", param: "radius", min: 0.0, max: 2000.0, step: 50.0, defaultValue: 900.0 }
    ]

  "position" ->
    [ { label: "Strength", param: "strength", min: 0.0, max: 1.0, step: 0.05, defaultValue: 0.2 }
    ]

  _ -> []

renderSlider :: forall m. String -> SliderConfig -> H.ComponentHTML Action () m
renderSlider forceName config =
  HH.div
    [ HP.class_ (HH.ClassName "slider-row") ]
    [ HH.label
        [ HP.class_ (HH.ClassName "slider-label") ]
        [ HH.text config.label ]
    , HH.input
        [ HP.type_ HP.InputRange
        , HP.class_ (HH.ClassName "slider-input")
        , HP.min config.min
        , HP.max config.max
        , HP.step (HP.Step config.step)
        , HP.value (show config.defaultValue)
        , HE.onValueInput \val -> SetParam forceName config.param val
        ]
    , HH.span
        [ HP.class_ (HH.ClassName "slider-value") ]
        [ HH.text (show config.defaultValue) ]
    ]
