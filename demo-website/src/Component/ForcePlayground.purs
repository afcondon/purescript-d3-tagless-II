-- | Force Playground Component
-- |
-- | Interactive force-directed graph visualization for exploring
-- | network datasets. Features both real-world datasets and
-- | procedurally generated graphs with rich attributes.
-- |
-- | Styled as a fullscreen showcase with floating control panel.
module Component.ForcePlayground where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Shared.DataLoader (loadJSON, defaultConfig)
import PSD3.Shared.SiteNav as SiteNav
import D3.Viz.ForcePlayground.Model (NetworkRawModel, processRawModel, fromGeneratedGraph)
import D3.Viz.ForcePlayground.Simple (initSimpleForce, SimpleForceState, ForceId(..), toggleForce, setForcesEnabled, setUseLinkWeights, filterByCategory, showAllNodes)
import D3.Viz.ForcePlayground.Generator as Gen
import Unsafe.Coerce (unsafeCoerce)

-- | Force presets - pre-configured combinations
data ForcePreset
  = PresetStandard     -- All forces enabled (default)
  | PresetClustered    -- No X/Y centering - clusters drift apart
  | PresetMinimal      -- Only links and collide - organic layout

derive instance eqForcePreset :: Eq ForcePreset

presetLabel :: ForcePreset -> String
presetLabel = case _ of
  PresetStandard -> "Standard"
  PresetClustered -> "Clustered"
  PresetMinimal -> "Minimal"

presetTooltip :: ForcePreset -> String
presetTooltip = case _ of
  PresetStandard -> "All forces enabled - balanced layout"
  PresetClustered -> "No centering - clusters float freely"
  PresetMinimal -> "Links and collision only - organic shapes"

presetForces :: ForcePreset -> Set ForceId
presetForces = case _ of
  PresetStandard -> Set.fromFoldable [ForceX, ForceY, ForceCharge, ForceCollide, ForceLink]
  PresetClustered -> Set.fromFoldable [ForceCharge, ForceCollide, ForceLink]
  PresetMinimal -> Set.fromFoldable [ForceCollide, ForceLink]

-- | Available datasets
data Dataset
  = GeneratedGraph    -- Procedurally generated with rich attributes
  | KarateClub        -- Zachary's Karate Club (34 nodes, 2 groups)
  | EcoStMarks        -- St. Marks food web (54 nodes)
  | EcoEverglades     -- Everglades food web (69 nodes)
  | CElegansFrontal   -- C. elegans neural network (131 nodes)

derive instance eqDataset :: Eq Dataset

datasetLabel :: Dataset -> String
datasetLabel = case _ of
  GeneratedGraph -> "Generated Network"
  KarateClub -> "Karate Club (34)"
  EcoStMarks -> "St. Marks (54)"
  EcoEverglades -> "Everglades (69)"
  CElegansFrontal -> "C. elegans (131)"

datasetDescription :: Dataset -> String
datasetDescription = case _ of
  GeneratedGraph -> "Procedurally generated network with 20-30 clusters. Node size/color varies by category and importance."
  KarateClub -> "Classic social network - friendships between karate club members that split into two factions."
  EcoStMarks -> "Predator-prey relationships in St. Marks National Wildlife Refuge, Florida."
  EcoEverglades -> "Food web from the Florida Everglades ecosystem."
  CElegansFrontal -> "Synaptic connections between neurons in the frontal region of C. elegans."

-- | All forces enabled by default
allForces :: Set ForceId
allForces = Set.fromFoldable [ForceX, ForceY, ForceCharge, ForceCollide, ForceLink]

-- | Component state
type State =
  { selectedDataset :: Dataset
  , forceState :: Maybe (Ref SimpleForceState)
  , loading :: Boolean
  , error :: Maybe String
  , enabledForces :: Set ForceId  -- Track for UI rendering
  , activePreset :: Maybe ForcePreset  -- Track current preset (Nothing if custom)
  , useLinkWeights :: Boolean  -- Whether link strength uses data weights
  , shownCategories :: Set Int  -- Which node categories are visible (0-3)
  }

-- | All categories visible by default
allCategories :: Set Int
allCategories = Set.fromFoldable [0, 1, 2, 3]

-- | Component actions
data Action
  = Initialize
  | SelectDataset Dataset
  | LoadDataset
  | RegenerateGraph
  | ToggleForce ForceId
  | ApplyPreset ForcePreset
  | ToggleLinkWeights
  | ToggleCategory Int
  | ShowAllCategories

-- | Component definition
component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { selectedDataset: GeneratedGraph
      , forceState: Nothing
      , loading: false
      , error: Nothing
      , enabledForces: allForces
      , activePreset: Just PresetStandard
      , useLinkWeights: false
      , shownCategories: allCategories
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

-- =============================================================================
-- Render - Fullscreen Layout with Floating Panel
-- =============================================================================

render :: forall w. State -> HH.HTML w Action
render state =
  HH.div
    [ HP.classes [ HH.ClassName "force-playground-page" ] ]
    [ -- Site Navigation (top)
      SiteNav.render
        { logoSize: SiteNav.Normal
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Force Playground"
        }

    -- Fullscreen visualization container
    , HH.div
        [ HP.classes [ HH.ClassName "fullscreen-container", HH.ClassName "force-playground-viz" ] ]
        [ -- Main viz area
          HH.div
            [ HP.id "force-playground-container"
            , HP.classes [ HH.ClassName "fullscreen-viz", HH.ClassName "svg-container" ]
            ]
            []

        -- Floating control panel (top-right)
        , renderControlPanel state
        ]

    -- Loading overlay
    , if state.loading
        then HH.div
          [ HP.classes [ HH.ClassName "loading-overlay" ] ]
          [ HH.text "Loading..." ]
        else HH.text ""

    -- Error message
    , case state.error of
        Just err -> HH.div
          [ HP.classes [ HH.ClassName "error-toast" ] ]
          [ HH.text $ "Error: " <> err ]
        Nothing -> HH.text ""
    ]

-- | Floating control panel
renderControlPanel :: forall w. State -> HH.HTML w Action
renderControlPanel state =
  HH.div
    [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-right", HH.ClassName "floating-panel--medium", HH.ClassName "force-playground-panel" ] ]
    [ -- Panel title
      HH.h2
        [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
        [ HH.text "Controls" ]

    -- Dataset selector
    , HH.div
        [ HP.classes [ HH.ClassName "control-group" ] ]
        [ HH.h4_ [ HH.text "Dataset" ]
        , HH.select
            [ HP.classes [ HH.ClassName "control-select" ]
            , HE.onValueChange (\v -> SelectDataset (parseDataset v))
            ]
            [ HH.option [ HP.value "generated", HP.selected (state.selectedDataset == GeneratedGraph) ] [ HH.text (datasetLabel GeneratedGraph) ]
            , HH.option [ HP.value "karate-club", HP.selected (state.selectedDataset == KarateClub) ] [ HH.text (datasetLabel KarateClub) ]
            , HH.option [ HP.value "eco-stmarks", HP.selected (state.selectedDataset == EcoStMarks) ] [ HH.text (datasetLabel EcoStMarks) ]
            , HH.option [ HP.value "eco-everglades", HP.selected (state.selectedDataset == EcoEverglades) ] [ HH.text (datasetLabel EcoEverglades) ]
            , HH.option [ HP.value "c-elegans", HP.selected (state.selectedDataset == CElegansFrontal) ] [ HH.text (datasetLabel CElegansFrontal) ]
            ]
        , HH.p
            [ HP.classes [ HH.ClassName "control-description" ] ]
            [ HH.text (datasetDescription state.selectedDataset) ]
        -- Regenerate button (only for generated graphs)
        , if state.selectedDataset == GeneratedGraph
            then HH.button
              [ HP.classes [ HH.ClassName "control-button", HH.ClassName "control-button--secondary" ]
              , HE.onClick \_ -> RegenerateGraph
              ]
              [ HH.text "Regenerate" ]
            else HH.text ""
        ]

    -- Force Presets
    , HH.div
        [ HP.classes [ HH.ClassName "control-group" ] ]
        [ HH.h4_ [ HH.text "Presets" ]
        , HH.div
            [ HP.classes [ HH.ClassName "button-row" ] ]
            [ presetButton state PresetStandard
            , presetButton state PresetClustered
            , presetButton state PresetMinimal
            ]
        ]

    -- Force Toggles
    , HH.div
        [ HP.classes [ HH.ClassName "control-group" ] ]
        [ HH.h4_ [ HH.text "Forces" ]
        , HH.div
            [ HP.classes [ HH.ClassName "toggle-grid" ] ]
            [ forceToggle state ForceCharge "Charge"
            , forceToggle state ForceCollide "Collide"
            , forceToggle state ForceLink "Links"
            , forceToggle state ForceX "X Center"
            , forceToggle state ForceY "Y Center"
            ]
        ]

    -- Options
    , HH.div
        [ HP.classes [ HH.ClassName "control-group" ] ]
        [ HH.h4_ [ HH.text "Options" ]
        , linkWeightsToggle state
        ]

    -- Category Filters (only for generated graphs)
    , if state.selectedDataset == GeneratedGraph
        then HH.div
          [ HP.classes [ HH.ClassName "control-group" ] ]
          [ HH.h4_ [ HH.text "Filter Categories" ]
          , HH.div
              [ HP.classes [ HH.ClassName "category-filters" ] ]
              [ categoryToggle state 0 "Research" "#1f77b4"
              , categoryToggle state 1 "Industry" "#ff7f0e"
              , categoryToggle state 2 "Government" "#2ca02c"
              , categoryToggle state 3 "Community" "#d62728"
              ]
          , if state.shownCategories /= allCategories
              then HH.button
                [ HP.classes [ HH.ClassName "control-button", HH.ClassName "control-button--link" ]
                , HE.onClick \_ -> ShowAllCategories
                ]
                [ HH.text "Show All" ]
              else HH.text ""
          ]
        else HH.text ""

    -- Instructions
    , HH.div
        [ HP.classes [ HH.ClassName "panel-info-box" ] ]
        [ HH.p_ [ HH.text "Scroll to zoom, drag to pan." ]
        , HH.p_ [ HH.text "Click-drag nodes to pin them." ]
        , HH.p_ [ HH.text "Tiny drag to unpin." ]
        ]
    ]

-- | Render a preset button
presetButton :: forall w. State -> ForcePreset -> HH.HTML w Action
presetButton state preset =
  let isActive = state.activePreset == Just preset
  in HH.button
      [ HP.classes $
          [ HH.ClassName "control-button" ] <>
          if isActive then [ HH.ClassName "control-button--active" ] else []
      , HP.title (presetTooltip preset)
      , HE.onClick \_ -> ApplyPreset preset
      ]
      [ HH.text (presetLabel preset) ]

-- | Render a force toggle button
forceToggle :: forall w. State -> ForceId -> String -> HH.HTML w Action
forceToggle state forceId label =
  let isEnabled = Set.member forceId state.enabledForces
  in HH.button
      [ HP.classes $
          [ HH.ClassName "toggle-button" ] <>
          if isEnabled then [ HH.ClassName "toggle-button--on" ] else [ HH.ClassName "toggle-button--off" ]
      , HE.onClick \_ -> ToggleForce forceId
      ]
      [ HH.text label ]

-- | Render link weights toggle
linkWeightsToggle :: forall w. State -> HH.HTML w Action
linkWeightsToggle state =
  HH.label
    [ HP.classes [ HH.ClassName "checkbox-label" ] ]
    [ HH.input
        [ HP.type_ HP.InputCheckbox
        , HP.checked state.useLinkWeights
        , HE.onChecked \_ -> ToggleLinkWeights
        ]
    , HH.text "Weighted Links"
    ]

-- | Render a category filter toggle with color indicator
categoryToggle :: forall w. State -> Int -> String -> String -> HH.HTML w Action
categoryToggle state catId label color =
  let isVisible = Set.member catId state.shownCategories
  in HH.button
      [ HP.classes $
          [ HH.ClassName "category-button" ] <>
          if isVisible then [ HH.ClassName "category-button--active" ] else [ HH.ClassName "category-button--inactive" ]
      , HP.style $ "border-color: " <> color <> "; " <> if isVisible then "background-color: " <> color <> ";" else ""
      , HP.title $ "Toggle " <> label <> " nodes"
      , HE.onClick \_ -> ToggleCategory catId
      ]
      [ HH.text label ]

parseDataset :: String -> Dataset
parseDataset = case _ of
  "generated" -> GeneratedGraph
  "karate-club" -> KarateClub
  "eco-everglades" -> EcoEverglades
  "c-elegans" -> CElegansFrontal
  _ -> EcoStMarks

-- =============================================================================
-- Action Handlers
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction LoadDataset

  SelectDataset dataset -> do
    H.modify_ _ { selectedDataset = dataset }
    handleAction LoadDataset

  RegenerateGraph -> do
    handleAction LoadDataset

  ToggleForce forceId -> do
    state <- H.get
    case state.forceState of
      Nothing -> pure unit
      Just ref -> do
        newEnabled <- liftEffect $ toggleForce forceId ref
        let newForces = if newEnabled
              then Set.insert forceId state.enabledForces
              else Set.delete forceId state.enabledForces
        H.modify_ _ { enabledForces = newForces, activePreset = Nothing }

  ApplyPreset preset -> do
    state <- H.get
    case state.forceState of
      Nothing -> pure unit
      Just ref -> do
        let targetForces = presetForces preset
        newForces <- liftEffect $ setForcesEnabled targetForces ref
        H.modify_ _ { enabledForces = newForces, activePreset = Just preset }

  ToggleLinkWeights -> do
    state <- H.get
    case state.forceState of
      Nothing -> pure unit
      Just ref -> do
        let newValue = not state.useLinkWeights
        liftEffect $ setUseLinkWeights newValue ref
        H.modify_ _ { useLinkWeights = newValue }

  ToggleCategory catId -> do
    state <- H.get
    case state.forceState of
      Nothing -> pure unit
      Just ref -> do
        let isCurrentlyShown = Set.member catId state.shownCategories
        let newShown = if isCurrentlyShown
              then Set.delete catId state.shownCategories
              else Set.insert catId state.shownCategories
        liftEffect $ filterByCategory (Set.toUnfoldable newShown) ref
        H.modify_ _ { shownCategories = newShown }

  ShowAllCategories -> do
    state <- H.get
    case state.forceState of
      Nothing -> pure unit
      Just ref -> do
        liftEffect $ showAllNodes ref
        H.modify_ _ { shownCategories = allCategories }

  LoadDataset -> do
    state <- H.get
    H.modify_ _ { loading = true, error = Nothing }

    liftEffect $ clearContainer "#force-playground-container"

    model <- case state.selectedDataset of
      GeneratedGraph -> do
        liftEffect $ Console.log "Generating random network..."
        generated <- liftEffect $ Gen.generateGraph Gen.defaultConfig
        let m = fromGeneratedGraph generated
        liftEffect $ Console.log $ "Generated: " <> show (length m.nodes) <> " nodes, " <> show (length m.links) <> " links"
        pure m

      _ -> do
        let path = datasetPath state.selectedDataset
        result <- liftAff $ loadJSON defaultConfig path

        case result of
          Left err -> do
            liftEffect $ Console.log $ "Failed to load: " <> show err
            H.modify_ _ { loading = false, error = Just (show err) }
            pure { nodes: [], links: [] }

          Right json -> do
            liftEffect $ Console.log $ "Loaded dataset from " <> path
            let rawModel = unsafeCoerce json :: NetworkRawModel
            let m = processRawModel rawModel
            liftEffect $ Console.log $ "Processed: " <> show (length m.nodes) <> " nodes, " <> show (length m.links) <> " links"
            pure m

    when (length model.nodes > 0) do
      forceRef <- liftEffect $ initSimpleForce model "#force-playground-container"
      H.modify_ _ { loading = false, forceState = Just forceRef, enabledForces = allForces, activePreset = Just PresetStandard, useLinkWeights = false, shownCategories = allCategories }

-- | Get path for JSON datasets
datasetPath :: Dataset -> String
datasetPath = case _ of
  GeneratedGraph -> ""
  KarateClub -> "/data/karate-club.json"
  EcoStMarks -> "/data/eco-stmarks.json"
  EcoEverglades -> "/data/eco-everglades.json"
  CElegansFrontal -> "/data/c-elegans-frontal.json"

-- | Clear the container (remove existing SVG)
foreign import clearContainer :: String -> Effect Unit
