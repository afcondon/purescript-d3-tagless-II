-- | Force Playground Component
-- |
-- | Interactive force-directed graph visualization for exploring
-- | network datasets. Features both real-world datasets and
-- | procedurally generated graphs with rich attributes.
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
  GeneratedGraph -> "Generated Network (random)"
  KarateClub -> "Zachary's Karate Club (34 nodes)"
  EcoStMarks -> "St. Marks Food Web (54 nodes)"
  EcoEverglades -> "Everglades Food Web (69 nodes)"
  CElegansFrontal -> "C. elegans Neural Network (131 nodes)"

datasetDescription :: Dataset -> String
datasetDescription = case _ of
  GeneratedGraph -> "Procedurally generated network with 20-30 clusters, some connected. Node size/color varies by category and importance."
  KarateClub -> "Classic social network - friendships between members of a karate club that split into two factions (blue/orange)"
  EcoStMarks -> "Predator-prey relationships in St. Marks National Wildlife Refuge, Florida"
  EcoEverglades -> "Food web from the Florida Everglades ecosystem"
  CElegansFrontal -> "Synaptic connections between neurons in the frontal region of C. elegans"

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

render :: forall w. State -> HH.HTML w Action
render state =
  HH.div
    [ HP.classes [ HH.ClassName "force-playground-page" ]
    , HP.style pageStyle
    ]
    [ -- Header
      HH.div
        [ HP.style "margin-bottom: 20px; border-bottom: 1px solid #333; padding-bottom: 15px;" ]
        [ HH.h1
            [ HP.style "color: #69b3a2; margin: 0 0 10px 0; font-size: 24px;" ]
            [ HH.text "Force Simulation Playground" ]
        , HH.p
            [ HP.style "color: #aaa; margin: 0; font-size: 14px;" ]
            [ HH.text "Explore network datasets with force-directed graph visualization" ]
        ]

    -- Controls row
    , HH.div
        [ HP.style "margin-bottom: 20px; display: flex; gap: 20px; align-items: center; flex-wrap: wrap;" ]
        [ -- Dataset selector
          HH.div_
            [ HH.label [ HP.style "color: #888; margin-right: 8px;" ] [ HH.text "Dataset:" ]
            , HH.select
                [ HP.style selectStyle
                , HE.onValueChange (\v -> SelectDataset (parseDataset v))
                ]
                [ HH.option
                    [ HP.value "generated"
                    , HP.selected (state.selectedDataset == GeneratedGraph)
                    ]
                    [ HH.text (datasetLabel GeneratedGraph) ]
                , HH.option
                    [ HP.value "karate-club"
                    , HP.selected (state.selectedDataset == KarateClub)
                    ]
                    [ HH.text (datasetLabel KarateClub) ]
                , HH.option
                    [ HP.value "eco-stmarks"
                    , HP.selected (state.selectedDataset == EcoStMarks)
                    ]
                    [ HH.text (datasetLabel EcoStMarks) ]
                , HH.option
                    [ HP.value "eco-everglades"
                    , HP.selected (state.selectedDataset == EcoEverglades)
                    ]
                    [ HH.text (datasetLabel EcoEverglades) ]
                , HH.option
                    [ HP.value "c-elegans"
                    , HP.selected (state.selectedDataset == CElegansFrontal)
                    ]
                    [ HH.text (datasetLabel CElegansFrontal) ]
                ]
            ]
        -- Regenerate button (only for generated graphs)
        , if state.selectedDataset == GeneratedGraph
            then HH.button
              [ HP.style buttonStyle
              , HE.onClick \_ -> RegenerateGraph
              ]
              [ HH.text "Regenerate" ]
            else HH.text ""
        ]

    -- Force presets
    , HH.div
        [ HP.style "margin-bottom: 15px; display: flex; gap: 15px; align-items: center; flex-wrap: wrap;" ]
        [ HH.span [ HP.style "color: #888; font-size: 13px;" ] [ HH.text "Presets:" ]
        , presetButton state PresetStandard
        , presetButton state PresetClustered
        , presetButton state PresetMinimal
        ]

    -- Force toggles
    , HH.div
        [ HP.style "margin-bottom: 15px; display: flex; gap: 15px; align-items: center; flex-wrap: wrap;" ]
        [ HH.span [ HP.style "color: #888; font-size: 13px;" ] [ HH.text "Forces:" ]
        , forceToggle state ForceCharge "Charge" "Nodes repel each other"
        , forceToggle state ForceCollide "Collide" "Prevents node overlap"
        , forceToggle state ForceLink "Links" "Connected nodes attract"
        , forceToggle state ForceX "X Center" "Pull toward center X"
        , forceToggle state ForceY "Y Center" "Pull toward center Y"
        ]

    -- Link weights toggle (separate for visibility)
    , HH.div
        [ HP.style "margin-bottom: 15px; display: flex; gap: 15px; align-items: center;" ]
        [ HH.span [ HP.style "color: #888; font-size: 13px;" ] [ HH.text "Options:" ]
        , linkWeightsToggle state
        ]

    -- Category filters (only for generated graphs)
    , if state.selectedDataset == GeneratedGraph
        then HH.div
          [ HP.style "margin-bottom: 15px; display: flex; gap: 15px; align-items: center; flex-wrap: wrap;" ]
          [ HH.span [ HP.style "color: #888; font-size: 13px;" ] [ HH.text "Filter:" ]
          , categoryToggle state 0 "Research" "#1f77b4"
          , categoryToggle state 1 "Industry" "#ff7f0e"
          , categoryToggle state 2 "Government" "#2ca02c"
          , categoryToggle state 3 "Community" "#d62728"
          , showAllButton state
          ]
        else HH.text ""

    -- Dataset description
    , HH.p
        [ HP.style "color: #888; font-size: 13px; margin-bottom: 15px; font-style: italic;" ]
        [ HH.text (datasetDescription state.selectedDataset) ]

    -- Legend (for generated graphs)
    , if state.selectedDataset == GeneratedGraph
        then renderLegend
        else HH.text ""

    -- Loading indicator
    , if state.loading
        then HH.div
          [ HP.style "color: #69b3a2; padding: 20px;" ]
          [ HH.text "Loading dataset..." ]
        else HH.text ""

    -- Error message
    , case state.error of
        Just err -> HH.div
          [ HP.style "color: #ff6b6b; padding: 10px; background: #2a2a4e; border-radius: 4px; margin-bottom: 15px;" ]
          [ HH.text $ "Error: " <> err ]
        Nothing -> HH.text ""

    -- Visualization container
    , HH.div
        [ HP.id "force-playground-container"
        , HP.style "background: #1e1e3a; border-radius: 8px; min-height: 600px;"
        ]
        []

    -- Instructions
    , HH.div
        [ HP.style "margin-top: 15px; color: #666; font-size: 12px;" ]
        [ HH.text "Scroll to zoom, drag to pan. Toggle forces above to see how they affect the layout." ]
    ]

-- | Render a preset button
presetButton :: forall w. State -> ForcePreset -> HH.HTML w Action
presetButton state preset =
  let
    isActive = state.activePreset == Just preset
    style = if isActive
      then presetActiveStyle
      else presetInactiveStyle
  in
    HH.button
      [ HP.style style
      , HP.title (presetTooltip preset)
      , HE.onClick \_ -> ApplyPreset preset
      ]
      [ HH.text (presetLabel preset) ]

-- | Render a force toggle button
forceToggle :: forall w. State -> ForceId -> String -> String -> HH.HTML w Action
forceToggle state forceId label tooltip =
  let
    isEnabled = Set.member forceId state.enabledForces
    style = if isEnabled
      then toggleOnStyle
      else toggleOffStyle
  in
    HH.button
      [ HP.style style
      , HP.title tooltip
      , HE.onClick \_ -> ToggleForce forceId
      ]
      [ HH.text label ]

-- | Render link weights toggle
linkWeightsToggle :: forall w. State -> HH.HTML w Action
linkWeightsToggle state =
  let
    style = if state.useLinkWeights
      then optionOnStyle
      else optionOffStyle
  in
    HH.button
      [ HP.style style
      , HP.title "When enabled, links with higher weight pull more strongly"
      , HE.onClick \_ -> ToggleLinkWeights
      ]
      [ HH.text (if state.useLinkWeights then "Weighted Links: ON" else "Weighted Links: OFF") ]

-- | Render a category filter toggle with color indicator
categoryToggle :: forall w. State -> Int -> String -> String -> HH.HTML w Action
categoryToggle state catId label color =
  let
    isVisible = Set.member catId state.shownCategories
    bgStyle = if isVisible
      then "background: " <> color <> "; color: #fff; border: 2px solid " <> color <> ";"
      else "background: #333; color: #666; border: 2px solid #555;"
  in
    HH.button
      [ HP.style $ "padding: 6px 12px; font-size: 12px; border-radius: 4px; cursor: pointer; " <> bgStyle
      , HP.title $ "Toggle " <> label <> " nodes"
      , HE.onClick \_ -> ToggleCategory catId
      ]
      [ HH.text label ]

-- | Render "Show All" button
showAllButton :: forall w. State -> HH.HTML w Action
showAllButton state =
  let
    allShown = state.shownCategories == allCategories
    style = if allShown
      then "padding: 6px 12px; font-size: 12px; background: #555; color: #888; border: 2px solid #555; border-radius: 4px; cursor: default;"
      else "padding: 6px 12px; font-size: 12px; background: #69b3a2; color: #fff; border: 2px solid #69b3a2; border-radius: 4px; cursor: pointer;"
  in
    HH.button
      [ HP.style style
      , HP.title "Show all categories"
      , HE.onClick \_ -> ShowAllCategories
      ]
      [ HH.text "Show All" ]

-- | Render legend for generated graphs
renderLegend :: forall w i. HH.HTML w i
renderLegend =
  HH.div
    [ HP.style "display: flex; gap: 30px; margin-bottom: 15px; flex-wrap: wrap;" ]
    [ -- Node categories
      HH.div_
        [ HH.span [ HP.style "color: #888; font-size: 12px; margin-right: 10px;" ] [ HH.text "Categories:" ]
        , legendItem "#1f77b4" "Research"
        , legendItem "#ff7f0e" "Industry"
        , legendItem "#2ca02c" "Government"
        , legendItem "#d62728" "Community"
        ]
    -- Link types
    , HH.div_
        [ HH.span [ HP.style "color: #888; font-size: 12px; margin-right: 10px;" ] [ HH.text "Links:" ]
        , legendItem "#6baed6" "Collaboration"
        , legendItem "#fd8d3c" "Citation"
        , legendItem "#74c476" "Funding"
        , legendItem "#e377c2" "Communication"
        ]
    ]
  where
  legendItem color label =
    HH.span
      [ HP.style "margin-right: 12px; font-size: 11px;" ]
      [ HH.span
          [ HP.style $ "display: inline-block; width: 10px; height: 10px; border-radius: 50%; background: " <> color <> "; margin-right: 4px;" ]
          []
      , HH.span [ HP.style "color: #aaa;" ] [ HH.text label ]
      ]

pageStyle :: String
pageStyle = "padding: 20px; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #1a1a2e; color: #eee; min-height: 100vh;"

selectStyle :: String
selectStyle = "padding: 8px 12px; font-size: 14px; background: #2a2a4e; color: #fff; border: 1px solid #444; border-radius: 4px;"

buttonStyle :: String
buttonStyle = "padding: 8px 16px; font-size: 14px; background: #69b3a2; color: #fff; border: none; border-radius: 4px; cursor: pointer;"

toggleOnStyle :: String
toggleOnStyle = "padding: 6px 12px; font-size: 12px; background: #69b3a2; color: #fff; border: none; border-radius: 4px; cursor: pointer;"

toggleOffStyle :: String
toggleOffStyle = "padding: 6px 12px; font-size: 12px; background: #444; color: #888; border: none; border-radius: 4px; cursor: pointer;"

presetActiveStyle :: String
presetActiveStyle = "padding: 6px 14px; font-size: 12px; background: #5a7; color: #fff; border: 2px solid #7c9; border-radius: 4px; cursor: pointer; font-weight: bold;"

presetInactiveStyle :: String
presetInactiveStyle = "padding: 6px 14px; font-size: 12px; background: #3a3a5e; color: #aaa; border: 2px solid #555; border-radius: 4px; cursor: pointer;"

optionOnStyle :: String
optionOnStyle = "padding: 6px 14px; font-size: 12px; background: #7b6; color: #fff; border: 2px solid #9d8; border-radius: 4px; cursor: pointer; font-weight: bold;"

optionOffStyle :: String
optionOffStyle = "padding: 6px 14px; font-size: 12px; background: #444; color: #888; border: 2px solid #555; border-radius: 4px; cursor: pointer;"

parseDataset :: String -> Dataset
parseDataset = case _ of
  "generated" -> GeneratedGraph
  "karate-club" -> KarateClub
  "eco-everglades" -> EcoEverglades
  "c-elegans" -> CElegansFrontal
  _ -> EcoStMarks

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
        -- Update local state for UI
        let newForces = if newEnabled
              then Set.insert forceId state.enabledForces
              else Set.delete forceId state.enabledForces
        -- Clear active preset since user manually toggled
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
        -- Apply filter - convert Set to Array
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

    -- Clear existing visualization
    liftEffect $ clearContainer "#force-playground-container"

    model <- case state.selectedDataset of
      GeneratedGraph -> do
        -- Generate a new random graph
        liftEffect $ Console.log "Generating random network..."
        generated <- liftEffect $ Gen.generateGraph Gen.defaultConfig
        let m = fromGeneratedGraph generated
        liftEffect $ Console.log $ "Generated: " <> show (length m.nodes) <> " nodes, " <> show (length m.links) <> " links"
        pure m

      _ -> do
        -- Load from JSON file
        let path = datasetPath state.selectedDataset
        result <- liftAff $ loadJSON defaultConfig path

        case result of
          Left err -> do
            liftEffect $ Console.log $ "Failed to load: " <> show err
            H.modify_ _ { loading = false, error = Just (show err) }
            -- Return empty model on error
            pure { nodes: [], links: [] }

          Right json -> do
            liftEffect $ Console.log $ "Loaded dataset from " <> path
            let rawModel = unsafeCoerce json :: NetworkRawModel
            let m = processRawModel rawModel
            liftEffect $ Console.log $ "Processed: " <> show (length m.nodes) <> " nodes, " <> show (length m.links) <> " links"
            pure m

    -- Only initialize if we have nodes
    when (length model.nodes > 0) do
      forceRef <- liftEffect $ initSimpleForce model "#force-playground-container"
      -- Reset all forces to enabled for new graph (Standard preset, no weighted links, all categories)
      H.modify_ _ { loading = false, forceState = Just forceRef, enabledForces = allForces, activePreset = Just PresetStandard, useLinkWeights = false, shownCategories = allCategories }

-- | Get path for JSON datasets
datasetPath :: Dataset -> String
datasetPath = case _ of
  GeneratedGraph -> ""  -- Not used
  KarateClub -> "/data/karate-club.json"
  EcoStMarks -> "/data/eco-stmarks.json"
  EcoEverglades -> "/data/eco-everglades.json"
  CElegansFrontal -> "/data/c-elegans-frontal.json"

-- | Clear the container (remove existing SVG)
foreign import clearContainer :: String -> Effect Unit
