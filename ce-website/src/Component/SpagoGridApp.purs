-- | Halogen wrapper for Code Explorer
-- | Provides UI controls for scene transitions and project/snapshot selection
module Component.SpagoGridApp where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Engine.Explorer as Explorer
import Data.Loader (Project, Snapshot, fetchProjects, fetchProjectWithSnapshots)

-- | Component state
type State =
  { initialized :: Boolean
  , projects :: Array Project
  , selectedProjectId :: Maybe Int
  , selectedSnapshotId :: Maybe Int
  , loading :: Boolean
  , error :: Maybe String
  }

-- | Actions
data Action
  = Initialize
  | LoadProjects
  | SelectProject Int
  | SelectSnapshot Int
  | FormGrid    -- Transition to Grid layout (Static)
  | GridRun     -- Run physics in Grid layout
  | FormOrbit   -- Form orbit ring (Static)
  | OrbitRun    -- Run physics in Orbit layout
  | FormTree    -- Form tree positions (Static)
  | TreeRun     -- Run force-directed tree

-- | Main app component
component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { initialized: false
        , projects: []
        , selectedProjectId: Nothing
        , selectedSnapshotId: Nothing
        , loading: true
        , error: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "explorer-app")
    , HP.style "width: 100%; height: 100%; position: relative;"
    ]
    [ -- Control panel
      HH.div
        [ HP.style "position: absolute; top: 10px; left: 10px; z-index: 100; display: flex; gap: 10px; align-items: center; flex-wrap: wrap;"
        ]
        [ HH.h1
            [ HP.style "margin: 0; font-size: 18px; color: #333;" ]
            [ HH.text "Code Explorer" ]
        -- Project selector
        , renderProjectSelector state
        -- Snapshot selector
        , renderSnapshotSelector state
        -- Scene buttons
        , HH.button
            [ HP.style formButtonStyle
            , HE.onClick \_ -> FormGrid
            ]
            [ HH.text "Form Grid" ]
        , HH.button
            [ HP.style runButtonStyle
            , HE.onClick \_ -> GridRun
            ]
            [ HH.text "Grid Run" ]
        , HH.button
            [ HP.style formButtonStyle
            , HE.onClick \_ -> FormOrbit
            ]
            [ HH.text "Form Orbit" ]
        , HH.button
            [ HP.style runButtonStyle
            , HE.onClick \_ -> OrbitRun
            ]
            [ HH.text "Orbit Run" ]
        , HH.button
            [ HP.style formButtonStyle
            , HE.onClick \_ -> FormTree
            ]
            [ HH.text "Form Tree" ]
        , HH.button
            [ HP.style runButtonStyle
            , HE.onClick \_ -> TreeRun
            ]
            [ HH.text "Tree Run" ]
        ]
    -- Error message
    , case state.error of
        Just err -> HH.div
          [ HP.style "position: absolute; top: 60px; left: 10px; color: red; z-index: 100;" ]
          [ HH.text err ]
        Nothing -> HH.text ""
    -- Visualization container
    , HH.div [ HP.id "viz", HP.style "width: 100%; height: 100%;" ] []
    ]
  where
  formButtonStyle = "padding: 8px 16px; border: none; border-radius: 4px; background: #4a90d9; color: white; cursor: pointer; font-size: 14px;"
  runButtonStyle = "padding: 8px 16px; border: none; border-radius: 4px; background: #9b4ad9; color: white; cursor: pointer; font-size: 14px;"

-- | Render project dropdown selector
renderProjectSelector :: forall m. State -> H.ComponentHTML Action () m
renderProjectSelector state =
  HH.select
    [ HP.style selectStyle
    , HE.onSelectedIndexChange handleProjectChange
    ]
    ( [ HH.option [ HP.disabled true, HP.selected (state.selectedProjectId == Nothing) ]
        [ HH.text (if state.loading then "Loading..." else "Select Project") ]
      ]
      <> map projectOption state.projects
    )
  where
  selectStyle = "padding: 6px 12px; border: 1px solid #ccc; border-radius: 4px; font-size: 14px; min-width: 150px;"

  handleProjectChange :: Int -> Action
  handleProjectChange idx =
    -- idx 0 is the "Select Project" placeholder
    case Array.index state.projects (idx - 1) of
      Just p -> SelectProject p.id
      Nothing -> LoadProjects -- Fallback to reload

  projectOption :: Project -> H.ComponentHTML Action () m
  projectOption p = HH.option
    [ HP.selected (state.selectedProjectId == Just p.id) ]
    [ HH.text $ p.name <> " (" <> show p.snapshotCount <> " snapshots)" ]

-- | Render snapshot dropdown selector
renderSnapshotSelector :: forall m. State -> H.ComponentHTML Action () m
renderSnapshotSelector state =
  case findSelectedProject state of
    Nothing -> HH.text ""
    Just project ->
      if Array.length project.snapshots == 0 then HH.text ""
      else
        HH.select
          [ HP.style selectStyle
          , HE.onSelectedIndexChange (handleSnapshotChange project.snapshots)
          ]
          ( [ HH.option [ HP.disabled true, HP.selected (state.selectedSnapshotId == Nothing) ]
              [ HH.text "Select Snapshot" ]
            ]
            <> map (snapshotOption state) project.snapshots
          )
  where
  selectStyle = "padding: 6px 12px; border: 1px solid #ccc; border-radius: 4px; font-size: 14px; min-width: 200px;"

  handleSnapshotChange :: Array Snapshot -> Int -> Action
  handleSnapshotChange snapshots idx =
    case Array.index snapshots (idx - 1) of
      Just s -> SelectSnapshot s.id
      Nothing -> LoadProjects

  snapshotOption :: State -> Snapshot -> H.ComponentHTML Action () m
  snapshotOption st s = HH.option
    [ HP.selected (st.selectedSnapshotId == Just s.id) ]
    [ HH.text $ formatSnapshotLabel s ]

  formatSnapshotLabel :: Snapshot -> String
  formatSnapshotLabel s =
    let label = if s.label == "" then s.gitRef else s.label
    in label <> " - " <> show s.moduleCount <> " modules"

-- | Find the currently selected project
findSelectedProject :: State -> Maybe Project
findSelectedProject state =
  state.selectedProjectId >>= \pid ->
    Array.find (\p -> p.id == pid) state.projects

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    log "[Explorer] Initializing with new Scene Engine..."
    -- Initialize the explorer (loads data via legacy endpoints)
    liftEffect $ Explorer.initExplorer "#viz"
    H.modify_ _ { initialized = true }
    -- Load projects list for selector
    handleAction LoadProjects

  LoadProjects -> do
    log "[Explorer] Loading projects..."
    H.modify_ _ { loading = true, error = Nothing }
    result <- liftAff fetchProjects
    case result of
      Left err -> do
        log $ "[Explorer] Error loading projects: " <> err
        H.modify_ _ { loading = false, error = Just err }
      Right projects -> do
        log $ "[Explorer] Loaded " <> show (Array.length projects) <> " projects"
        -- Auto-select first project if available
        let firstProjectId = Array.head projects <#> _.id
        H.modify_ _
          { projects = projects
          , loading = false
          , selectedProjectId = firstProjectId
          }
        -- If we got a project, fetch its snapshots
        case firstProjectId of
          Just pid -> handleAction (SelectProject pid)
          Nothing -> pure unit

  SelectProject projectId -> do
    log $ "[Explorer] Selected project: " <> show projectId
    H.modify_ _ { selectedProjectId = Just projectId, selectedSnapshotId = Nothing, loading = true }
    -- Fetch project with snapshots
    result <- liftAff $ fetchProjectWithSnapshots projectId
    case result of
      Left err -> do
        log $ "[Explorer] Error loading project: " <> err
        H.modify_ _ { loading = false, error = Just err }
      Right project -> do
        -- Update the project in state with its snapshots
        H.modify_ \st -> st
          { loading = false
          , projects = updateProjectInList st.projects project
          }
        -- Auto-select latest snapshot
        case Array.head project.snapshots of
          Just s -> handleAction (SelectSnapshot s.id)
          Nothing -> pure unit

  SelectSnapshot snapshotId -> do
    log $ "[Explorer] Selected snapshot: " <> show snapshotId
    H.modify_ _ { selectedSnapshotId = Just snapshotId }
    -- TODO: Reload model for this snapshot
    -- For now, the legacy endpoints always return latest snapshot
    -- This will work once we add snapshot-specific data endpoints

  FormGrid -> goToScene "GridForm"
  GridRun -> goToScene "GridRun"
  FormOrbit -> goToScene "OrbitForm"
  OrbitRun -> goToScene "OrbitRun"
  FormTree -> goToScene "TreeForm"
  TreeRun -> goToScene "TreeRun"

-- | Update a project in the projects list
updateProjectInList :: Array Project -> Project -> Array Project
updateProjectInList projects updated =
  map (\p -> if p.id == updated.id then updated else p) projects

-- | Helper to transition to a scene by name
goToScene :: forall output m. MonadAff m => String -> H.HalogenM State Action () output m Unit
goToScene sceneName = do
  log $ "[Explorer] " <> sceneName <> " button clicked"
  liftEffect do
    mRef <- Ref.read Explorer.globalStateRef
    case mRef of
      Nothing -> log "[Explorer] Not ready yet"
      Just ref -> Explorer.goToScene sceneName ref
