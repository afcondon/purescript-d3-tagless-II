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
    [ HP.class_ (HH.ClassName "explorer-app") ]
    [ -- Floating Control Panel (top-left)
      HH.div
        [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-left" ] ]
        [ HH.h2 [ HP.class_ (HH.ClassName "panel-title") ] [ HH.text "Code Explorer" ]

        -- Data Source section
        , HH.div [ HP.class_ (HH.ClassName "control-section") ]
            [ HH.div [ HP.class_ (HH.ClassName "section-label") ] [ HH.text "Data Source" ]
            , HH.div [ HP.class_ (HH.ClassName "control-row") ]
                [ renderProjectSelector state
                , renderSnapshotSelector state
                ]
            ]

        -- Layout section
        , HH.div [ HP.class_ (HH.ClassName "control-section") ]
            [ HH.div [ HP.class_ (HH.ClassName "section-label") ] [ HH.text "Arrange" ]
            , HH.div [ HP.class_ (HH.ClassName "control-row") ]
                [ HH.div [ HP.class_ (HH.ClassName "btn-group") ]
                    [ HH.button
                        [ HP.class_ (HH.ClassName "btn-editorial")
                        , HE.onClick \_ -> FormGrid
                        , HP.title "Arrange modules in a grid by size"
                        ]
                        [ HH.text "By Size" ]
                    , HH.button
                        [ HP.class_ (HH.ClassName "btn-editorial")
                        , HE.onClick \_ -> FormOrbit
                        , HP.title "Arrange modules in a circular ring"
                        ]
                        [ HH.text "Ring" ]
                    , HH.button
                        [ HP.class_ (HH.ClassName "btn-editorial")
                        , HE.onClick \_ -> FormTree
                        , HP.title "Arrange by import hierarchy"
                        ]
                        [ HH.text "Hierarchy" ]
                    ]
                ]
            ]

        -- Physics section
        , HH.div [ HP.class_ (HH.ClassName "control-section") ]
            [ HH.div [ HP.class_ (HH.ClassName "section-label") ] [ HH.text "Simulate" ]
            , HH.div [ HP.class_ (HH.ClassName "control-row") ]
                [ HH.div [ HP.class_ (HH.ClassName "btn-group") ]
                    [ HH.button
                        [ HP.class_ (HH.ClassName "btn-editorial")
                        , HE.onClick \_ -> GridRun
                        , HP.title "Cluster modules by dependencies"
                        ]
                        [ HH.text "Cluster" ]
                    , HH.button
                        [ HP.class_ (HH.ClassName "btn-editorial")
                        , HE.onClick \_ -> OrbitRun
                        , HP.title "Spread modules with physics"
                        ]
                        [ HH.text "Spread" ]
                    , HH.button
                        [ HP.class_ (HH.ClassName "btn-editorial")
                        , HE.onClick \_ -> TreeRun
                        , HP.title "Flow along import graph"
                        ]
                        [ HH.text "Flow" ]
                    ]
                ]
            ]
        ]

    -- Color Key Panel (top-right)
    , HH.div
        [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-right", HH.ClassName "legend-panel" ] ]
        [ HH.div [ HP.class_ (HH.ClassName "section-label") ] [ HH.text "Color = Package" ]
        , HH.div [ HP.class_ (HH.ClassName "info-text") ]
            [ HH.text "Modules are colored by their package. Similar colors indicate related code." ]
        , HH.div [ HP.class_ (HH.ClassName "section-label") ] [ HH.text "Size = Lines" ]
        , HH.div [ HP.class_ (HH.ClassName "info-text") ]
            [ HH.text "Circle size shows lines of code. Larger modules have more code." ]
        ]

    -- Error message
    , case state.error of
        Just err -> HH.div
          [ HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--bottom-left", HH.ClassName "error-message" ] ]
          [ HH.text err ]
        Nothing -> HH.text ""

    -- Visualization container
    , HH.div [ HP.id "viz" ] []
    ]

-- | Render project dropdown selector
renderProjectSelector :: forall m. State -> H.ComponentHTML Action () m
renderProjectSelector state =
  HH.select
    [ HP.class_ (HH.ClassName "select-editorial")
    , HE.onSelectedIndexChange handleProjectChange
    ]
    ( [ HH.option [ HP.disabled true, HP.selected (state.selectedProjectId == Nothing) ]
        [ HH.text (if state.loading then "Loading..." else "Project") ]
      ]
      <> map projectOption state.projects
    )
  where
  handleProjectChange :: Int -> Action
  handleProjectChange idx =
    case Array.index state.projects (idx - 1) of
      Just p -> SelectProject p.id
      Nothing -> LoadProjects

  projectOption :: Project -> H.ComponentHTML Action () m
  projectOption p = HH.option
    [ HP.selected (state.selectedProjectId == Just p.id) ]
    [ HH.text p.name ]

-- | Render snapshot dropdown selector
renderSnapshotSelector :: forall m. State -> H.ComponentHTML Action () m
renderSnapshotSelector state =
  case findSelectedProject state of
    Nothing -> HH.text ""
    Just project ->
      if Array.length project.snapshots == 0 then HH.text ""
      else
        HH.select
          [ HP.class_ (HH.ClassName "select-editorial")
          , HE.onSelectedIndexChange (handleSnapshotChange project.snapshots)
          ]
          ( [ HH.option [ HP.disabled true, HP.selected (state.selectedSnapshotId == Nothing) ]
              [ HH.text "Snapshot" ]
            ]
            <> map (snapshotOption state) project.snapshots
          )
  where
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
    in label

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
