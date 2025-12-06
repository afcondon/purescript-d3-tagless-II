-- | NarrativePanel - Halogen component for the "What Is Being Shown" panel
-- |
-- | Four-icon navigation for overview views, with description text below.
-- | Color key switches between package colors (overview) and declaration types (detail).
module Component.NarrativePanel
  ( component
  , Query(..)
  , Output(..)
  , Input
  , ColorEntry
  , ProjectInfo
  , defaultProjectName
  ) where

import Prelude

import Data.Array as Array
import Data.ColorPalette (LegendItem, PaletteType(..), getPalette)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Types (Namespace(..), ElemName(..))
import Engine.ViewState (ViewState(..), OverviewView(..), DetailView(..), viewLabel, viewDescription, getBaseOverview)

-- =============================================================================
-- Types
-- =============================================================================

-- | A color entry for the package palette
type ColorEntry = { name :: String, color :: String }

-- | Project info for dropdown (subset of full Project type)
type ProjectInfo =
  { id :: Int
  , name :: String
  }

-- | Default project name
defaultProjectName :: String
defaultProjectName = "psd3"

-- | Component input
type Input =
  { viewState :: ViewState
  , packagePalette :: Array ColorEntry
  , projectName :: String
  , projectId :: Int
  , projects :: Array ProjectInfo
  }

-- | State
type State =
  { viewState :: ViewState
  , packagePalette :: Array ColorEntry
  , projectName :: String
  , projectId :: Int
  , projects :: Array ProjectInfo
  , hintText :: Maybe String
  , projectDropdownOpen :: Boolean
  , originView :: Maybe OverviewView  -- Where we came from (for back button label)
  }

-- | Actions
data Action
  = HandleInput Input
  | SelectView OverviewView  -- Click on an icon to change view
  | GoBack                   -- Click the back button in detail view
  | ToggleProjectDropdown
  | SelectProject Int -- Project ID selected

-- | Queries from parent
data Query a
  = SetViewState ViewState a
  | SetHintText (Maybe String) a
  | SetPackagePalette (Array ColorEntry) a
  | SetOriginView OverviewView a  -- Set the origin view for back button label

-- | Output messages to parent
data Output
  = ViewSelected OverviewView  -- User clicked an icon
  | BackRequested              -- User clicked the back button
  | ProjectSelected Int -- Project ID selected

-- =============================================================================
-- Component
-- =============================================================================

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< HandleInput
        }
    }

initialState :: Input -> State
initialState input =
  { viewState: input.viewState
  , packagePalette: input.packagePalette
  , projectName: input.projectName
  , projectId: input.projectId
  , projects: input.projects
  , hintText: Nothing
  , projectDropdownOpen: false
  , originView: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.id "narrative-panel"
    , HP.classes [ HH.ClassName "floating-panel", HH.ClassName "floating-panel--top-left", HH.ClassName "narrative-panel" ]
    ]
    [ -- Title
      HH.div [ HP.class_ (HH.ClassName "panel-title") ]
        [ HH.text "What Is Being Shown" ]

    -- Four view icons
    , renderViewIcons state

    -- Description text
    , renderDescription state.viewState

    -- Hint text
    , renderHintText state.hintText state.viewState

    -- Color key (scene-aware)
    , renderColorKey state

    -- Project selector
    , renderProjectSelector state
    ]

-- | Render view icons OR back button depending on view state
renderViewIcons :: forall m. State -> H.ComponentHTML Action () m
renderViewIcons state =
  case state.viewState of
    Detail _ -> renderBackButton state.originView
    _ ->
      let
        currentOverview = getBaseOverview state.viewState
        allViews = [ TreemapView, TreeView, ForceView, TopoView ]
      in
        HH.div [ HP.class_ (HH.ClassName "view-icons") ]
          (map (renderViewIcon currentOverview) allViews)

-- | Render back button for detail views
renderBackButton :: forall m. Maybe OverviewView -> H.ComponentHTML Action () m
renderBackButton mOrigin =
  let
    originLabel = case mOrigin of
      Just ov -> viewLabel ov
      Nothing -> "Back"
  in
    HH.div [ HP.class_ (HH.ClassName "view-icons view-icons--back") ]
      [ HH.button
          [ HP.class_ (HH.ClassName "back-button")
          , HP.title ("Back to " <> originLabel)
          , HE.onClick \_ -> GoBack
          ]
          [ HH.elementNS svgNS (ElemName "svg")
              [ HP.attr (HH.AttrName "viewBox") "0 0 24 24"
              , HP.attr (HH.AttrName "width") "20"
              , HP.attr (HH.AttrName "height") "20"
              ]
              [ HH.elementNS svgNS (ElemName "path")
                  [ HP.attr (HH.AttrName "d") backArrowIcon
                  , HP.attr (HH.AttrName "fill") "currentColor"
                  ]
                  []
              ]
          , HH.span [ HP.class_ (HH.ClassName "back-label") ]
              [ HH.text ("← " <> originLabel) ]
          ]
      ]

-- | SVG path for back arrow icon
backArrowIcon :: String
backArrowIcon = "M20 11H7.83l5.59-5.59L12 4l-8 8 8 8 1.41-1.41L7.83 13H20v-2z"

-- | Render a single view icon
renderViewIcon :: forall m. OverviewView -> OverviewView -> H.ComponentHTML Action () m
renderViewIcon currentView thisView =
  let
    isActive = currentView == thisView
    label = viewLabel thisView
    iconPath = case thisView of
      TreemapView -> gridIcon
      TreeView -> treeIcon
      ForceView -> radialIcon
      TopoView -> packagesIcon
  in
    HH.button
      [ HP.classes $ [ HH.ClassName "view-icon" ] <>
          if isActive then [ HH.ClassName "view-icon--active" ] else []
      , HP.title label
      , HE.onClick \_ -> SelectView thisView
      ]
      [ HH.elementNS svgNS (ElemName "svg")
          [ HP.attr (HH.AttrName "viewBox") "0 0 24 24"
          , HP.attr (HH.AttrName "width") "24"
          , HP.attr (HH.AttrName "height") "24"
          ]
          [ HH.elementNS svgNS (ElemName "path")
              [ HP.attr (HH.AttrName "d") iconPath
              , HP.attr (HH.AttrName "fill") "currentColor"
              ]
              []
          ]
      ]

-- | SVG path for grid/treemap icon
gridIcon :: String
gridIcon = "M3 3h8v8H3V3zm0 10h8v8H3v-8zm10-10h8v8h-8V3zm0 10h8v8h-8v-8z"

-- | SVG path for tree icon
treeIcon :: String
treeIcon = "M12 2L4 7v3h3v12h10V10h3V7l-8-5zm0 2.5L17 8v1H7V8l5-3.5zM9 12h6v8H9v-8z"

-- | SVG path for radial icon
radialIcon :: String
radialIcon = "M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8zm0-14c-3.31 0-6 2.69-6 6s2.69 6 6 6 6-2.69 6-6-2.69-6-6-6zm0 10c-2.21 0-4-1.79-4-4s1.79-4 4-4 4 1.79 4 4-1.79 4-4 4zm0-6c-1.1 0-2 .9-2 2s.9 2 2 2 2-.9 2-2-.9-2-2-2z"

-- | SVG path for packages/topo icon
packagesIcon :: String
packagesIcon = "M20 3H4c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h16c1.1 0 2-.9 2-2V5c0-1.1-.9-2-2-2zm0 16H4V5h16v14zM6 7h5v5H6V7zm7 0h5v2h-5V7zm0 4h5v2h-5v-2zm0 4h5v2h-5v-2zm-7 0h5v2H6v-2z"

-- | Render description text for current view
renderDescription :: forall m. ViewState -> H.ComponentHTML Action () m
renderDescription viewState =
  HH.div [ HP.class_ (HH.ClassName "view-description") ]
    [ HH.text (viewDescription viewState) ]

-- | Render hint text
renderHintText :: forall m. Maybe String -> ViewState -> H.ComponentHTML Action () m
renderHintText mHint viewState =
  HH.div [ HP.class_ (HH.ClassName "narrative-hint") ]
    [ HH.text (case mHint of
        Just h -> h
        Nothing -> hintForView viewState)
    ]

-- | Default hint based on current view
hintForView :: ViewState -> String
hintForView (Overview TreemapView) = "Click any module to explore its neighborhood."
hintForView (Overview TreeView) = "Showing import hierarchy from Main."
hintForView (Overview ForceView) = "Drag nodes to rearrange. Click to explore."
hintForView (Overview TopoView) = "Package dependencies in topological order."
hintForView (Detail (NeighborhoodDetail _)) = "Click an icon above to return to overview."
hintForView (Detail (FunctionCallsDetail _)) = "Click an icon above to return to overview."

-- | Render the color key based on current view
renderColorKey :: forall m. State -> H.ComponentHTML Action () m
renderColorKey state =
  case state.viewState of
    Detail _ -> renderDeclarationTypesKey
    _ -> renderPackagesKey state.packagePalette

-- | Render package color key (for overview views)
renderPackagesKey :: forall m. Array ColorEntry -> H.ComponentHTML Action () m
renderPackagesKey palette =
  if Array.null palette then HH.text ""
  else
    HH.div [ HP.class_ (HH.ClassName "narrative-color-key") ]
      [ HH.div [ HP.class_ (HH.ClassName "color-key-title") ]
          [ HH.text "Packages" ]
      , HH.div [ HP.class_ (HH.ClassName "color-key-items") ]
          (map renderPackageEntry palette)
      ]

renderPackageEntry :: forall m. ColorEntry -> H.ComponentHTML Action () m
renderPackageEntry { name, color } =
  HH.div [ HP.class_ (HH.ClassName "color-key-item") ]
    [ HH.span
        [ HP.class_ (HH.ClassName "color-key-swatch")
        , HP.style ("background-color: " <> color)
        ]
        []
    , HH.span [ HP.class_ (HH.ClassName "color-key-label") ]
        [ HH.text name ]
    ]

-- | Render declaration types color key (for detail views)
renderDeclarationTypesKey :: forall m. H.ComponentHTML Action () m
renderDeclarationTypesKey =
  let
    palette = getPalette DeclarationTypes
  in
    HH.div [ HP.class_ (HH.ClassName "narrative-color-key") ]
      [ HH.div [ HP.class_ (HH.ClassName "color-key-title") ]
          [ HH.text "Declaration Types" ]
      , HH.div [ HP.class_ (HH.ClassName "color-key-items") ]
          (map renderLegendEntry palette.legendItems)
      ]

renderLegendEntry :: forall m. LegendItem -> H.ComponentHTML Action () m
renderLegendEntry { label, color } =
  HH.div [ HP.class_ (HH.ClassName "color-key-item") ]
    [ HH.elementNS svgNS (ElemName "svg")
        [ HP.attr (HH.AttrName "width") "24"
        , HP.attr (HH.AttrName "height") "24"
        , HP.attr (HH.AttrName "class") "color-key-circles"
        ]
        [ HH.elementNS svgNS (ElemName "circle")
            [ HP.attr (HH.AttrName "cx") "12"
            , HP.attr (HH.AttrName "cy") "12"
            , HP.attr (HH.AttrName "r") "10"
            , HP.style ("fill: " <> color <> "; fill-opacity: 0.6;")
            ]
            []
        , HH.elementNS svgNS (ElemName "circle")
            [ HP.attr (HH.AttrName "cx") "12"
            , HP.attr (HH.AttrName "cy") "12"
            , HP.attr (HH.AttrName "r") "6"
            , HP.style ("fill: " <> color <> "; fill-opacity: 0.9;")
            ]
            []
        ]
    , HH.span [ HP.class_ (HH.ClassName "color-key-label") ]
        [ HH.text label ]
    ]

-- | SVG namespace
svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

-- | Render project selector with dropdown
renderProjectSelector :: forall m. State -> H.ComponentHTML Action () m
renderProjectSelector state =
  HH.div [ HP.class_ (HH.ClassName "narrative-nav") ]
    [ HH.div
        [ HP.class_ (HH.ClassName "narrative-project-wrapper") ]
        [ HH.button
            [ HP.class_ (HH.ClassName "btn-editorial narrative-project")
            , HE.onClick \_ -> ToggleProjectDropdown
            ]
            [ HH.text state.projectName ]
        , if state.projectDropdownOpen then renderProjectDropdown state
          else HH.text ""
        ]
    ]

-- | Render project dropdown menu
renderProjectDropdown :: forall m. State -> H.ComponentHTML Action () m
renderProjectDropdown state =
  HH.div [ HP.class_ (HH.ClassName "project-dropdown") ]
    ( map
        ( \p ->
            HH.div
              [ HP.classes $
                  [ HH.ClassName "project-dropdown-item" ] <>
                    (if p.id == state.projectId then [ HH.ClassName "project-dropdown-item--active" ] else [])
              , HE.onClick \_ -> SelectProject p.id
              ]
              [ HH.text p.name ]
        )
        state.projects
    )

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  HandleInput input -> do
    H.modify_ _
      { viewState = input.viewState
      , packagePalette = input.packagePalette
      , projectName = input.projectName
      , projectId = input.projectId
      , projects = input.projects
      }

  SelectView newView -> do
    log $ "[NarrativePanel] View selected: " <> viewLabel newView
    H.raise (ViewSelected newView)

  GoBack -> do
    log "[NarrativePanel] Back button clicked"
    H.raise BackRequested

  ToggleProjectDropdown -> do
    state <- H.get
    H.modify_ _ { projectDropdownOpen = not state.projectDropdownOpen }

  SelectProject projectId -> do
    H.modify_ _ { projectDropdownOpen = false }
    H.raise (ProjectSelected projectId)

-- =============================================================================
-- Queries
-- =============================================================================

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  SetViewState vs a -> do
    log $ "[NarrativePanel] SetViewState received: " <> showViewState vs
    H.modify_ _ { viewState = vs }
    pure (Just a)

  SetHintText hint a -> do
    H.modify_ _ { hintText = hint }
    pure (Just a)

  SetPackagePalette palette a -> do
    H.modify_ _ { packagePalette = palette }
    pure (Just a)

  SetOriginView ov a -> do
    log $ "[NarrativePanel] SetOriginView: " <> viewLabel ov
    H.modify_ _ { originView = Just ov }
    pure (Just a)

-- | Helper to show ViewState for logging
showViewState :: ViewState -> String
showViewState (Overview TreemapView) = "Overview(Treemap)"
showViewState (Overview TreeView) = "Overview(Tree)"
showViewState (Overview ForceView) = "Overview(Force)"
showViewState (Overview TopoView) = "Overview(Topo)"
showViewState (Detail (NeighborhoodDetail name)) = "Detail(Neighborhood:" <> name <> ")"
showViewState (Detail (FunctionCallsDetail name)) = "Detail(FunctionCalls:" <> name <> ")"
