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
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Types (Namespace(..), ElemName(..))
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import CodeExplorer.ViewState (ViewState(..), OverviewView(..), DetailView(..), NeighborhoodViewType(..), viewLabel, viewDescription, getBaseOverview, neighborhoodViewLabel)

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
  , moduleNames :: Array String -- All module names for search
  }

-- | State
type State =
  { viewState :: ViewState
  , packagePalette :: Array ColorEntry
  , projectName :: String
  , projectId :: Int
  , projects :: Array ProjectInfo
  , moduleNames :: Array String -- All module names for search
  , hintText :: Maybe String
  , projectDropdownOpen :: Boolean
  , originView :: Maybe OverviewView -- Where we came from (for back button label)
  , searchQuery :: String -- Current search input
  , searchResults :: Array String -- Filtered results
  , searchOpen :: Boolean -- Whether dropdown is shown
  , searchSelectedIndex :: Int -- Currently highlighted result (-1 = none)
  }

-- | Actions
data Action
  = HandleInput Input
  | SelectView OverviewView -- Click on an icon to change view
  | SelectNeighborhoodViewType NeighborhoodViewType -- Click on view type toggle in neighborhood
  | GoBack -- Click the back button in detail view
  | ToggleProjectDropdown
  | SelectProject Int -- Project ID selected
  | SearchInput String -- User typed in search box
  | SearchKeyDown KeyboardEvent -- User pressed a key in search box
  | SelectSearchResult String -- User clicked a search result
  | CloseSearch -- Close search dropdown

-- | Queries from parent
data Query a
  = SetViewState ViewState a
  | SetHintText (Maybe String) a
  | SetPackagePalette (Array ColorEntry) a
  | SetOriginView OverviewView a -- Set the origin view for back button label

-- | Output messages to parent
data Output
  = ViewSelected OverviewView -- User clicked an icon
  | NeighborhoodViewTypeSelected NeighborhoodViewType -- User clicked view type toggle
  | BackRequested -- User clicked the back button
  | ProjectSelected Int -- Project ID selected
  | ModuleSearchSelected String -- User selected a module from search

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
  , moduleNames: input.moduleNames
  , hintText: Nothing
  , projectDropdownOpen: false
  , originView: Nothing
  , searchQuery: ""
  , searchResults: []
  , searchOpen: false
  , searchSelectedIndex: -1
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

    -- Module search
    , renderModuleSearch state

    -- Description text
    , renderDescription state.viewState

    -- Hint text
    , renderHintText state.hintText state.viewState

    -- Color key (scene-aware)
    , renderColorKey state

    -- Project selector
    , renderProjectSelector state
    ]

-- | Render module search input with dropdown
-- | Supports keyboard navigation: ArrowUp/ArrowDown to navigate, Enter to select, Escape to close
renderModuleSearch :: forall m. State -> H.ComponentHTML Action () m
renderModuleSearch state =
  HH.div [ HP.class_ (HH.ClassName "module-search-wrapper") ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.class_ (HH.ClassName "module-search-input")
        , HP.placeholder "Search modules..."
        , HP.value state.searchQuery
        , HE.onValueInput SearchInput
        , HE.onKeyDown SearchKeyDown
        ]
    , if state.searchOpen && not (Array.null state.searchResults) then
        HH.div [ HP.class_ (HH.ClassName "module-search-dropdown") ]
          (Array.take 10 state.searchResults # Array.mapWithIndex renderSearchResult)
      else
        HH.text ""
    ]
  where
  renderSearchResult idx moduleName =
    let
      isSelected = idx == state.searchSelectedIndex
    in
      HH.div
        [ HP.classes $ [ HH.ClassName "module-search-result" ] <>
            if isSelected then [ HH.ClassName "module-search-result--selected" ] else []
        , HE.onClick \_ -> SelectSearchResult moduleName
        ]
        [ HH.text moduleName ]

-- | Render view icons OR back button depending on view state
-- | Neighborhood detail now uses triptych view only, so no view type toggles needed
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
    iconSrc = case thisView of
      TreemapView -> "icons/Treemap.jpeg"
      TreeView -> "icons/Tree.jpeg"
      ForceView -> "icons/Force.jpeg"
      TopoView -> "icons/Topograph.jpeg"
  in
    HH.button
      [ HP.classes $ [ HH.ClassName "view-icon" ] <>
          if isActive then [ HH.ClassName "view-icon--active" ] else []
      , HP.title label
      , HE.onClick \_ -> SelectView thisView
      ]
      [ HH.img
          [ HP.src iconSrc
          , HP.alt label
          , HP.width 50
          , HP.height 50
          , HP.class_ (HH.ClassName "view-icon-img")
          ]
      ]

-- | Render description text for current view
renderDescription :: forall m. ViewState -> H.ComponentHTML Action () m
renderDescription viewState =
  HH.div [ HP.class_ (HH.ClassName "view-description") ]
    [ HH.text (viewDescription viewState) ]

-- | Render hint text
renderHintText :: forall m. Maybe String -> ViewState -> H.ComponentHTML Action () m
renderHintText mHint viewState =
  HH.div [ HP.class_ (HH.ClassName "narrative-hint") ]
    [ HH.text
        ( case mHint of
            Just h -> h
            Nothing -> hintForView viewState
        )
    ]

-- | Default hint based on current view
hintForView :: ViewState -> String
hintForView (Overview TreemapView) = "Click any module to explore its neighborhood."
hintForView (Overview TreeView) = "Showing import hierarchy from Main."
hintForView (Overview ForceView) = "Drag nodes to rearrange. Click to explore."
hintForView (Overview TopoView) = "Package dependencies in topological order."
hintForView (Detail (NeighborhoodDetail _ _)) = "Click an icon above to return to overview."
hintForView (Detail (PackageNeighborhoodDetail _)) = "Click an icon above to return to overview."
hintForView (Detail (FunctionCallsDetail _)) = "Click an icon above to return to overview."

-- | Render the color key based on current view
renderColorKey :: forall m. State -> H.ComponentHTML Action () m
renderColorKey state =
  case state.viewState of
    Detail _ -> renderDeclarationTypesKey
    Overview TreemapView -> renderTreemapKey state.packagePalette
    Overview TreeView -> renderTreeForceKey state.packagePalette
    Overview ForceView -> renderTreeForceKey state.packagePalette
    Overview TopoView -> renderTopoKey state.packagePalette

-- | Render color key for Treemap view (packages colored, modules white)
renderTreemapKey :: forall m. Array ColorEntry -> H.ComponentHTML Action () m
renderTreemapKey palette =
  HH.div [ HP.class_ (HH.ClassName "narrative-color-key") ]
    [ HH.div [ HP.class_ (HH.ClassName "color-key-title") ]
        [ HH.text "Color Key" ]
    , HH.div [ HP.class_ (HH.ClassName "color-key-concept") ]
        [ renderConceptItem "Packages" (Array.take 4 palette)
        , renderConceptSwatch "Modules" "#ffffff" -- white
        ]
    ]

-- | Render color key for Tree/Force views (modules inherit package color)
renderTreeForceKey :: forall m. Array ColorEntry -> H.ComponentHTML Action () m
renderTreeForceKey palette =
  HH.div [ HP.class_ (HH.ClassName "narrative-color-key") ]
    [ HH.div [ HP.class_ (HH.ClassName "color-key-title") ]
        [ HH.text "Color Key" ]
    , HH.div [ HP.class_ (HH.ClassName "color-key-concept") ]
        [ HH.div [ HP.class_ (HH.ClassName "color-key-concept-row") ]
            [ HH.span [ HP.class_ (HH.ClassName "color-key-concept-label") ]
                [ HH.text "Modules inherit package color" ]
            ]
        , renderPackageSamples (Array.take 5 palette)
        ]
    ]

-- | Render color key for TopoGraph view (packages by topological layer)
renderTopoKey :: forall m. Array ColorEntry -> H.ComponentHTML Action () m
renderTopoKey _palette =
  HH.div [ HP.class_ (HH.ClassName "narrative-color-key") ]
    [ HH.div [ HP.class_ (HH.ClassName "color-key-title") ]
        [ HH.text "Color Key" ]
    , HH.div [ HP.class_ (HH.ClassName "color-key-concept") ]
        [ HH.div [ HP.class_ (HH.ClassName "color-key-concept-row") ]
            [ HH.span [ HP.class_ (HH.ClassName "color-key-concept-label") ]
                [ HH.text "Packages sorted by dependencies" ]
            ]
        , HH.div [ HP.class_ (HH.ClassName "color-key-concept-row") ]
            [ HH.span [ HP.class_ (HH.ClassName "color-key-concept-sublabel") ]
                [ HH.text "Left: no dependencies → Right: most dependencies" ]
            ]
        ]
    ]

-- | Render a concept item with label and sample swatches
renderConceptItem :: forall m. String -> Array ColorEntry -> H.ComponentHTML Action () m
renderConceptItem label samplePalette =
  HH.div [ HP.class_ (HH.ClassName "color-key-concept-row") ]
    [ HH.span [ HP.class_ (HH.ClassName "color-key-concept-label") ]
        [ HH.text label ]
    , HH.div [ HP.class_ (HH.ClassName "color-key-swatches") ]
        ( map
            ( \e -> HH.span
                [ HP.class_ (HH.ClassName "color-key-mini-swatch")
                , HP.style ("background-color: " <> e.color)
                , HP.title e.name
                ]
                []
            )
            samplePalette
        )
    ]

-- | Render a simple concept swatch with label
renderConceptSwatch :: forall m. String -> String -> H.ComponentHTML Action () m
renderConceptSwatch label color =
  HH.div [ HP.class_ (HH.ClassName "color-key-concept-row") ]
    [ HH.span [ HP.class_ (HH.ClassName "color-key-concept-label") ]
        [ HH.text label ]
    , HH.span
        [ HP.class_ (HH.ClassName "color-key-mini-swatch")
        , HP.style ("background-color: " <> color <> "; border: 1px solid #666;")
        ]
        []
    ]

-- | Render sample package swatches in a row
renderPackageSamples :: forall m. Array ColorEntry -> H.ComponentHTML Action () m
renderPackageSamples samples =
  HH.div [ HP.class_ (HH.ClassName "color-key-samples") ]
    ( map
        ( \e -> HH.span
            [ HP.class_ (HH.ClassName "color-key-sample")
            , HP.style ("background-color: " <> e.color)
            , HP.title e.name
            ]
            []
        )
        samples
    )

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
      , moduleNames = input.moduleNames
      }

  SelectView newView -> do
    log $ "[NarrativePanel] View selected: " <> viewLabel newView
    H.raise (ViewSelected newView)

  SelectNeighborhoodViewType viewType -> do
    log $ "[NarrativePanel] Neighborhood view type selected: " <> neighborhoodViewLabel viewType
    H.raise (NeighborhoodViewTypeSelected viewType)

  GoBack -> do
    log "[NarrativePanel] Back button clicked"
    H.raise BackRequested

  ToggleProjectDropdown -> do
    state <- H.get
    H.modify_ _ { projectDropdownOpen = not state.projectDropdownOpen }

  SelectProject projectId -> do
    H.modify_ _ { projectDropdownOpen = false }
    H.raise (ProjectSelected projectId)

  SearchInput query -> do
    state <- H.get
    let results = fuzzyMatch query state.moduleNames
    -- Reset selection when query changes
    H.modify_ _ { searchQuery = query, searchResults = results, searchOpen = query /= "", searchSelectedIndex = -1 }

  SearchKeyDown event -> do
    state <- H.get
    let keyName = KE.key event
    let visibleResults = Array.take 10 state.searchResults
    let maxIndex = Array.length visibleResults - 1
    case keyName of
      -- ArrowDown: Move selection down (wrap to -1 for no selection)
      "ArrowDown" -> do
        let
          newIndex =
            if state.searchSelectedIndex < maxIndex then state.searchSelectedIndex + 1
            else -1 -- Wrap to no selection
        H.modify_ _ { searchSelectedIndex = newIndex }
      -- ArrowUp: Move selection up (wrap to bottom)
      "ArrowUp" -> do
        let
          newIndex =
            if state.searchSelectedIndex > -1 then state.searchSelectedIndex - 1
            else maxIndex -- Wrap to bottom
        H.modify_ _ { searchSelectedIndex = newIndex }
      -- Enter: Select the highlighted item
      "Enter" -> do
        when (state.searchSelectedIndex >= 0) do
          case Array.index visibleResults state.searchSelectedIndex of
            Just moduleName -> handleAction (SelectSearchResult moduleName)
            Nothing -> pure unit
      -- Escape: Close the dropdown
      "Escape" -> do
        H.modify_ _ { searchOpen = false, searchSelectedIndex = -1 }
      -- Other keys: let them through for normal input handling
      _ -> pure unit

  SelectSearchResult moduleName -> do
    log $ "[NarrativePanel] Module search selected: " <> moduleName
    H.modify_ _ { searchQuery = "", searchResults = [], searchOpen = false, searchSelectedIndex = -1 }
    H.raise (ModuleSearchSelected moduleName)

  CloseSearch -> do
    H.modify_ _ { searchQuery = "", searchResults = [], searchOpen = false, searchSelectedIndex = -1 }

-- | Fuzzy match: case-insensitive substring match
fuzzyMatch :: String -> Array String -> Array String
fuzzyMatch query names =
  if query == "" then []
  else Array.filter (matchesQuery (String.toLower query)) names
  where
  matchesQuery q name = String.contains (Pattern q) (String.toLower name)

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
showViewState (Detail (NeighborhoodDetail name _)) = "Detail(Neighborhood:" <> name <> ")"
showViewState (Detail (PackageNeighborhoodDetail name)) = "Detail(PackageNeighborhood:" <> name <> ")"
showViewState (Detail (FunctionCallsDetail name)) = "Detail(FunctionCalls:" <> name <> ")"
