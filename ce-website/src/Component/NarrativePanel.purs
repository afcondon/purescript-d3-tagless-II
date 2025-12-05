-- | NarrativePanel - Halogen component for the "What Is Being Shown" panel
-- |
-- | Scene-aware panel that updates its content based on ViewState.
-- | Color key switches between package colors (overview) and declaration types (neighborhood).
module Component.NarrativePanel
  ( component
  , Query(..)
  , Output(..)
  , Input
  , ColorEntry
  , DeclarationKind
  , ProjectInfo
  , declarationKinds
  , defaultProjectName
  ) where

import Prelude

import Data.Array as Array
import Data.ColorPalette (PaletteType(..), PaletteConfig, LegendItem, getPalette)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints as String
import Data.String.Common (toLower)
import Data.String.Pattern (Pattern(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Types (Namespace(..), ElemName(..))
import Engine.ViewState (ViewState(..), Control, describe)

-- =============================================================================
-- Types
-- =============================================================================

-- | A color entry for the package palette
type ColorEntry = { name :: String, color :: String }

-- | Declaration kind with colors (DEPRECATED - use LegendItem from ColorPalette)
type DeclarationKind =
  { label :: String
  , color :: String
  , intense :: String
  }

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
  , dropdownOpen :: Maybe String -- Which control ID has dropdown open
  , projectDropdownOpen :: Boolean
  }

-- | Actions
data Action
  = HandleInput Input
  | ClickControl String -- Control ID clicked
  | SelectOption String String -- Control ID, selected value
  | CloseDropdown
  | ClickBack
  | ToggleProjectDropdown
  | SelectProject Int -- Project ID selected

-- | Queries from parent
data Query a
  = SetViewState ViewState a
  | SetHintText (Maybe String) a
  | SetPackagePalette (Array ColorEntry) a

-- | Output messages to parent
data Output
  = ControlChanged String String -- controlId, newValue
  | BackClicked
  | ProjectSelected Int -- Project ID selected

-- =============================================================================
-- Declaration Types (for inner scene color key)
-- =============================================================================

-- | Get declaration types from ColorPalette module
declarationKinds :: Array DeclarationKind
declarationKinds =
  let
    palette = getPalette DeclarationTypes
    toLegacyKind :: LegendItem -> DeclarationKind
    toLegacyKind item = { label: item.label, color: item.color, intense: item.color }
  in
    map toLegacyKind palette.legendItems

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
  , dropdownOpen: Nothing
  , projectDropdownOpen: false
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

    -- Hero text with TangleJS-style controls
    , renderHeroText state

    -- Hint text
    , renderHintText state.hintText

    -- Color key (scene-aware)
    , renderColorKey state

    -- Navigation section (back button + project selector)
    , renderNavigation state
    ]

-- | Render the main descriptive text with clickable controls
renderHeroText :: forall m. State -> H.ComponentHTML Action () m
renderHeroText state =
  let
    { text, controls } = describe state.viewState
    segments = parseTextWithControls text controls
  in
    HH.div [ HP.class_ (HH.ClassName "narrative-hero") ]
      (map (renderSegment state controls) segments)

-- | A segment is either plain text or a control
data TextSegment
  = PlainText String
  | ControlText String Control -- display text, control

-- | Parse text with [bracketed] controls
parseTextWithControls :: String -> Array Control -> Array TextSegment
parseTextWithControls text controls = go text []
  where
  go "" acc = Array.reverse acc
  go s acc =
    case findBracketedText s of
      Nothing -> Array.reverse (Array.cons (PlainText s) acc)
      Just { before, bracketed, after } ->
        let
          segment = case findControlForText bracketed controls of
            Just ctrl -> ControlText bracketed ctrl
            Nothing -> PlainText ("[" <> bracketed <> "]")
        in
          if before == "" then go after (Array.cons segment acc)
          else go after (Array.cons segment (Array.cons (PlainText before) acc))

  findControlForText :: String -> Array Control -> Maybe Control
  findControlForText inner ctrls =
    let lowerInner = toLower inner
    in Array.find (\c ->
         toLower c.current == lowerInner ||
         Array.any (\opt -> toLower opt == lowerInner) c.options
       ) ctrls

-- | Render a text segment
renderSegment :: forall m. State -> Array Control -> TextSegment -> H.ComponentHTML Action () m
renderSegment state _controls seg = case seg of
  PlainText s -> HH.text s
  ControlText display ctrl ->
    if Array.null ctrl.options then
      -- Non-interactive highlight
      HH.span [ HP.class_ (HH.ClassName "tangle-value") ]
        [ HH.text display ]
    else
      -- Interactive control
      HH.span
        [ HP.class_ (HH.ClassName "tangle-control")
        , HE.onClick \_ -> ClickControl ctrl.id
        ]
        [ HH.text display
        , if state.dropdownOpen == Just ctrl.id
          then renderDropdown ctrl
          else HH.text ""
        ]

-- | Render dropdown for a control
renderDropdown :: forall m. Control -> H.ComponentHTML Action () m
renderDropdown ctrl =
  HH.div [ HP.class_ (HH.ClassName "tangle-dropdown") ]
    ( map (\opt ->
        HH.div
          [ HP.classes $
              [ HH.ClassName "tangle-dropdown-item" ] <>
              (if opt == ctrl.current then [ HH.ClassName "tangle-dropdown-item--active" ] else [])
          , HE.onClick \_ -> SelectOption ctrl.id opt
          ]
          [ HH.text opt ]
      ) ctrl.options
    )

-- | Render hint text
renderHintText :: forall m. Maybe String -> H.ComponentHTML Action () m
renderHintText mHint =
  HH.div [ HP.class_ (HH.ClassName "narrative-hint") ]
    [ HH.text (case mHint of
        Just h -> h
        Nothing -> hintForView) ]
  where
  hintForView = "Click any module to explore its neighborhood."

-- | Render the color key based on current scene
renderColorKey :: forall m. State -> H.ComponentHTML Action () m
renderColorKey state =
  case state.viewState of
    Neighborhood _ -> renderDeclarationTypesKey
    FunctionCalls _ -> renderDeclarationTypesKey
    _ -> renderPackagesKey state.packagePalette

-- | Render package color key (for grid/orbit/tree views)
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

-- | Render declaration types color key (for neighborhood/function views)
renderDeclarationTypesKey :: forall m. H.ComponentHTML Action () m
renderDeclarationTypesKey =
  HH.div [ HP.class_ (HH.ClassName "narrative-color-key") ]
    [ HH.div [ HP.class_ (HH.ClassName "color-key-title") ]
        [ HH.text "Declaration Types" ]
    , HH.div [ HP.class_ (HH.ClassName "color-key-items") ]
        (map renderDeclarationEntry declarationKinds)
    ]

renderDeclarationEntry :: forall m. DeclarationKind -> H.ComponentHTML Action () m
renderDeclarationEntry { label, color, intense } =
  HH.div [ HP.class_ (HH.ClassName "color-key-item") ]
    [ -- SVG with nested circles showing category and declaration colors
      HH.elementNS svgNS (ElemName "svg")
        [ HP.attr (HH.AttrName "width") "24"
        , HP.attr (HH.AttrName "height") "24"
        , HP.attr (HH.AttrName "class") "color-key-circles"
        ]
        [ -- Outer circle (category color)
          HH.elementNS svgNS (ElemName "circle")
            [ HP.attr (HH.AttrName "cx") "12"
            , HP.attr (HH.AttrName "cy") "12"
            , HP.attr (HH.AttrName "r") "10"
            , HP.style ("fill: " <> color <> "; fill-opacity: 0.6;")
            ]
            []
        -- Inner circle (intense color)
        , HH.elementNS svgNS (ElemName "circle")
            [ HP.attr (HH.AttrName "cx") "12"
            , HP.attr (HH.AttrName "cy") "12"
            , HP.attr (HH.AttrName "r") "6"
            , HP.style ("fill: " <> intense <> "; fill-opacity: 0.9;")
            ]
            []
        ]
    , HH.span [ HP.class_ (HH.ClassName "color-key-label") ]
        [ HH.text label ]
    ]

-- | SVG namespace
svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

-- | Render navigation section (back button + project selector)
renderNavigation :: forall m. State -> H.ComponentHTML Action () m
renderNavigation state =
  HH.div [ HP.class_ (HH.ClassName "narrative-nav") ]
    [ renderBackButton state.viewState
    , renderProjectSelector state
    ]

-- | Render back button (always visible, disabled at top level)
renderBackButton :: forall m. ViewState -> H.ComponentHTML Action () m
renderBackButton vs = case vs of
  Neighborhood _ ->
    HH.button
      [ HP.class_ (HH.ClassName "btn-editorial narrative-back")
      , HE.onClick \_ -> ClickBack
      ]
      [ HH.text "← Back to overview" ]
  FunctionCalls _ ->
    HH.button
      [ HP.class_ (HH.ClassName "btn-editorial narrative-back")
      , HE.onClick \_ -> ClickBack
      ]
      [ HH.text "← Back to neighborhood" ]
  _ ->
    HH.button
      [ HP.classes [ HH.ClassName "btn-editorial", HH.ClassName "narrative-back", HH.ClassName "narrative-back--disabled" ]
      , HP.disabled true
      ]
      [ HH.text "At top level" ]

-- | Render project selector with dropdown
renderProjectSelector :: forall m. State -> H.ComponentHTML Action () m
renderProjectSelector state =
  HH.div
    [ HP.class_ (HH.ClassName "narrative-project-wrapper") ]
    [ HH.button
        [ HP.class_ (HH.ClassName "btn-editorial narrative-project")
        , HE.onClick \_ -> ToggleProjectDropdown
        ]
        [ HH.text state.projectName ]
    , if state.projectDropdownOpen
        then renderProjectDropdown state
        else HH.text ""
    ]

-- | Render project dropdown menu
renderProjectDropdown :: forall m. State -> H.ComponentHTML Action () m
renderProjectDropdown state =
  HH.div [ HP.class_ (HH.ClassName "project-dropdown") ]
    ( map (\p ->
        HH.div
          [ HP.classes $
              [ HH.ClassName "project-dropdown-item" ] <>
              (if p.id == state.projectId then [ HH.ClassName "project-dropdown-item--active" ] else [])
          , HE.onClick \_ -> SelectProject p.id
          ]
          [ HH.text p.name ]
      ) state.projects
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

  ClickControl ctrlId -> do
    state <- H.get
    H.modify_ _ { dropdownOpen = if state.dropdownOpen == Just ctrlId then Nothing else Just ctrlId }

  SelectOption ctrlId value -> do
    H.modify_ _ { dropdownOpen = Nothing }
    H.raise (ControlChanged ctrlId value)

  CloseDropdown ->
    H.modify_ _ { dropdownOpen = Nothing, projectDropdownOpen = false }

  ClickBack ->
    H.raise BackClicked

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

-- =============================================================================
-- String Helpers (pure PureScript using Data.String.CodePoints)
-- =============================================================================

-- | Find bracketed text: "[foo]" -> { before, bracketed, after }
findBracketedText :: String -> Maybe { before :: String, bracketed :: String, after :: String }
findBracketedText s =
  case String.indexOf (Pattern "[") s of
    Nothing -> Nothing
    Just startIdx ->
      case String.indexOf' (Pattern "]") (startIdx + 1) s of
        Nothing -> Nothing
        Just endIdx ->
          Just
            { before: String.take startIdx s
            , bracketed: String.take (endIdx - startIdx - 1) (String.drop (startIdx + 1) s)
            , after: String.drop (endIdx + 1) s
            }

-- | Helper to show ViewState for logging
showViewState :: ViewState -> String
showViewState (Treemap _) = "Treemap"
showViewState (TreeLayout _ _) = "TreeLayout"
showViewState (ForceLayout _ _) = "ForceLayout"
showViewState (PackageGrid _) = "PackageGrid"
showViewState (ModuleOrbit _) = "ModuleOrbit"
showViewState (DependencyTree _) = "DependencyTree"
showViewState (Neighborhood name) = "Neighborhood(" <> name <> ")"
showViewState (FunctionCalls name) = "FunctionCalls(" <> name <> ")"
