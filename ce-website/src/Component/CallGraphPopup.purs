-- | CallGraphPopup - Halogen component for displaying function call relationships
-- |
-- | Modal popup showing callers (left), source code (center), and callees (right)
-- | for a selected function declaration.
module Component.CallGraphPopup
  ( component
  , Query(..)
  , Output(..)
  , Input
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Loader as Loader

-- =============================================================================
-- Types
-- =============================================================================

-- | Component input (empty - popup is controlled via queries)
type Input = Unit

-- | Popup visibility and loading state
data PopupState
  = Hidden
  | Loading { moduleName :: String, declarationName :: String }
  | Loaded Loader.CallGraphData
  | Error { moduleName :: String, declarationName :: String, message :: String }

-- | Component state
type State =
  { popupState :: PopupState
  }

-- | Actions
data Action
  = Close
  | ClickOverlay
  | ClickCallerCallee String String -- moduleName, declarationName
  | NoOp

-- | Queries from parent
data Query a
  = ShowPopup String String a  -- moduleName, declarationName
  | HidePopup a

-- | Output messages to parent
data Output
  = PopupClosed
  | NavigateToFunction String String  -- moduleName, declarationName

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
        }
    }

initialState :: Input -> State
initialState _ =
  { popupState: Hidden
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.popupState of
  Hidden -> HH.text ""

  Loading { moduleName, declarationName } ->
    renderPopupContainer moduleName declarationName $
      [ renderColumn "Callers" [ renderLoading ]
      , renderSourceColumn Nothing Nothing Nothing declarationName
      , renderColumn "Calls" [ renderLoading ]
      ]

  Loaded callData ->
    renderPopupContainer callData.moduleName callData.declarationName $
      [ renderCallersColumn callData.callers
      , renderSourceColumn callData.sourceCode callData.declarationKind callData.gitMetrics callData.declarationName
      , renderCalleesColumn callData.callees
      ]

  Error { moduleName, declarationName, message } ->
    renderPopupContainer moduleName declarationName $
      [ renderColumn "Callers" [ renderError "Error loading callers" ]
      , renderColumn "Source Code" [ renderError message ]
      , renderColumn "Calls" [ renderError "Error loading callees" ]
      ]

-- | Main popup container with overlay
renderPopupContainer :: forall m. String -> String -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
renderPopupContainer moduleName declarationName content =
  HH.div
    [ HP.id "call-graph-popup"
    , HP.class_ (HH.ClassName "call-graph-popup")
    , HE.onClick \_ -> ClickOverlay
    ]
    [ HH.div
        [ HP.class_ (HH.ClassName "call-graph-content")
        , HE.onClick \_ -> NoOp  -- Stop propagation
        ]
        [ -- Header
          HH.div [ HP.class_ (HH.ClassName "call-graph-header") ]
            [ HH.h2 [ HP.id "call-graph-title" ]
                [ HH.text $ declarationName <> " (" <> moduleName <> ")" ]
            , HH.button
                [ HP.id "call-graph-close"
                , HP.class_ (HH.ClassName "call-graph-close")
                , HE.onClick \_ -> Close
                ]
                [ HH.text "×" ]
            ]
        -- Three columns
        , HH.div [ HP.class_ (HH.ClassName "call-graph-columns") ]
            content
        ]
    ]

-- | Render a generic column
renderColumn :: forall m. String -> Array (H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
renderColumn title content =
  HH.div [ HP.class_ (HH.ClassName "call-graph-column") ]
    [ HH.h3 [] [ HH.text title ]
    , HH.div [ HP.class_ (HH.ClassName "call-graph-list") ]
        content
    ]

-- | Render callers column
renderCallersColumn :: forall m. Array Loader.CallInfo -> H.ComponentHTML Action () m
renderCallersColumn callers =
  renderColumn "Callers" $
    if Array.null callers
      then [ renderEmpty "No callers found" ]
      else map renderCallItem callers

-- | Render callees column
renderCalleesColumn :: forall m. Array Loader.CallInfo -> H.ComponentHTML Action () m
renderCalleesColumn callees =
  renderColumn "Calls" $
    if Array.null callees
      then [ renderEmpty "No calls found" ]
      else map renderCallItem callees

-- | Render a single caller/callee item (clickable link)
renderCallItem :: forall m. Loader.CallInfo -> H.ComponentHTML Action () m
renderCallItem call =
  HH.div
    [ HP.classes [ HH.ClassName "call-graph-list-item", HH.ClassName "call-graph-link" ]
    , HE.onClick \_ -> ClickCallerCallee call.targetModule call.target
    ]
    [ HH.text call.target ]

-- | Render source code column with git metrics
renderSourceColumn :: forall m.
  Maybe String ->           -- source code
  Maybe String ->           -- declaration kind
  Maybe Loader.GitMetrics -> -- git metrics
  String ->                 -- declaration name (for fallback)
  H.ComponentHTML Action () m
renderSourceColumn mSource mKind mMetrics declName =
  HH.div [ HP.class_ (HH.ClassName "call-graph-column call-graph-source-column") ]
    [ HH.h3 [] [ HH.text "Source Code" ]
    , HH.div [ HP.id "call-graph-source-code" ]
        [ -- Git metrics (if available)
          case mMetrics of
            Just m -> renderGitMetrics m
            Nothing -> HH.text ""
        -- Source code or fallback
        , case mSource of
            Just src -> HH.pre [ HP.class_ (HH.ClassName "call-graph-source") ]
                          [ HH.text src ]
            Nothing ->
              let kindText = case mKind of
                    Just k -> k
                    Nothing -> "Declaration"
              in renderEmpty $ kindText <> ": " <> declName <> " (Source code not available)"
        ]
    ]

-- | Render git metrics section
renderGitMetrics :: forall m. Loader.GitMetrics -> H.ComponentHTML Action () m
renderGitMetrics m =
  HH.div [ HP.class_ (HH.ClassName "call-graph-git-metrics") ]
    [ HH.div [ HP.class_ (HH.ClassName "git-metric") ]
        [ HH.strong_ [ HH.text "Commits: " ]
        , HH.text $ show m.commitCount
        ]
    , HH.div [ HP.class_ (HH.ClassName "git-metric") ]
        [ HH.strong_ [ HH.text "Last modified: " ]
        , HH.text $ show m.daysSinceModified <> " days ago"
        ]
    , HH.div [ HP.class_ (HH.ClassName "git-metric") ]
        [ HH.strong_ [ HH.text "Authors: " ]
        , HH.text $ show m.authorCount
        ]
    , if Array.null m.authors then HH.text ""
      else HH.div [ HP.class_ (HH.ClassName "git-metric") ]
        [ HH.strong_ [ HH.text "Contributors: " ]
        , HH.text $ formatAuthors m.authors
        ]
    ]
  where
  formatAuthors authors =
    let shown = Array.take 3 authors
        suffix = if Array.length authors > 3 then "..." else ""
    in Array.intercalate ", " shown <> suffix

-- | Render loading indicator
renderLoading :: forall m. H.ComponentHTML Action () m
renderLoading =
  HH.div [ HP.class_ (HH.ClassName "call-graph-loading") ]
    [ HH.text "Loading..." ]

-- | Render empty state
renderEmpty :: forall m. String -> H.ComponentHTML Action () m
renderEmpty msg =
  HH.div [ HP.class_ (HH.ClassName "call-graph-empty") ]
    [ HH.text msg ]

-- | Render error state
renderError :: forall m. String -> H.ComponentHTML Action () m
renderError msg =
  HH.div [ HP.class_ (HH.ClassName "call-graph-error") ]
    [ HH.text msg ]

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Close -> do
    H.modify_ _ { popupState = Hidden }
    H.raise PopupClosed

  ClickOverlay -> do
    -- Close when clicking the overlay (outside the content)
    H.modify_ _ { popupState = Hidden }
    H.raise PopupClosed

  ClickCallerCallee moduleName declarationName -> do
    log $ "[CallGraphPopup] Navigating to: " <> moduleName <> "." <> declarationName
    -- Fetch new data for clicked function
    H.modify_ _ { popupState = Loading { moduleName, declarationName } }
    result <- H.liftAff $ Loader.fetchCallGraphData moduleName declarationName
    case result of
      Left err -> do
        log $ "[CallGraphPopup] Error fetching data: " <> err
        H.modify_ _ { popupState = Error { moduleName, declarationName, message: err } }
      Right callData -> do
        log $ "[CallGraphPopup] Loaded data for: " <> moduleName <> "." <> declarationName
        H.modify_ _ { popupState = Loaded callData }

  NoOp -> pure unit

-- =============================================================================
-- Queries
-- =============================================================================

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  ShowPopup moduleName declarationName a -> do
    log $ "[CallGraphPopup] ShowPopup query: " <> moduleName <> "." <> declarationName
    -- Show loading state immediately
    H.modify_ _ { popupState = Loading { moduleName, declarationName } }
    -- Fetch data
    result <- H.liftAff $ Loader.fetchCallGraphData moduleName declarationName
    case result of
      Left err -> do
        log $ "[CallGraphPopup] Error fetching data: " <> err
        H.modify_ _ { popupState = Error { moduleName, declarationName, message: err } }
      Right callData -> do
        log $ "[CallGraphPopup] Loaded data for: " <> moduleName <> "." <> declarationName
        H.modify_ _ { popupState = Loaded callData }
    pure (Just a)

  HidePopup a -> do
    H.modify_ _ { popupState = Hidden }
    pure (Just a)
