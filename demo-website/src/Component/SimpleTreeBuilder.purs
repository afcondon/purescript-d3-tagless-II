module PSD3.SimpleTreeBuilder
  ( component
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import EmmetParser.Parser (parseEmmet)
import EmmetParser.Validator (validate)
import EmmetParser.Converter (convertToTree)
import EmmetParser.Types (ParseError, ValidationError)
import TreeBuilder3.TypePropagation (pointType, nodeType, countryType, letterType, boardType, rowType, cellType)
import TreeBuilder3.Types (DatumType(..), datumTypeLabel, datumTypeFields, PrimType(..))
import Data.Tree (Tree)
import TreeBuilder3.Types (TreeNode)
import PSD3.Shared.SiteNav as SiteNav

-- =============================================================================
-- State
-- =============================================================================

type State =
  { emmetInput :: String
  , parseResult :: Maybe (Either ParseError (Either ValidationError (Tree TreeNode)))
  , selectedType :: Maybe DatumType
  , exampleIndex :: Maybe Int
  }

-- | Available datum types
availableTypes :: Array DatumType
availableTypes = [ pointType, nodeType, countryType, letterType, boardType ]

-- | Example Emmet expressions
type Example =
  { name :: String
  , expr :: String
  , description :: String
  }

examples :: Array Example
examples =
  [ { name: "Simple Circle"
    , expr: "g>c"
    , description: "Group with a circle child"
    }
  , { name: "Scatter Plot"
    , expr: "j(Point)>c[cx:x,cy:y,r=5,fill=steelblue]"
    , description: "Join points with circle elements"
    }
  , { name: "Bubble Chart"
    , expr: "j(Country)>c[cx:gdp,cy:lifeExpectancy,r:population,fill=coral,opacity=0.6]"
    , description: "Countries with population-sized bubbles"
    }
  , { name: "Nested Grid"
    , expr: "n(Board)>g>n(Row)>g>n(Cell)>r[x@index,y@index,width=20,height=20]"
    , description: "2D board with nested rows and cells"
    }
  , { name: "Bar Chart"
    , expr: "j(Point)>r[x:x,y=0,width=40,height:y,fill=green]"
    , description: "Rectangles for bar chart"
    }
  , { name: "Circle with Multiple Attributes"
    , expr: "c[cx=100,cy=50,r=25,fill=red,stroke=black,stroke-width=2]"
    , description: "Circle with many static attributes"
    }
  ]

-- =============================================================================
-- Actions
-- =============================================================================

data Action
  = Initialize
  | UpdateInput String
  | SelectExample Int
  | SelectType DatumType
  | ClearInput

-- =============================================================================
-- Component
-- =============================================================================

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: forall i. i -> State
initialState _ =
  { emmetInput: ""
  , parseResult: Nothing
  , selectedType: Nothing
  , exampleIndex: Nothing
  }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "simple-tree-builder" ] ]
    [ -- Navigation
      SiteNav.render
        { logoSize: SiteNav.Normal
        , quadrant: SiteNav.NoQuadrant
        , prevNext: Nothing
        , pageTitle: Just "Simple Tree Builder"
        }

    , HH.div
        [ HP.classes [ HH.ClassName "builder-container" ] ]
        [ -- Left panel: Input and examples
          HH.div
            [ HP.classes [ HH.ClassName "builder-left-panel" ] ]
            [ renderInputSection state
            , renderExamplesSection state
            ]

        -- Right panel: Type selector and preview
        , HH.div
            [ HP.classes [ HH.ClassName "builder-right-panel" ] ]
            [ renderTypeSelector state
            , renderPreview state
            ]
        ]
    ]

renderInputSection :: forall m. State -> H.ComponentHTML Action () m
renderInputSection state =
  HH.div
    [ HP.classes [ HH.ClassName "input-section" ] ]
    [ HH.h2
        [ HP.classes [ HH.ClassName "section-title" ] ]
        [ HH.text "Emmet Expression" ]

    , HH.div
        [ HP.classes [ HH.ClassName "input-controls" ] ]
        [ HH.textarea
            [ HP.classes [ HH.ClassName "emmet-input" ]
            , HP.placeholder "Enter Emmet expression (e.g., g>c or j(Point)>c[cx:x,cy:y])"
            , HP.value state.emmetInput
            , HE.onValueInput UpdateInput
            , HP.rows 4
            ]
        , HH.button
            [ HP.classes [ HH.ClassName "clear-button" ]
            , HE.onClick \_ -> ClearInput
            , HP.disabled (String.null state.emmetInput)
            ]
            [ HH.text "Clear" ]
        ]

    , renderParseStatus state
    ]

renderParseStatus :: forall m. State -> H.ComponentHTML Action () m
renderParseStatus state =
  case state.parseResult of
    Nothing ->
      if String.null state.emmetInput
        then HH.div
          [ HP.classes [ HH.ClassName "parse-status info" ] ]
          [ HH.text "Enter an Emmet expression or select an example below" ]
        else HH.div_ []

    Just (Left parseError) ->
      HH.div
        [ HP.classes [ HH.ClassName "parse-status error" ] ]
        [ HH.strong_ [ HH.text "Parse Error: " ]
        , HH.text (show parseError)
        ]

    Just (Right (Left validationError)) ->
      HH.div
        [ HP.classes [ HH.ClassName "parse-status error" ] ]
        [ HH.strong_ [ HH.text "Validation Error: " ]
        , HH.text (show validationError)
        ]

    Just (Right (Right _tree)) ->
      HH.div
        [ HP.classes [ HH.ClassName "parse-status success" ] ]
        [ HH.text "✓ Valid expression" ]

renderExamplesSection :: forall m. State -> H.ComponentHTML Action () m
renderExamplesSection state =
  HH.div
    [ HP.classes [ HH.ClassName "examples-section" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "section-subtitle" ] ]
        [ HH.text "Examples" ]

    , HH.div
        [ HP.classes [ HH.ClassName "examples-grid" ] ]
        (Array.mapWithIndex renderExampleCard examples)
    ]
  where
    renderExampleCard :: Int -> Example -> H.ComponentHTML Action () m
    renderExampleCard idx example =
      HH.div
        [ HP.classes
            [ HH.ClassName "example-card"
            , HH.ClassName if state.exampleIndex == Just idx then "selected" else ""
            ]
        , HE.onClick \_ -> SelectExample idx
        ]
        [ HH.div
            [ HP.classes [ HH.ClassName "example-name" ] ]
            [ HH.text example.name ]
        , HH.div
            [ HP.classes [ HH.ClassName "example-description" ] ]
            [ HH.text example.description ]
        , HH.code
            [ HP.classes [ HH.ClassName "example-code" ] ]
            [ HH.text example.expr ]
        ]

renderTypeSelector :: forall m. State -> H.ComponentHTML Action () m
renderTypeSelector state =
  HH.div
    [ HP.classes [ HH.ClassName "type-selector-section" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "section-subtitle" ] ]
        [ HH.text "Available Types" ]

    , HH.div
        [ HP.classes [ HH.ClassName "types-grid" ] ]
        (map renderTypeCard availableTypes)
    ]
  where
    renderTypeCard :: DatumType -> H.ComponentHTML Action () m
    renderTypeCard dtype =
      let
        isSelected = state.selectedType == Just dtype
        typeName = datumTypeLabel dtype
        fields = datumTypeFields dtype
      in
        HH.div
          [ HP.classes
              [ HH.ClassName "type-card"
              , HH.ClassName if isSelected then "selected" else ""
              ]
          , HE.onClick \_ -> SelectType dtype
          ]
          [ HH.div
              [ HP.classes [ HH.ClassName "type-name" ] ]
              [ HH.text typeName ]
          , HH.div
              [ HP.classes [ HH.ClassName "type-fields" ] ]
              (map renderField fields)
          ]

    renderField :: { name :: String, typ :: PrimType } -> H.ComponentHTML Action () m
    renderField field =
      HH.div
        [ HP.classes [ HH.ClassName "type-field" ] ]
        [ HH.span
            [ HP.classes [ HH.ClassName "field-name" ] ]
            [ HH.text field.name ]
        , HH.span
            [ HP.classes [ HH.ClassName "field-type" ] ]
            [ HH.text (" : " <> primTypeLabel field.typ) ]
        ]

    primTypeLabel :: PrimType -> String
    primTypeLabel = case _ of
      TNumber -> "Number"
      TString -> "String"
      TInt -> "Int"
      TBoolean -> "Boolean"

renderPreview :: forall m. State -> H.ComponentHTML Action () m
renderPreview state =
  HH.div
    [ HP.classes [ HH.ClassName "preview-section" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "section-subtitle" ] ]
        [ HH.text "AST Preview" ]

    , case state.parseResult of
        Just (Right (Right tree)) ->
          HH.div
            [ HP.classes [ HH.ClassName "ast-preview" ] ]
            [ HH.pre
                [ HP.classes [ HH.ClassName "ast-tree" ] ]
                [ HH.text (showTree tree) ]
            ]

        _ ->
          HH.div
            [ HP.classes [ HH.ClassName "ast-preview empty" ] ]
            [ HH.text "Parse an expression to see the AST" ]
    ]

-- | Show tree structure (simplified for preview)
showTree :: Tree TreeNode -> String
showTree _tree = "Tree structure parsed successfully!\n(Detailed view coming soon)"

-- =============================================================================
-- Event Handling
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    pure unit

  UpdateInput input -> do
    H.modify_ _ { emmetInput = input, exampleIndex = Nothing }
    -- Parse the input
    if String.null input
      then H.modify_ _ { parseResult = Nothing }
      else do
        let parseResult = parseEmmet input
        case parseResult of
          Left err ->
            H.modify_ _ { parseResult = Just (Left err) }
          Right expr -> do
            let validationResult = validate expr
            case validationResult of
              Left err ->
                H.modify_ _ { parseResult = Just (Right (Left err)) }
              Right validExpr -> do
                let tree = convertToTree validExpr
                H.modify_ _ { parseResult = Just (Right (Right tree)) }

  SelectExample idx -> do
    case Array.index examples idx of
      Nothing -> pure unit
      Just example -> do
        H.modify_ _ { emmetInput = example.expr, exampleIndex = Just idx }
        handleAction (UpdateInput example.expr)

  SelectType dtype -> do
    H.modify_ _ { selectedType = Just dtype }

  ClearInput -> do
    H.modify_ _
      { emmetInput = ""
      , parseResult = Nothing
      , exampleIndex = Nothing
      }
