module PSD3.Wizard.Wizard where

import Prelude

import Data.Array (filter, head, uncons)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (trim)
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.Char (toCharCode, fromCharCode)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Wizard.Datasets (Dataset, Difficulty(..), allDatasets)
import PSD3.Wizard.Generator (GeneratorConfig, GeneratedFile, generateProject)
import PSD3.Wizard.FileDownload (downloadAsZip, copyFilesToClipboard)

-- | Wizard step
data WizardStep
  = ChooseDataset
  | NameModule
  | ReviewFiles
  | Download

derive instance eqWizardStep :: Eq WizardStep

-- | Component state
type State =
  { currentStep :: WizardStep
  , selectedDataset :: Maybe Dataset
  , vizName :: String
  , includeMain :: Boolean
  , generatedFiles :: Array GeneratedFile
  , filterDifficulty :: Maybe Difficulty
  }

-- | Component actions
data Action
  = SelectDataset Dataset
  | SetVizName String
  | ToggleIncludeMain
  | NextStep
  | PreviousStep
  | GoToStep WizardStep
  | GenerateFiles
  | DownloadZip
  | CopyToClipboard
  | SetDifficultyFilter (Maybe Difficulty)

-- | Component definition
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }

initialState :: State
initialState =
  { currentStep: ChooseDataset
  , selectedDataset: Nothing
  , vizName: ""
  , includeMain: true
  , generatedFiles: []
  , filterDifficulty: Nothing
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "wizard" ] ]
    [ renderHeader state
    , renderProgressBar state
    , renderStepContent state
    , renderNavigation state
    ]

-- | Render wizard header
renderHeader :: forall w. State -> HH.HTML w Action
renderHeader state =
  HH.header
    [ HP.classes [ HH.ClassName "wizard__header" ] ]
    [ HH.h1_ [ HH.text "PSD3 Visualization Wizard" ]
    , HH.p
        [ HP.classes [ HH.ClassName "wizard__subtitle" ] ]
        [ HH.text "Create a new PSD3 visualization project in 4 easy steps" ]
    ]

-- | Render progress bar
renderProgressBar :: forall w. State -> HH.HTML w Action
renderProgressBar state =
  HH.div
    [ HP.classes [ HH.ClassName "wizard__progress" ] ]
    [ renderStep ChooseDataset "1" "Choose Dataset"
    , renderStep NameModule "2" "Name Module"
    , renderStep ReviewFiles "3" "Review Files"
    , renderStep Download "4" "Download"
    ]
  where
  renderStep :: WizardStep -> String -> String -> HH.HTML w Action
  renderStep step num label =
    let
      isActive = state.currentStep == step
      isCompleted = stepIndex state.currentStep > stepIndex step
      classes = HH.ClassName $ "wizard__step"
        <> (if isActive then " wizard__step--active" else "")
        <>
          (if isCompleted then " wizard__step--completed" else "")
    in
      HH.div
        [ HP.classes [ classes ]
        , HE.onClick \_ -> GoToStep step
        ]
        [ HH.div [ HP.classes [ HH.ClassName "wizard__step-number" ] ] [ HH.text num ]
        , HH.div [ HP.classes [ HH.ClassName "wizard__step-label" ] ] [ HH.text label ]
        ]

  stepIndex :: WizardStep -> Int
  stepIndex ChooseDataset = 0
  stepIndex NameModule = 1
  stepIndex ReviewFiles = 2
  stepIndex Download = 3

-- | Render current step content
renderStepContent :: forall m. MonadAff m => State -> H.ComponentHTML Action () m
renderStepContent state =
  HH.div
    [ HP.classes [ HH.ClassName "wizard__content" ] ]
    [ case state.currentStep of
        ChooseDataset -> renderChooseDataset state
        NameModule -> renderNameModule state
        ReviewFiles -> renderReviewFiles state
        Download -> renderDownload state
    ]

-- | Step 1: Choose Dataset
renderChooseDataset :: forall w. State -> HH.HTML w Action
renderChooseDataset state =
  HH.div
    [ HP.classes [ HH.ClassName "wizard__step-content" ] ]
    [ HH.h2_ [ HH.text "Choose a Dataset" ]
    , HH.p_ [ HH.text "Select an example dataset to get started. Each dataset includes sample data and suggestions for visualizations." ]

    -- Difficulty filter
    , HH.div
        [ HP.classes [ HH.ClassName "wizard__filters" ] ]
        [ HH.label_ [ HH.text "Filter by difficulty:" ]
        , HH.button
            [ HP.classes [ HH.ClassName $ "filter-btn" <> if state.filterDifficulty == Nothing then " filter-btn--active" else "" ]
            , HE.onClick \_ -> SetDifficultyFilter Nothing
            ]
            [ HH.text "All" ]
        , HH.button
            [ HP.classes [ HH.ClassName $ "filter-btn" <> if state.filterDifficulty == Just Beginner then " filter-btn--active" else "" ]
            , HE.onClick \_ -> SetDifficultyFilter (Just Beginner)
            ]
            [ HH.text "Beginner" ]
        , HH.button
            [ HP.classes [ HH.ClassName $ "filter-btn" <> if state.filterDifficulty == Just Intermediate then " filter-btn--active" else "" ]
            , HE.onClick \_ -> SetDifficultyFilter (Just Intermediate)
            ]
            [ HH.text "Intermediate" ]
        ]

    -- Dataset grid
    , HH.div
        [ HP.classes [ HH.ClassName "wizard__dataset-grid" ] ]
        (map renderDatasetCard filteredDatasets)
    ]
  where
  filteredDatasets = case state.filterDifficulty of
    Nothing -> allDatasets
    Just difficulty -> filter (\d -> d.difficulty == difficulty) allDatasets

  renderDatasetCard :: Dataset -> HH.HTML w Action
  renderDatasetCard dataset =
    let
      isSelected = case state.selectedDataset of
        Just selected -> selected.id == dataset.id
        Nothing -> false
      cardClass = HH.ClassName $ "dataset-card" <> if isSelected then " dataset-card--selected" else ""
    in
      HH.div
        [ HP.classes [ cardClass ]
        , HE.onClick \_ -> SelectDataset dataset
        ]
        [ HH.div
            [ HP.classes [ HH.ClassName "dataset-card__header" ] ]
            [ HH.h3_ [ HH.text dataset.name ]
            , HH.span
                [ HP.classes [ HH.ClassName $ "difficulty-badge difficulty-badge--" <> toLowerCase (show dataset.difficulty) ] ]
                [ HH.text $ show dataset.difficulty ]
            ]
        , HH.p
            [ HP.classes [ HH.ClassName "dataset-card__description" ] ]
            [ HH.text dataset.description ]
        , HH.div
            [ HP.classes [ HH.ClassName "dataset-card__info" ] ]
            [ HH.strong_ [ HH.text "Fields: " ]
            , HH.text $ intercalate ", " (map (\f -> f.name <> ": " <> f.fieldType) dataset.fields)
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "dataset-card__viz" ] ]
            [ HH.strong_ [ HH.text "Suggested: " ]
            , HH.text dataset.suggestedViz
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "dataset-card__note" ] ]
            [ HH.text dataset.educationalNote ]
        ]

  toLowerCase :: String -> String
  toLowerCase = fromCharArray <<< map toLower <<< toCharArray
    where
    toLower c = if c >= 'A' && c <= 'Z' then fromMaybe c (fromCharCode (toCharCode c + 32)) else c

  intercalate :: String -> Array String -> String
  intercalate sep arr = maybe ""
    ( \{ head: first, tail: rest } ->
        if rest == [] then first else first <> sep <> intercalate sep rest
    )
    (uncons arr)

-- | Step 2: Name Module
renderNameModule :: forall w. State -> HH.HTML w Action
renderNameModule state =
  HH.div
    [ HP.classes [ HH.ClassName "wizard__step-content" ] ]
    [ HH.h2_ [ HH.text "Name Your Visualization" ]
    , HH.p_ [ HH.text "Choose a name for your visualization module. It should be a valid PureScript module name (CamelCase, starting with uppercase)." ]

    , HH.div
        [ HP.classes [ HH.ClassName "wizard__form-group" ] ]
        [ HH.label
            [ HP.for "viz-name" ]
            [ HH.text "Visualization Name" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.id "viz-name"
            , HP.value state.vizName
            , HP.placeholder "e.g., MyScatterPlot"
            , HE.onValueInput SetVizName
            , HP.classes [ HH.ClassName "wizard__input" ]
            ]
        , if isValidModuleName state.vizName || state.vizName == "" then HH.div_ []
          else HH.div
            [ HP.classes [ HH.ClassName "wizard__error" ] ]
            [ HH.text "Module name must start with uppercase and contain only letters and numbers" ]
        ]

    , HH.div
        [ HP.classes [ HH.ClassName "wizard__form-group" ] ]
        [ HH.label_
            [ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.checked state.includeMain
                , HE.onChecked \_ -> ToggleIncludeMain
                ]
            , HH.text " Include Main.purs (for bundling with spago)"
            ]
        , HH.p
            [ HP.classes [ HH.ClassName "wizard__help-text" ] ]
            [ HH.text "Main.purs provides an entry point for bundling your visualization. Check this if you plan to use 'spago bundle'." ]
        ]

    -- Preview
    , if state.vizName /= "" && isValidModuleName state.vizName then HH.div
        [ HP.classes [ HH.ClassName "wizard__preview" ] ]
        [ HH.h3_ [ HH.text "Files that will be generated:" ]
        , HH.ul_
            [ HH.li_ [ HH.text $ state.vizName <> "/Unsafe.purs" ]
            , HH.li_ [ HH.text $ state.vizName <> "/Model.purs" ]
            , HH.li_ [ HH.text $ state.vizName <> "/Draw.purs" ]
            , HH.li_ [ HH.text $ state.vizName <> "/index.html" ]
            , HH.li_ [ HH.text $ state.vizName <> "/README.md" ]
            , if state.includeMain then HH.li_ [ HH.text "Main.purs" ]
              else HH.text ""
            ]
        ]
      else HH.div_ []
    ]

-- | Step 3: Review Files
renderReviewFiles :: forall w. State -> HH.HTML w Action
renderReviewFiles state =
  HH.div
    [ HP.classes [ HH.ClassName "wizard__step-content" ] ]
    [ HH.h2_ [ HH.text "Review Generated Files" ]
    , HH.p_ [ HH.text "Preview the files that will be generated for your project." ]

    , HH.div
        [ HP.classes [ HH.ClassName "wizard__file-list" ] ]
        (map renderFile state.generatedFiles)
    ]
  where
  renderFile :: GeneratedFile -> HH.HTML w Action
  renderFile file =
    HH.details
      [ HP.classes [ HH.ClassName "wizard__file" ] ]
      [ HH.summary_ [ HH.text file.filename ]
      , HH.pre
          [ HP.classes [ HH.ClassName "wizard__code" ] ]
          [ HH.code_ [ HH.text file.content ] ]
      ]

-- | Step 4: Download
renderDownload :: forall w. State -> HH.HTML w Action
renderDownload state =
  HH.div
    [ HP.classes [ HH.ClassName "wizard__step-content wizard__step-content--centered" ] ]
    [ HH.h2_ [ HH.text "Download Your Project" ]
    , HH.p_ [ HH.text "Your visualization project is ready! Download it as a zip file or copy the code to your clipboard." ]

    , HH.div
        [ HP.classes [ HH.ClassName "wizard__download-options" ] ]
        [ HH.button
            [ HP.classes [ HH.ClassName "wizard__btn wizard__btn--primary wizard__btn--large" ]
            , HE.onClick \_ -> DownloadZip
            ]
            [ HH.text $ "📦 Download " <> state.vizName <> ".zip" ]
        , HH.button
            [ HP.classes [ HH.ClassName "wizard__btn wizard__btn--secondary wizard__btn--large" ]
            , HE.onClick \_ -> CopyToClipboard
            ]
            [ HH.text "📋 Copy to Clipboard" ]
        ]

    , HH.div
        [ HP.classes [ HH.ClassName "wizard__next-steps" ] ]
        [ HH.h3_ [ HH.text "Next Steps" ]
        , HH.ol_
            [ HH.li_ [ HH.text "Extract the zip file to your project directory" ]
            , HH.li_ [ HH.text "Add your dependencies to spago.dhall or package.json" ]
            , HH.li_ [ HH.text "Customize the visualization in Draw.purs" ]
            , HH.li_ [ HH.text "Build with 'spago build'" ]
            , HH.li_ [ HH.text "Open index.html in your browser" ]
            ]
        ]
    ]

-- | Render navigation buttons
renderNavigation :: forall w. State -> HH.HTML w Action
renderNavigation state =
  HH.div
    [ HP.classes [ HH.ClassName "wizard__navigation" ] ]
    [ HH.button
        [ HP.classes [ HH.ClassName "wizard__btn wizard__btn--secondary" ]
        , HE.onClick \_ -> PreviousStep
        , HP.disabled $ state.currentStep == ChooseDataset
        ]
        [ HH.text "← Previous" ]
    , HH.button
        [ HP.classes [ HH.ClassName "wizard__btn wizard__btn--primary" ]
        , HE.onClick \_ -> NextStep
        , HP.disabled $ not canProceed
        ]
        [ HH.text $ if state.currentStep == Download then "Restart" else "Next →" ]
    ]
  where
  canProceed = case state.currentStep of
    ChooseDataset -> isJust state.selectedDataset
    NameModule -> state.vizName /= "" && isValidModuleName state.vizName
    ReviewFiles -> true
    Download -> true

-- | Handle actions
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  SelectDataset dataset ->
    H.modify_ _ { selectedDataset = Just dataset }

  SetVizName name ->
    H.modify_ _ { vizName = trim name }

  ToggleIncludeMain ->
    H.modify_ \s -> s { includeMain = not s.includeMain }

  NextStep -> do
    state <- H.get
    case state.currentStep of
      ChooseDataset -> H.modify_ _ { currentStep = NameModule }
      NameModule -> do
        handleAction GenerateFiles
        H.modify_ _ { currentStep = ReviewFiles }
      ReviewFiles -> H.modify_ _ { currentStep = Download }
      Download -> H.modify_ _ { currentStep = ChooseDataset } -- Restart

  PreviousStep -> do
    state <- H.get
    case state.currentStep of
      NameModule -> H.modify_ _ { currentStep = ChooseDataset }
      ReviewFiles -> H.modify_ _ { currentStep = NameModule }
      Download -> H.modify_ _ { currentStep = ReviewFiles }
      ChooseDataset -> pure unit

  GoToStep step -> do
    state <- H.get
    -- Only allow going to steps that are accessible
    when (canGoToStep state step) $
      H.modify_ _ { currentStep = step }

  GenerateFiles -> do
    state <- H.get
    case state.selectedDataset of
      Just dataset -> do
        let
          config :: GeneratorConfig
          config =
            { vizName: state.vizName
            , dataset: dataset
            , includeMain: state.includeMain
            }
          files = generateProject config
        H.modify_ _ { generatedFiles = files }
      Nothing -> pure unit

  DownloadZip -> do
    state <- H.get
    H.liftAff $ downloadAsZip state.vizName state.generatedFiles

  CopyToClipboard -> do
    state <- H.get
    success <- liftEffect $ copyFilesToClipboard state.generatedFiles
    -- Could show a toast notification here
    pure unit

  SetDifficultyFilter difficulty ->
    H.modify_ _ { filterDifficulty = difficulty }

-- | Check if can proceed to a step
canGoToStep :: State -> WizardStep -> Boolean
canGoToStep state step = case step of
  ChooseDataset -> true
  NameModule -> isJust state.selectedDataset
  ReviewFiles -> isJust state.selectedDataset && state.vizName /= "" && isValidModuleName state.vizName
  Download -> isJust state.selectedDataset && state.vizName /= "" && isValidModuleName state.vizName

-- | Validate module name (must be CamelCase starting with uppercase)
isValidModuleName :: String -> Boolean
isValidModuleName name =
  let
    chars = toCharArray name
    isUpperCase c = let code = toCharCode c in code >= 65 && code <= 90 -- A-Z
    isLowerCase c = let code = toCharCode c in code >= 97 && code <= 122 -- a-z
    isDigitChar c = let code = toCharCode c in code >= 48 && code <= 57 -- 0-9
    isAlphaNum c = isUpperCase c || isLowerCase c || isDigitChar c
  in
    case chars of
      [] -> false
      _ -> fromMaybe false do
        first <- head chars
        pure $ isUpperCase first && all isAlphaNum chars
  where
  all :: forall a. (a -> Boolean) -> Array a -> Boolean
  all pred arr = case uncons arr of
    Nothing -> true
    Just { head: x, tail: xs } -> pred x && all pred xs
