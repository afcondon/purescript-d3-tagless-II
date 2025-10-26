#!/usr/bin/env node

/**
 * Generate individual Halogen components for each library module
 * Each component will fetch and display its module's source code
 */

const fs = require('fs');
const path = require('path');

// Module registry (same as ModuleRegistry.purs)
const moduleCategories = [
  {
    title: "Core API",
    modules: [
      { name: "PSD3.Types", path: "PSD3/Types.purs", description: "Common type definitions used throughout PSD3" },
      { name: "PSD3.Attributes", path: "PSD3/Attributes.purs", description: "SVG and HTML attribute combinators" }
    ]
  },
  {
    title: "Capabilities",
    modules: [
      { name: "PSD3.Capabilities.Selection", path: "PSD3/Capabilities/Selection.purs", description: "Core selection and element manipulation operations" },
      { name: "PSD3.Capabilities.Simulation", path: "PSD3/Capabilities/Simulation.purs", description: "Force simulation capabilities" },
      { name: "PSD3.Capabilities.Sankey", path: "PSD3/Capabilities/Sankey.purs", description: "Sankey diagram layout capabilities" }
    ]
  },
  {
    title: "Interpreters",
    modules: [
      { name: "PSD3.Interpreter.D3", path: "PSD3/Interpreter/D3.purs", description: "D3.js interpreter - runs visualizations in the browser" },
      { name: "PSD3.Interpreter.String", path: "PSD3/Interpreter/String.purs", description: "String interpreter - generates code as text" },
      { name: "PSD3.Interpreter.MetaTree", path: "PSD3/Interpreter/MetaTree.purs", description: "MetaTree interpreter - produces AST representation" }
    ]
  },
  {
    title: "Data Structures",
    modules: [
      { name: "PSD3.Data.Tree", path: "PSD3/Data/Tree.purs", description: "Tree data structure for hierarchical layouts" },
      { name: "PSD3.Data.Node", path: "PSD3/Data/Node.purs", description: "Node types for tree structures" },
      { name: "PSD3.Data.Utility", path: "PSD3/Data/Utility.purs", description: "Utility functions for data manipulation" }
    ]
  },
  {
    title: "Internal - Selection",
    modules: [
      { name: "PSD3.Internal.Selection.Types", path: "PSD3/Internal/Selection/Types.purs", description: "Selection type definitions" },
      { name: "PSD3.Internal.Selection.Functions", path: "PSD3/Internal/Selection/Functions.purs", description: "Selection implementation functions" }
    ]
  },
  {
    title: "Internal - Simulation",
    modules: [
      { name: "PSD3.Internal.Simulation.Types", path: "PSD3/Internal/Simulation/Types.purs", description: "Simulation type definitions" },
      { name: "PSD3.Internal.Simulation.Functions", path: "PSD3/Internal/Simulation/Functions.purs", description: "Simulation implementation functions" },
      { name: "PSD3.Internal.Simulation.Forces", path: "PSD3/Internal/Simulation/Forces.purs", description: "Force definitions for simulations" },
      { name: "PSD3.Internal.Simulation.Config", path: "PSD3/Internal/Simulation/Config.purs", description: "Simulation configuration" }
    ]
  },
  {
    title: "Internal - Other",
    modules: [
      { name: "PSD3.Internal.Types", path: "PSD3/Internal/Types.purs", description: "Internal type definitions" },
      { name: "PSD3.Internal.FFI", path: "PSD3/Internal/FFI.purs", description: "Foreign function interface to D3.js" },
      { name: "PSD3.Internal.Attributes.Instances", path: "PSD3/Internal/Attributes/Instances.purs", description: "Type class instances for attributes" },
      { name: "PSD3.Internal.Attributes.Sugar", path: "PSD3/Internal/Attributes/Sugar.purs", description: "Syntactic sugar for attributes" },
      { name: "PSD3.Internal.Axes", path: "PSD3/Internal/Axes.purs", description: "Axis generation" },
      { name: "PSD3.Internal.Hierarchical", path: "PSD3/Internal/Hierarchical.purs", description: "Hierarchical layout algorithms" },
      { name: "PSD3.Internal.Generators.Line", path: "PSD3/Internal/Generators/Line.purs", description: "Line and area generators" },
      { name: "PSD3.Internal.Scales.Scales", path: "PSD3/Internal/Scales/Scales.purs", description: "Scale functions" },
      { name: "PSD3.Internal.Scales.Linear", path: "PSD3/Internal/Scales/Linear.purs", description: "Linear scale implementation" },
      { name: "PSD3.Internal.Sankey.Types", path: "PSD3/Internal/Sankey/Types.purs", description: "Sankey diagram types" },
      { name: "PSD3.Internal.Sankey.Functions", path: "PSD3/Internal/Sankey/Functions.purs", description: "Sankey diagram functions" },
      { name: "PSD3.Internal.Utility", path: "PSD3/Internal/Utility.purs", description: "Internal utility functions" },
      { name: "PSD3.Internal.Zoom", path: "PSD3/Internal/Zoom.purs", description: "Zoom and pan behavior" }
    ]
  }
];

// Generate component template
function generateComponent(moduleInfo) {
  // Convert PSD3.Types -> Types, PSD3.Capabilities.Selection -> Capabilities.Selection
  const moduleNameSuffix = moduleInfo.name.replace(/^PSD3\./, '');
  const moduleName = `PSD3.Reference.Modules.${moduleNameSuffix}`;

  return `module ${moduleName} where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.PrismJS (highlightAll)
import PSD3.Reference.SourceLoader (loadSourceFile)

-- | State
type State =
  { sourceCode :: Maybe String
  , loading :: Boolean
  , error :: Maybe String
  }

-- | Actions
data Action
  = Initialize
  | SourceLoaded (Either String String)

-- | ${moduleInfo.name} module viewer
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \\_ ->
      { sourceCode: Nothing
      , loading: true
      , error: Nothing
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [ HH.ClassName "module-page" ] ]
    [ -- Module header
      HH.section
        [ HP.classes [ HH.ClassName "module-page__header" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "module-page__title" ] ]
            [ HH.text "${moduleInfo.name}" ]
        , HH.p
            [ HP.classes [ HH.ClassName "module-page__description" ] ]
            [ HH.text "${moduleInfo.description}" ]
        ]

    -- Custom content section (add explanations, diagrams, etc. here)
    , HH.section
        [ HP.classes [ HH.ClassName "module-page__content" ] ]
        [
          -- TODO: Add custom explanatory content here
          -- Examples: diagrams, usage examples, conceptual explanations
        ]

    -- Source code
    , HH.section
        [ HP.classes [ HH.ClassName "module-page__source" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "module-page__source-title" ] ]
            [ HH.text "Source Code" ]
        , case state.sourceCode of
            Nothing ->
              if state.loading
                then HH.div
                  [ HP.classes [ HH.ClassName "module-page__loading" ] ]
                  [ HH.text "Loading source code..." ]
                else case state.error of
                  Just err ->
                    HH.div
                      [ HP.classes [ HH.ClassName "module-page__error" ] ]
                      [ HH.text err ]
                  Nothing ->
                    HH.div
                      [ HP.classes [ HH.ClassName "module-page__error" ] ]
                      [ HH.text "Failed to load source code" ]
            Just code ->
              HH.div
                [ HP.classes [ HH.ClassName "module-page__code" ] ]
                [ HH.pre
                    [ HP.classes [ HH.ClassName "line-numbers" ] ]
                    [ HH.code
                        [ HP.classes [ HH.ClassName "language-haskell" ] ]
                        [ HH.text code ]
                    ]
                ]
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Load source file
    result <- H.liftAff $ loadSourceFile "${moduleInfo.path}"
    handleAction $ SourceLoaded result

  SourceLoaded result -> do
    case result of
      Left err -> do
        H.modify_ _ { loading = false, error = Just err }
      Right code -> do
        H.modify_ _ { sourceCode = Just code, loading = false, error = Nothing }
        liftEffect highlightAll
`;
}

// Generate directory structure for nested modules
function ensureDirectoryExists(filePath) {
  const dir = path.dirname(filePath);
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
}

// Main function
function main() {
  const outputDir = path.join(__dirname, '../src/website/Component/Reference/Modules');

  // Create output directory
  if (!fs.existsSync(outputDir)) {
    fs.mkdirSync(outputDir, { recursive: true });
  }

  let count = 0;

  // Generate components
  for (const category of moduleCategories) {
    for (const moduleInfo of category.modules) {
      // Convert PSD3.Types -> Types, PSD3.Capabilities.Selection -> Capabilities/Selection
      const moduleNameSuffix = moduleInfo.name.replace(/^PSD3\./, '');
      const filePath = moduleNameSuffix.replace(/\./g, '/');
      const pursFile = path.join(outputDir, `${filePath}.purs`);

      // Ensure directory structure exists
      ensureDirectoryExists(pursFile);

      // Write PureScript component (no JS file needed)
      fs.writeFileSync(pursFile, generateComponent(moduleInfo), 'utf8');

      count++;
      console.log(`✓ Generated ${moduleInfo.name}`);
    }
  }

  console.log(`\n✓ Generated ${count} module components in ${outputDir}`);
}

main();
