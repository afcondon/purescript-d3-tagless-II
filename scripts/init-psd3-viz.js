#!/usr/bin/env node

/**
 * PSD3 Visualization Scaffold Generator
 *
 * Creates a properly structured PSD3 visualization with:
 * - Unsafe.purs (Datum_ coercion functions)
 * - Model.purs (data type definitions)
 * - Draw.purs (visualization code with datum_ accessor pattern)
 */

const fs = require('fs');
const path = require('path');
const readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

function question(query) {
  return new Promise(resolve => rl.question(query, resolve));
}

function capitalizeFirst(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

function parseDataFields(input) {
  // Parse "x:Number,y:Number,label:String" into array of {name, type}
  return input.split(',').map(field => {
    const [name, type] = field.trim().split(':');
    return { name: name.trim(), type: type.trim() };
  });
}

function generateRecordType(fields) {
  const fieldLines = fields.map(f => `    ${f.name} :: ${f.type}`);
  return fieldLines.join('\n  , ');
}

function generateAccessors(fields, coerceFnName) {
  const accessorLines = fields.map(f => `    ${f.name}: _.${f.name} <<< ${coerceFnName}`);
  return accessorLines.join('\n  , ');
}

function generateUnsafeModule(vizName, dataTypeName, fields) {
  const recordType = generateRecordType(fields);

  return `module ${vizName}.Unsafe where

import PSD3.Internal.Types (Datum_, Index_)
import Unsafe.Coerce (unsafeCoerce)

-- | Data type for this visualization
type ${dataTypeName} =
  { ${recordType}
  }

-- | Coerce Datum_ to ${dataTypeName}
-- This is safe because D3's data join ensures Datum_ contains
-- the data we originally passed to simpleJoin/updateJoin
coerceTo${dataTypeName} :: Datum_ -> ${dataTypeName}
coerceTo${dataTypeName} = unsafeCoerce

-- | Coerce Index_ to Int (always provided by D3)
coerceIndex :: Index_ -> Int
coerceIndex = unsafeCoerce
`;
}

function generateModelModule(vizName, dataTypeName, fields) {
  const recordType = generateRecordType(fields);

  return `module ${vizName}.Model where

-- | Data type for this visualization
-- Re-exported from Unsafe for use in type signatures
type ${dataTypeName} =
  { ${recordType}
  }

-- | Example data for testing
exampleData :: Array ${dataTypeName}
exampleData =
  [ -- Add your example data here
  ]
`;
}

function generateDrawModule(vizName, dataTypeName, fields, coerceFnName) {
  const accessors = generateAccessors(fields, coerceFnName);

  return `module ${vizName}.Draw where

import Prelude

import PSD3
import PSD3.Attributes as A
import PSD3.Internal.FFI (keyIsID_)
import ${vizName}.Unsafe (${coerceFnName}, coerceIndex)
import ${vizName}.Model (${dataTypeName})
import Data.Int (toNumber)
import Effect (Effect)

-- | Accessor record for working with bound data
datum_ ::
  { ${fields.map(f => `${f.name} :: Datum_ -> ${f.type}`).join('\n  , ')}
  , index :: Index_ -> Int
  }
datum_ =
  { ${accessors}
  , index: coerceIndex
  }

-- | Main drawing function
draw :: forall m. SelectionM D3Selection_ m => Array ${dataTypeName} -> Selector D3Selection_ -> m Unit
draw dataPoints selector = do
  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg
    [ A.width 800.0
    , A.height 600.0
    , A.viewBox 0.0 0.0 800.0 600.0
    ]

  -- Example: Simple data join
  circles <- simpleJoin svg Circle dataPoints keyIsID_
  setAttributes circles
    [ A.cx (\\(_ :: Datum_) (i :: Index_) -> toNumber (datum_.index i) * 50.0)
    , A.cy 300.0
    , A.radius 20.0
    , A.fill "steelblue"
    ]

  pure unit

-- | Entry point
run :: Array ${dataTypeName} -> Effect Unit
run dataPoints = eval_D3M $ draw dataPoints "#chart"
`;
}

function generateMainModule(vizName, dataTypeName) {
  return `module Main where

import Prelude
import Effect (Effect)
import ${vizName}.Draw (run)
import ${vizName}.Model (exampleData)

main :: Effect Unit
main = run exampleData
`;
}

async function main() {
  console.log('PSD3 Visualization Scaffold Generator');
  console.log('=====================================\n');

  const vizName = await question('Visualization module name (e.g., MyBarChart): ');
  if (!vizName || !/^[A-Z][a-zA-Z0-9]*$/.test(vizName)) {
    console.error('Error: Module name must start with uppercase letter and contain only letters/numbers');
    rl.close();
    return;
  }

  const dataFields = await question('Data record fields (e.g., x:Number,y:Number,label:String): ');
  if (!dataFields) {
    console.error('Error: Must provide at least one field');
    rl.close();
    return;
  }

  const outputDir = await question(`Output directory (default: src/viz/${vizName}): `) || `src/viz/${vizName}`;

  const includeMain = await question('Generate Main.purs? (y/n, default: n): ');

  rl.close();

  // Parse fields
  const fields = parseDataFields(dataFields);
  const dataTypeName = `${vizName}Data`;
  const coerceFnName = `coerceTo${dataTypeName}`;

  console.log('\nGenerating files...\n');

  // Create directory
  const fullOutputDir = path.join(process.cwd(), outputDir);
  fs.mkdirSync(fullOutputDir, { recursive: true });

  // Generate files
  const unsafeContent = generateUnsafeModule(vizName, dataTypeName, fields);
  const modelContent = generateModelModule(vizName, dataTypeName, fields);
  const drawContent = generateDrawModule(vizName, dataTypeName, fields, coerceFnName);

  fs.writeFileSync(path.join(fullOutputDir, 'Unsafe.purs'), unsafeContent);
  console.log(`✓ Created ${outputDir}/Unsafe.purs`);

  fs.writeFileSync(path.join(fullOutputDir, 'Model.purs'), modelContent);
  console.log(`✓ Created ${outputDir}/Model.purs`);

  fs.writeFileSync(path.join(fullOutputDir, 'Draw.purs'), drawContent);
  console.log(`✓ Created ${outputDir}/Draw.purs`);

  if (includeMain.toLowerCase() === 'y') {
    const mainContent = generateMainModule(vizName, dataTypeName);
    fs.writeFileSync(path.join(fullOutputDir, 'Main.purs'), mainContent);
    console.log(`✓ Created ${outputDir}/Main.purs`);
  }

  console.log('\nScaffold complete! Next steps:');
  console.log('1. Add your data to Model.purs exampleData');
  console.log('2. Customize the draw function in Draw.purs');
  console.log('3. Run: spago build');
  console.log(`4. Import and use: import ${vizName}.Draw (run)`);
}

main().catch(err => {
  console.error('Error:', err);
  process.exit(1);
});
