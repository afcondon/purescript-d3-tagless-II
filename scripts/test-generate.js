#!/usr/bin/env node

// Direct test of the generation functions
const fs = require('fs');
const path = require('path');

function parseDataFields(input) {
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

// Test data
const vizName = 'ParabolaTest';
const dataFields = 'x:Number,y:Number';
const outputDir = 'src/test/ParabolaTest';

const fields = parseDataFields(dataFields);
const dataTypeName = `${vizName}Data`;
const coerceFnName = `coerceTo${dataTypeName}`;

console.log('Generating files for', vizName);
console.log('Fields:', fields);
console.log('');

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

console.log('\nGenerated files:');
console.log('\n=== Unsafe.purs ===');
console.log(unsafeContent);
console.log('\n=== Model.purs ===');
console.log(modelContent);
console.log('\n=== Draw.purs ===');
console.log(drawContent);
