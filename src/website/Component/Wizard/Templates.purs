module PSD3.Wizard.Templates where

import Prelude

import Data.Array (intercalate, head)
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import PSD3.Wizard.Datasets (FieldDef)

-- | Generate the Unsafe.purs module
generateUnsafe :: String -> String -> Array FieldDef -> String
generateUnsafe vizName dataTypeName fields =
  let
    recordType = generateRecordType fields
  in
    """module """ <> vizName <> """.Unsafe where

import PSD3.Internal.Types (Datum_, Index_)
import Unsafe.Coerce (unsafeCoerce)

-- | Data type for this visualization
type """ <> dataTypeName <> """ =
  { """ <> recordType <> """
  }

-- | Coerce Datum_ to """ <> dataTypeName <> """
-- This is safe because D3's data join ensures Datum_ contains
-- the data we originally passed to simpleJoin/updateJoin
coerceTo""" <> dataTypeName <> """ :: Datum_ -> """ <> dataTypeName <> """
coerceTo""" <> dataTypeName <> """ = unsafeCoerce

-- | Coerce Index_ to Int (always provided by D3)
coerceIndex :: Index_ -> Int
coerceIndex = unsafeCoerce
"""

-- | Generate the Model.purs module
generateModel :: String -> String -> Array FieldDef -> String -> String
generateModel vizName dataTypeName fields exampleData =
  let
    recordType = generateRecordType fields
  in
    """module """ <> vizName <> """.Model where

-- | Data type for this visualization
-- Re-exported from Unsafe for use in type signatures
type """ <> dataTypeName <> """ =
  { """ <> recordType <> """
  }

-- | Example data for testing
exampleData :: Array """ <> dataTypeName <> """
exampleData =
  """ <> exampleData <> """
"""

-- | Generate the Draw.purs module
generateDraw :: String -> String -> Array FieldDef -> String -> String
generateDraw vizName dataTypeName fields coerceFnName =
  let
    accessors = generateAccessors fields coerceFnName
    datumType = generateDatumType fields
    firstField = fromMaybe "x" (map (\f -> f.name) (head fields))
  in
    """module """ <> vizName <> """.Draw where

import Prelude

import PSD3
import PSD3.Attributes as A
import PSD3.Internal.FFI (keyIsID_)
import """ <> vizName <> """.Unsafe (""" <> coerceFnName <> """, coerceIndex)
import """ <> vizName <> """.Model (""" <> dataTypeName <> """)
import Data.Int (toNumber)
import Effect (Effect)

-- | Accessor record for working with bound data
datum_ ::
  { """ <> datumType <> """
  , index :: Index_ -> Int
  }
datum_ =
  { """ <> accessors <> """
  , index: coerceIndex
  }

-- | Main drawing function
draw :: forall m. SelectionM D3Selection_ m => Array """ <> dataTypeName <> """ -> Selector D3Selection_ -> m Unit
draw dataPoints selector = do
  (root :: D3Selection_) <- attach selector
  svg <- appendTo root Svg
    [ A.width 800.0
    , A.height 600.0
    , A.viewBox 0.0 0.0 800.0 600.0
    ]

  -- Example: Simple data join
  -- Replace this with your visualization logic
  circles <- simpleJoin svg Circle dataPoints keyIsID_
  setAttributes circles
    [ A.cx (\\(_ :: Datum_) (i :: Index_) -> toNumber (datum_.index i) * 50.0)
    , A.cy 300.0
    , A.radius 20.0
    , A.fill "steelblue"
    ]

  -- Examples of using your data fields in attributes:
  -- A.cx (\\(d :: Datum_) _ -> datum_.""" <> firstField <> """ d)  -- Use first data field
  -- A.fill (\\(d :: Datum_) (i :: Index_) -> if datum_.index i > 5 then "red" else "blue")
  -- A.radius (\\(d :: Datum_) _ -> datum_.""" <> firstField <> """ d * 2.0)  -- Scale by data

  pure unit

-- | Entry point
run :: Array """ <> dataTypeName <> """ -> Effect Unit
run dataPoints = eval_D3M $ draw dataPoints "#chart"
"""

-- | Generate Main.purs module
generateMain :: String -> String -> String
generateMain vizName dataTypeName =
  """module Main where

import Prelude
import Effect (Effect)
import """ <> vizName <> """.Draw (run)
import """ <> vizName <> """.Model (exampleData)

main :: Effect Unit
main = run exampleData
"""

-- | Generate index.html
generateHTML :: String -> String
generateHTML vizName =
  """<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>""" <> vizName <> """ - PSD3 Visualization</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
      margin: 0;
      padding: 20px;
      background-color: #f5f5f5;
    }
    .container {
      max-width: 1200px;
      margin: 0 auto;
      background-color: white;
      padding: 30px;
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    h1 {
      color: #333;
      margin-top: 0;
    }
    #chart {
      margin: 20px 0;
      border: 1px solid #e0e0e0;
      border-radius: 4px;
      background-color: #fafafa;
    }
    svg {
      display: block;
    }
  </style>
</head>
<body>
  <div class="container">
    <h1>""" <> vizName <> """</h1>
    <div id="chart"></div>
  </div>

  <!-- D3.js from CDN -->
  <script src="https://cdn.jsdelivr.net/npm/d3@7.9.0/dist/d3.min.js"></script>

  <!-- Your bundled PureScript code -->
  <!-- After building with 'spago bundle', include your bundle here: -->
  <!-- <script src="./bundle.js"></script> -->
</body>
</html>
"""

-- | Generate README.md
generateREADME :: String -> String -> Array FieldDef -> String
generateREADME vizName dataTypeName fields =
  let
    fieldList = intercalate "\n" $ map (\f -> "- " <> f.name <> ": " <> f.fieldType) fields
    exampleFields = intercalate ", " $ map (\f -> f.name <> ": 0.0") fields
    firstField = fromMaybe "x" (map (\f -> f.name) (head fields))
    datumType = generateDatumType fields
  in
    """# """ <> vizName <> """

PSD3 visualization scaffold generated by the web wizard.

## Files Generated

- **Unsafe.purs** - Type coercion functions (isolates all unsafeCoerce calls)
- **Model.purs** - Data type definitions
- **Draw.purs** - Main visualization code
- **index.html** - HTML page with D3.js loaded from CDN

## Data Type

```purescript
type """ <> dataTypeName <> """ =
  { """ <> fieldList <> """
  }
```

## Quick Start

1. **Add your data** in `Model.purs`:
   ```purescript
   exampleData :: Array """ <> dataTypeName <> """
   exampleData =
     [ { """ <> exampleFields <> """ }
     -- Add more data points
     ]
   ```

2. **Customize the visualization** in `Draw.purs`
   - Modify the `draw` function to create your desired chart
   - Use the `datum_` accessor to work with your data safely
   - See the commented examples for using data fields in attributes

3. **Build and run**:
   ```bash
   # Compile the PureScript
   spago build

   # Bundle for the browser (if you created Main.purs)
   spago bundle --module Main --outfile bundle.js

   # Open index.html in your browser
   open index.html
   ```

## The datum_ Pattern

The `datum_` accessor record provides type-safe access to your data:

```purescript
datum_ ::
  { """ <> datumType <> """
  , index :: Index_ -> Int
  }
```

Use it in attributes like this:
```purescript
A.cx (\\(d :: Datum_) _ -> datum_.""" <> firstField <> """ d)
```

The type annotations `(d :: Datum_)` and `(i :: Index_)` help PureScript's type checker
find the correct ToAttr instance.

## Learn More

- [PSD3 Documentation](https://github.com/afcondon/PureScript-Tagless-D3)
- [Wizard Guide](../../docs/wizard-guide.md)
"""

-- Helper functions

generateRecordType :: Array FieldDef -> String
generateRecordType fields =
  let
    fieldLines = map (\f -> f.name <> " :: " <> f.fieldType) fields
  in
    joinWith "\n  , " fieldLines

generateAccessors :: Array FieldDef -> String -> String
generateAccessors fields coerceFnName =
  let
    accessorLines = map (\f -> f.name <> ": _." <> f.name <> " <<< " <> coerceFnName) fields
  in
    joinWith "\n  , " accessorLines

generateDatumType :: Array FieldDef -> String
generateDatumType fields =
  let
    typeLines = map (\f -> f.name <> " :: Datum_ -> " <> f.fieldType) fields
  in
    joinWith "\n  , " typeLines
