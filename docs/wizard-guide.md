# Using the PSD3 Visualization Wizard

The PSD3 project includes a scaffold generator to help you quickly set up new visualizations following best practices.

## Running the Wizard

From your project root, run:

```bash
node scripts/init-psd3-viz.js
```

The wizard will prompt you for:

1. **Visualization module name** - Must start with an uppercase letter (e.g., `MyBarChart`, `NetworkGraph`)
2. **Data record fields** - Comma-separated field definitions (e.g., `x:Number,y:Number,label:String`)
3. **Output directory** - Where to create the files (default: `src/viz/YourModuleName`)
4. **Generate Main.purs?** - Optional entry point module (y/n, default: n)
5. **Generate index.html?** - HTML page with D3.js loaded (y/n, default: y)

## Example Session

```
Visualization module name: ParabolaChart
Data record fields: x:Number,y:Number
Output directory: src/viz/ParabolaChart
Generate Main.purs? (y/n, default: n): y
Generate index.html? (y/n, default: y): y
```

## Generated Files

The wizard creates the following files:

- **Unsafe.purs** - Type coercion functions
- **Model.purs** - Data type definitions
- **Draw.purs** - Visualization code
- **README.md** - Quick start guide for your visualization
- **index.html** - HTML page with D3.js from CDN (optional)
- **Main.purs** - Entry point module (optional)

### 1. Unsafe.purs

Contains only the type-coercion functions needed to work with D3's untyped data:

```purescript
module ParabolaChart.Unsafe where

import PSD3.Internal.Types (Datum_, Index_)
import Unsafe.Coerce (unsafeCoerce)

type ParabolaChartData =
  { x :: Number
  , y :: Number
  }

coerceToParabolaChartData :: Datum_ -> ParabolaChartData
coerceToParabolaChartData = unsafeCoerce

coerceIndex :: Index_ -> Int
coerceIndex = unsafeCoerce
```

**Why this module exists:** D3.js works with untyped JavaScript data, but PureScript is strongly typed. The data join operations return opaque `Datum_` types that must be coerced back to your actual data type. By isolating these `unsafeCoerce` operations in a dedicated `Unsafe` module, we make it clear where type safety boundaries exist while keeping the rest of your visualization code clean and type-safe.

### 2. Model.purs

Defines your data type and provides a placeholder for example data:

```purescript
module ParabolaChart.Model where

type ParabolaChartData =
  { x :: Number
  , y :: Number
  }

exampleData :: Array ParabolaChartData
exampleData =
  [ -- Add your example data here
  ]
```

### 3. Draw.purs

Contains the visualization code with the accessor pattern:

```purescript
module ParabolaChart.Draw where

import Prelude
import PSD3
import PSD3.Attributes as A
import PSD3.Internal.FFI (keyIsID_)
import ParabolaChart.Unsafe (coerceToParabolaChartData, coerceIndex)
import ParabolaChart.Model (ParabolaChartData)
import Data.Int (toNumber)
import Effect (Effect)

-- | Accessor record for working with bound data
datum_ ::
  { x :: Datum_ -> Number
  , y :: Datum_ -> Number
  , index :: Index_ -> Int
  }
datum_ =
  { x: _.x <<< coerceToParabolaChartData
  , y: _.y <<< coerceToParabolaChartData
  , index: coerceIndex
  }

-- | Main drawing function
draw :: forall m. SelectionM D3Selection_ m => Array ParabolaChartData -> Selector D3Selection_ -> m Unit
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
    [ A.cx (\(_ :: Datum_) (i :: Index_) -> toNumber (datum_.index i) * 50.0)
    , A.cy 300.0
    , A.radius 20.0
    , A.fill "steelblue"
    ]

  pure unit

-- | Entry point
run :: Array ParabolaChartData -> Effect Unit
run dataPoints = eval_D3M $ draw dataPoints "#chart"
```

## The `datum_` Accessor Pattern

The `datum_` record is central to working with bound data in PSD3. It provides typed accessor functions that:

1. Take the opaque `Datum_` or `Index_` types from D3
2. Apply the coercion function from `Unsafe.purs`
3. Extract the specific field you need

This pattern keeps the unsafe coercion in one place while giving you convenient, typed access to your data throughout your visualization code.

### Using Accessors in Attributes

When you need to use data or index values in attributes, you have two options:

**1. Simple accessor (for data-only attributes):**
```purescript
A.radius (\(d :: Datum_) -> datum_.x d * 2.0)
```

**2. Indexed accessor (when you need both data and index):**
```purescript
A.cx (\(_ :: Datum_) (i :: Index_) -> toNumber (datum_.index i) * 50.0)
```

**Type annotations are required** in the lambda parameters (`(d :: Datum_)`, `(i :: Index_)`) to help PureScript's type inference find the correct `ToAttr` instance.

### 4. index.html (Optional)

A minimal HTML page ready to display your visualization:

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>YourViz - PSD3 Visualization</title>
  <style>
    /* Basic styling for the visualization container */
    #chart { margin: 20px 0; }
  </style>
</head>
<body>
  <div class="container">
    <h1>YourViz</h1>
    <div id="chart"></div>
  </div>

  <!-- D3.js v7.9.0 from CDN -->
  <script src="https://cdn.jsdelivr.net/npm/d3@7.9.0/dist/d3.min.js"></script>

  <!-- Your bundled PureScript code -->
  <script src="./bundle.js"></script>
</body>
</html>
```

The `#chart` div matches the selector in your `Draw.purs` file, and D3.js is loaded from a CDN so you don't need to install it locally.

### 5. README.md

A quick reference guide specific to your visualization, including:
- File descriptions
- Your data type definition
- Build and run instructions
- Examples of using the `datum_` pattern

## Next Steps

After running the wizard:

1. **Add your data** - Fill in the `exampleData` array in `Model.purs`
2. **Customize the visualization** - Modify the `draw` function in `Draw.purs` to create your desired chart
3. **Build** - Run `spago build` to compile
4. **Bundle for browser** (if you generated Main.purs and index.html):
   ```bash
   spago bundle --module Main --outfile bundle.js
   open index.html
   ```
5. **Or import in your app**:
   ```purescript
   import YourViz.Draw (run)
   import YourViz.Model (exampleData)
   ```

## Common Data Types

When specifying fields, use these PureScript types:

- `Number` - Floating point numbers
- `Int` - Integers
- `String` - Text
- `Boolean` - True/false values
- `Array SomeType` - Arrays of elements
- Custom types defined in your project

## Tips

- **Start simple** - Begin with basic fields and add complexity as needed
- **Add to datum_ as you go** - You don't need to define all accessors upfront; add them when you need them in your attributes
- **Type safety helps** - If you misspell a field or use the wrong type, PureScript's compiler will catch it
- **Keep Unsafe minimal** - Only coercion functions belong in `Unsafe.purs`; all visualization logic should be in `Draw.purs`
