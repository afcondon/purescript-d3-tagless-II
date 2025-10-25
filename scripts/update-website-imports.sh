#!/bin/bash

# Script to update imports in website/examples code after PSD3 refactoring
# Run from project root: bash scripts/update-website-imports.sh

set -e

echo "Updating website and example imports..."

# Find all .purs files in src/website and docs/code-examples
find src/website docs/code-examples -name "*.purs" -type f 2>/dev/null | while read file; do
  echo "Processing: $file"

  # Update import statements - FFI and Types first (most specific)
  sed -i '' \
    -e 's/import D3\.FFI/import PSD3.Internal.FFI/g' \
    -e 's/import D3\.Data\.Types/import PSD3.Internal.Types/g' \
    "$file"

  # Update import statements - Attributes
  sed -i '' \
    -e 's/import D3\.Attributes\.Instances/import PSD3.Internal.Attributes.Instances/g' \
    -e 's/import D3\.Attributes\.Sugar/import PSD3.Internal.Attributes.Sugar/g' \
    "$file"

  # Update import statements - Selection (most specific first)
  sed -i '' \
    -e 's/import D3\.Selection\.Functions/import PSD3.Internal.Selection.Functions/g' \
    -e 's/import D3\.Selection/import PSD3.Internal.Selection.Types/g' \
    "$file"

  # Update import statements - Scales
  sed -i '' \
    -e 's/import D3\.Scales\.Linear/import PSD3.Internal.Scales.Linear/g' \
    -e 's/import D3\.Scales/import PSD3.Internal.Scales.Scales/g' \
    "$file"

  # Update import statements - Axes
  sed -i '' \
    -e 's/import D3\.Axes/import PSD3.Internal.Axes/g' \
    "$file"

  # Update import statements - Simulation (most specific first)
  sed -i '' \
    -e 's/import D3\.Simulation\.Types/import PSD3.Internal.Simulation.Types/g' \
    -e 's/import D3\.Simulation\.Config/import PSD3.Internal.Simulation.Config/g' \
    -e 's/import D3\.Simulation\.Forces/import PSD3.Internal.Simulation.Forces/g' \
    -e 's/import D3\.Simulation\.Functions/import PSD3.Internal.Simulation.Functions/g' \
    "$file"

  # Update import statements - Sankey
  sed -i '' \
    -e 's/import D3\.Layouts\.Sankey\.Types/import PSD3.Internal.Sankey.Types/g' \
    -e 's/import D3\.Layouts\.Sankey\.Functions/import PSD3.Internal.Sankey.Functions/g' \
    "$file"

  # Update import statements - Hierarchical, Generators, Zoom
  sed -i '' \
    -e 's/import D3\.Layouts\.Hierarchical/import PSD3.Internal.Hierarchical/g' \
    -e 's/import D3\.Generators\.Line/import PSD3.Internal.Generators.Line/g' \
    -e 's/import D3\.Zoom/import PSD3.Internal.Zoom/g' \
    "$file"

  # Update import statements - Data
  sed -i '' \
    -e 's/import D3\.Data\.Tree/import PSD3.Data.Tree/g' \
    -e 's/import D3\.Data\.Graph/import Data.DependencyGraph/g' \
    -e 's/import D3\.Node/import PSD3.Data.Node/g' \
    -e 's/import D3\.Data\.Utility/import PSD3.Data.Utility/g' \
    "$file"

  # Update import statements - D3Tagless Capabilities
  # For website code, we'll just use the generic replacement
  sed -i '' \
    -e 's/import D3Tagless\.Capabilities (class SelectionM/import PSD3.Capabilities.Selection (class SelectionM/g' \
    -e 's/import D3Tagless\.Capabilities (class SimulationM/import PSD3.Capabilities.Simulation (class SimulationM/g' \
    -e 's/import D3Tagless\.Capabilities (class SankeyM/import PSD3.Capabilities.Sankey (class SankeyM/g' \
    "$file"

  # Handle bare D3Tagless.Capabilities imports
  sed -i '' \
    -e 's/import D3Tagless\.Capabilities$/import PSD3.Capabilities.Selection/g' \
    "$file"

  # Update import statements - D3Tagless Instances -> Interpreter.D3
  sed -i '' \
    -e 's/import D3Tagless\.Instance\.Selection/import PSD3.Interpreter.D3/g' \
    -e 's/import D3Tagless\.Instance\.Simulation/import PSD3.Interpreter.D3/g' \
    -e 's/import D3Tagless\.Instance\.Sankey/import PSD3.Interpreter.D3/g' \
    "$file"

  # Update import statements - Other interpreters
  sed -i '' \
    -e 's/import D3Tagless\.Utility/import PSD3.Internal.Utility/g' \
    -e 's/import D3Tagless\.Capabilities\.String/import PSD3.Interpreter.String/g' \
    -e 's/import D3Tagless\.Capabilities\.MetaTree/import PSD3.Interpreter.MetaTree/g' \
    "$file"

  echo "  ✓ Updated"
done

echo ""
echo "✓ Website import updates complete!"
echo ""
echo "Next steps:"
echo "1. Run: npm run build"
echo "2. Check for compilation errors"
echo "3. Fix any remaining import issues manually"
echo ""
