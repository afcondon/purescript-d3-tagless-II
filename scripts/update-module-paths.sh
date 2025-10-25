#!/bin/bash

# Script to update module declarations and imports after PSD3 refactoring
# Run from project root: bash scripts/update-module-paths.sh

set -e

echo "Updating module declarations and imports..."

# Find all .purs files in src/lib/PSD3 and src/lib/Data
find src/lib/PSD3 src/lib/Data -name "*.purs" -type f | while read file; do
  echo "Processing: $file"

  # Create backup
  cp "$file" "$file.bak"

  # Update module declarations
  sed -i '' \
    -e 's/^module D3\.FFI /module PSD3.Internal.FFI /' \
    -e 's/^module D3\.Data\.Types /module PSD3.Internal.Types /' \
    -e 's/^module D3\.Attributes\.Instances /module PSD3.Internal.Attributes.Instances /' \
    -e 's/^module D3\.Attributes\.Sugar /module PSD3.Internal.Attributes.Sugar /' \
    -e 's/^module D3\.Selection\.Functions /module PSD3.Internal.Selection.Functions /' \
    -e 's/^module D3\.Selection /module PSD3.Internal.Selection.Types /' \
    -e 's/^module D3\.Scales\.Linear /module PSD3.Internal.Scales.Linear /' \
    -e 's/^module D3\.Scales /module PSD3.Internal.Scales.Scales /' \
    -e 's/^module D3\.Axes /module PSD3.Internal.Axes /' \
    -e 's/^module D3\.Simulation\.Types /module PSD3.Internal.Simulation.Types /' \
    -e 's/^module D3\.Simulation\.Config /module PSD3.Internal.Simulation.Config /' \
    -e 's/^module D3\.Simulation\.Forces /module PSD3.Internal.Simulation.Forces /' \
    -e 's/^module D3\.Simulation\.Functions /module PSD3.Internal.Simulation.Functions /' \
    -e 's/^module D3\.Layouts\.Sankey\.Types /module PSD3.Internal.Sankey.Types /' \
    -e 's/^module D3\.Layouts\.Sankey\.Functions /module PSD3.Internal.Sankey.Functions /' \
    -e 's/^module D3\.Layouts\.Hierarchical /module PSD3.Internal.Hierarchical /' \
    -e 's/^module D3\.Generators\.Line /module PSD3.Internal.Generators.Line /' \
    -e 's/^module D3\.Zoom /module PSD3.Internal.Zoom /' \
    -e 's/^module D3\.Data\.Tree /module PSD3.Data.Tree /' \
    -e 's/^module D3\.Node /module PSD3.Data.Node /' \
    -e 's/^module D3\.Data\.Utility /module PSD3.Data.Utility /' \
    -e 's/^module D3Tagless\.Utility /module PSD3.Internal.Utility /' \
    -e 's/^module D3Tagless\.Capabilities\.String /module PSD3.Interpreter.String /' \
    -e 's/^module D3Tagless\.Capabilities\.MetaTree /module PSD3.Interpreter.MetaTree /' \
    "$file"

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
  # This is tricky - need to replace with the appropriate capability module
  sed -i '' \
    -e 's/import D3Tagless\.Capabilities (class SelectionM/import PSD3.Capabilities.Selection (class SelectionM/g' \
    -e 's/import D3Tagless\.Capabilities (class SimulationM/import PSD3.Capabilities.Simulation (class SimulationM/g' \
    -e 's/import D3Tagless\.Capabilities (class SankeyM/import PSD3.Capabilities.Sankey (class SankeyM/g' \
    "$file"

  # Handle multi-import cases for Capabilities
  sed -i '' \
    -e 's/import D3Tagless\.Capabilities/import PSD3.Capabilities.Selection/g' \
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

# Clean up backup files
echo ""
echo "Cleaning up backup files..."
find src/lib/PSD3 src/lib/Data -name "*.purs.bak" -delete

echo ""
echo "✓ Module path updates complete!"
echo ""
echo "Next steps:"
echo "1. Run: npm run build"
echo "2. Check for any compilation errors"
echo "3. If errors, review changes and adjust"
echo "4. If successful, commit changes"
echo ""
echo "To revert all changes: git checkout -- src/lib/"
