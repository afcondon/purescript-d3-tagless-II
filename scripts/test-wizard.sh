#!/bin/bash
# Test script for init-psd3-viz.js

cd "$(dirname "$0")/.."

# Test 1: Simple parabola example
echo "Testing wizard with ParabolaTest example..."
node scripts/init-psd3-viz.js <<EOF
ParabolaTest
x:Number,y:Number
src/test/ParabolaTest
n
EOF

echo ""
echo "Checking generated files..."
ls -la src/test/ParabolaTest/

echo ""
echo "=== Unsafe.purs ==="
cat src/test/ParabolaTest/Unsafe.purs

echo ""
echo "=== Model.purs ==="
cat src/test/ParabolaTest/Model.purs

echo ""
echo "=== Draw.purs (first 40 lines) ==="
head -n 40 src/test/ParabolaTest/Draw.purs
