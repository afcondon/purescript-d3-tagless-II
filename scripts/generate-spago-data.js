#!/usr/bin/env node

/**
 * Generate Spago visualization data files:
 * - modules.json (from spago graph modules --json)
 * - packages.json (from spago graph packages --json)
 * - LOC.json (from wc line counts of .purs files)
 *
 * Outputs to docs/data/spago-data/
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const OUTPUT_DIR = path.join(__dirname, '../docs/data/spago-data');
const OUTPUT_BUILD_DIR = path.join(__dirname, '../output');

// Ensure output directory exists
if (!fs.existsSync(OUTPUT_DIR)) {
  fs.mkdirSync(OUTPUT_DIR, { recursive: true });
}

// ====================================================
// PHASE 1: Declaration Extraction from docs.json
// ====================================================

/**
 * Extract all declarations (functions, types, typeclasses) from docs.json files
 * @returns {Object} Complete declaration registry with metadata
 */
function extractDeclarations() {
  const result = {
    modules: {},
    stats: {
      totalModules: 0,
      totalDeclarations: 0,
      byKind: {}
    }
  };

  if (!fs.existsSync(OUTPUT_BUILD_DIR)) {
    console.warn('⚠ output/ directory not found. Run `npm run build` first.');
    return result;
  }

  const moduleDirs = fs.readdirSync(OUTPUT_BUILD_DIR);

  for (const moduleDir of moduleDirs) {
    const docsPath = path.join(OUTPUT_BUILD_DIR, moduleDir, 'docs.json');

    if (!fs.existsSync(docsPath)) continue;

    try {
      const docs = JSON.parse(fs.readFileSync(docsPath, 'utf8'));
      const moduleName = moduleDir;

      result.modules[moduleName] = {
        name: moduleName,
        comments: docs.comments || '',
        declarations: []
      };

      if (docs.declarations) {
        for (const decl of docs.declarations) {
          const declInfo = {
            title: decl.title,
            kind: decl.info?.declType || 'unknown',
            comments: decl.comments || '',
            sourceSpan: decl.sourceSpan
          };

          // Extract type signature if available
          if (decl.info?.type) {
            declInfo.typeSignature = simplifyType(decl.info.type);
          }

          // For type classes, extract members
          if (decl.children && decl.children.length > 0) {
            declInfo.members = decl.children.map(child => ({
              name: child.title,
              comments: child.comments || '',
              typeSignature: child.info?.type ? simplifyType(child.info.type) : null
            }));
          }

          result.modules[moduleName].declarations.push(declInfo);
          result.stats.totalDeclarations++;
          result.stats.byKind[declInfo.kind] = (result.stats.byKind[declInfo.kind] || 0) + 1;
        }
      }

      result.stats.totalModules++;
    } catch (err) {
      console.warn(`⚠ Could not parse docs.json for ${moduleDir}:`, err.message);
    }
  }

  return result;
}

/**
 * Simplify PureScript type representation to extract referenced types
 * @param {Object} type - Type AST from docs.json
 * @returns {Object} Simplified type with referenced modules/types
 */
function simplifyType(type) {
  if (!type) return null;

  const referencedTypes = new Set();

  function extractTypeRefs(t) {
    if (!t) return;

    if (t.tag === 'TypeConstructor' && t.contents) {
      // Format: [["Module", "Name"], "TypeName"]
      if (Array.isArray(t.contents) && t.contents.length === 2) {
        const [modulePath, typeName] = t.contents;
        if (Array.isArray(modulePath)) {
          const fullName = modulePath.join('.') + '.' + typeName;
          referencedTypes.add(fullName);
        }
      }
    }

    if (t.tag === 'TypeApp' && t.contents) {
      t.contents.forEach(extractTypeRefs);
    }

    if (Array.isArray(t)) {
      t.forEach(extractTypeRefs);
    }

    if (t.contents && typeof t.contents === 'object') {
      extractTypeRefs(t.contents);
    }
  }

  extractTypeRefs(type);

  return {
    raw: type,
    referencedTypes: Array.from(referencedTypes)
  };
}

/**
 * Build type dependency graph from all declarations
 * Shows which types reference which other types
 * @returns {Object} Type dependency graph
 */
function extractTypeDependencies() {
  const result = {
    types: {},
    edges: []
  };

  if (!fs.existsSync(OUTPUT_BUILD_DIR)) {
    return result;
  }

  const moduleDirs = fs.readdirSync(OUTPUT_BUILD_DIR);

  for (const moduleDir of moduleDirs) {
    const docsPath = path.join(OUTPUT_BUILD_DIR, moduleDir, 'docs.json');

    if (!fs.existsSync(docsPath)) continue;

    try {
      const docs = JSON.parse(fs.readFileSync(docsPath, 'utf8'));

      if (docs.declarations) {
        for (const decl of docs.declarations) {
          if (decl.info?.declType === 'data' || decl.info?.declType === 'type') {
            const typeName = moduleDir + '.' + decl.title;

            result.types[typeName] = {
              module: moduleDir,
              name: decl.title,
              kind: decl.info.declType,
              usedBy: []
            };

            // Extract types this type depends on
            if (decl.info?.type) {
              const simplified = simplifyType(decl.info.type);
              if (simplified?.referencedTypes) {
                for (const refType of simplified.referencedTypes) {
                  result.edges.push({
                    from: typeName,
                    to: refType,
                    relationship: 'uses'
                  });
                }
              }
            }
          }
        }
      }
    } catch (err) {
      // Skip modules that can't be parsed
    }
  }

  // Build reverse index (usedBy)
  for (const edge of result.edges) {
    if (result.types[edge.to]) {
      result.types[edge.to].usedBy.push(edge.from);
    }
  }

  return result;
}

// ====================================================
// PHASE 2: Function Call Graph from corefn.json
// ====================================================

/**
 * Extract cross-module function call graph from CoreFn AST
 * Shows which functions actually call which other functions
 * @returns {Object} Function call graph
 */
function extractFunctionCalls() {
  const result = {
    functions: {},
    calls: [],
    stats: {
      totalFunctions: 0,
      totalCalls: 0,
      crossModuleCalls: 0
    }
  };

  if (!fs.existsSync(OUTPUT_BUILD_DIR)) {
    return result;
  }

  const moduleDirs = fs.readdirSync(OUTPUT_BUILD_DIR);

  for (const moduleDir of moduleDirs) {
    const corefnPath = path.join(OUTPUT_BUILD_DIR, moduleDir, 'corefn.json');

    if (!fs.existsSync(corefnPath)) continue;

    try {
      const corefn = JSON.parse(fs.readFileSync(corefnPath, 'utf8'));
      const currentModule = corefn.moduleName ? corefn.moduleName.join('.') : moduleDir;

      // Process each declaration in the module
      if (corefn.decls && Array.isArray(corefn.decls)) {
        for (const decl of corefn.decls) {
          if (!decl.identifier) continue;

          const funcName = currentModule + '.' + decl.identifier;

          result.functions[funcName] = {
            module: currentModule,
            name: decl.identifier,
            calls: [],
            calledBy: []
          };

          result.stats.totalFunctions++;

          // Extract function calls from the declaration's expression
          const calls = extractCalls(decl.expression, currentModule);

          for (const call of calls) {
            result.functions[funcName].calls.push(call);
            result.calls.push({
              from: funcName,
              to: call.target,
              targetModule: call.targetModule,
              isCrossModule: call.isCrossModule
            });

            result.stats.totalCalls++;
            if (call.isCrossModule) {
              result.stats.crossModuleCalls++;
            }
          }
        }
      }
    } catch (err) {
      console.warn(`⚠ Could not parse corefn.json for ${moduleDir}:`, err.message);
    }
  }

  // Build reverse index (calledBy)
  for (const call of result.calls) {
    if (result.functions[call.to]) {
      result.functions[call.to].calledBy.push(call.from);
    }
  }

  return result;
}

/**
 * Recursively walk CoreFn expression AST to find function calls
 * @param {Object} expr - CoreFn expression node
 * @param {String} currentModule - Current module name
 * @returns {Array} List of function calls
 */
function extractCalls(expr, currentModule) {
  const calls = [];

  if (!expr) return calls;

  function walk(node) {
    if (!node || typeof node !== 'object') return;

    // Found a variable reference (function call or value)
    if (node.type === 'Var' && node.value) {
      const moduleName = Array.isArray(node.value.moduleName)
        ? node.value.moduleName.join('.')
        : node.value.moduleName;
      const identifier = node.value.identifier;

      if (moduleName && identifier) {
        const isCrossModule = moduleName !== currentModule;
        calls.push({
          target: moduleName + '.' + identifier,
          targetModule: moduleName,
          identifier: identifier,
          isCrossModule: isCrossModule
        });
      }
    }

    // Recursively walk App (function application)
    if (node.type === 'App') {
      walk(node.abstraction);
      walk(node.argument);
    }

    // Recursively walk Abs (lambda abstraction)
    if (node.type === 'Abs') {
      walk(node.body);
    }

    // Recursively walk Let bindings
    if (node.type === 'Let' && node.binds) {
      for (const bind of node.binds) {
        walk(bind.expression);
      }
      walk(node.expression);
    }

    // Recursively walk Case expressions
    if (node.type === 'Case') {
      if (node.caseExpressions) {
        for (const caseExpr of node.caseExpressions) {
          walk(caseExpr);
        }
      }
      if (node.caseAlternatives) {
        for (const alt of node.caseAlternatives) {
          walk(alt.result);
        }
      }
    }

    // Recursively walk Accessor (record field access)
    if (node.type === 'Accessor') {
      walk(node.expression);
    }

    // Recursively walk ObjectUpdate
    if (node.type === 'ObjectUpdate') {
      walk(node.expression);
      if (node.updates) {
        for (const [key, value] of Object.entries(node.updates)) {
          walk(value);
        }
      }
    }

    // Recursively walk Literal values
    if (node.type === 'Literal' && node.value) {
      if (node.value.literalType === 'ArrayLiteral' && Array.isArray(node.value.value)) {
        for (const elem of node.value.value) {
          walk(elem);
        }
      }
      if (node.value.literalType === 'ObjectLiteral' && Array.isArray(node.value.value)) {
        for (const [key, val] of node.value.value) {
          walk(val);
        }
      }
    }

    // Handle arrays
    if (Array.isArray(node)) {
      for (const item of node) {
        walk(item);
      }
    }
  }

  walk(expr);
  return calls;
}

console.log('Generating Spago visualization data...\n');

// 1. Generate modules.json
console.log('📦 Generating modules.json...');
try {
  const modulesJson = execSync('npx spago graph modules --json', {
    encoding: 'utf8',
    maxBuffer: 10 * 1024 * 1024 // 10MB buffer
  });
  fs.writeFileSync(path.join(OUTPUT_DIR, 'modules.json'), modulesJson);
  console.log('✓ modules.json generated');
} catch (err) {
  console.error('✗ Error generating modules.json:', err.message);
  process.exit(1);
}

// 2. Generate packages.json
console.log('📦 Generating packages.json...');
try {
  const packagesJson = execSync('npx spago graph packages --json', {
    encoding: 'utf8',
    maxBuffer: 10 * 1024 * 1024
  });
  fs.writeFileSync(path.join(OUTPUT_DIR, 'packages.json'), packagesJson);
  console.log('✓ packages.json generated');
} catch (err) {
  console.error('✗ Error generating packages.json:', err.message);
  process.exit(1);
}

// 3. Generate LOC.json
console.log('📊 Generating LOC.json...');
try {
  // Find all .purs files on disk (using actual filesystem structure)
  const srcFiles = execSync('find src -name "*.purs" 2>/dev/null || true', { encoding: 'utf8' })
    .split('\n')
    .filter(f => f.trim());

  const spagoFiles = execSync('find .spago -name "*.purs" 2>/dev/null || true', { encoding: 'utf8' })
    .split('\n')
    .filter(f => f.trim());

  const allFiles = [...srcFiles, ...spagoFiles];

  const locData = {
    loc: []
  };

  let successCount = 0;
  let failCount = 0;

  for (const filePath of allFiles) {
    if (!filePath) continue;

    try {
      // Count lines in the file
      const lineCount = execSync(`grep -c ^ "${filePath}" 2>/dev/null || echo 0`, { encoding: 'utf8' }).trim();
      const loc = parseInt(lineCount, 10);

      if (loc > 0) {
        locData.loc.push({
          loc: loc,
          path: filePath
        });
        successCount++;
      } else {
        failCount++;
      }
    } catch (err) {
      failCount++;
    }
  }

  // Sort by path for consistency
  locData.loc.sort((a, b) => a.path.localeCompare(b.path));

  fs.writeFileSync(
    path.join(OUTPUT_DIR, 'LOC.json'),
    JSON.stringify(locData, null, 2)
  );
  console.log(`✓ LOC.json generated (${successCount} files from filesystem)`);
} catch (err) {
  console.error('✗ Error generating LOC.json:', err.message);
  process.exit(1);
}

// 4. Generate lsdeps.jsonlines (package name to repo mapping)
console.log('🔗 Generating lsdeps.jsonlines...');
try {
  const packagesListJson = execSync('npx spago ls packages --json', {
    encoding: 'utf8',
    maxBuffer: 10 * 1024 * 1024
  });

  const packagesList = JSON.parse(packagesListJson);
  const lsdepsLines = [];

  for (const [packageName, packageInfo] of Object.entries(packagesList)) {
    if (packageInfo.type === 'git' && packageInfo.value) {
      const repo = packageInfo.value.git;
      const version = packageInfo.value.ref || 'unknown';

      lsdepsLines.push(JSON.stringify({
        packageName: packageName,
        version: version,
        repo: {
          tag: 'Remote',
          contents: repo
        }
      }));
    }
  }

  fs.writeFileSync(
    path.join(OUTPUT_DIR, 'lsdeps.jsonlines'),
    lsdepsLines.join('\n') + '\n'
  );
  console.log(`✓ lsdeps.jsonlines generated (${lsdepsLines.length} packages)`);
} catch (err) {
  console.error('✗ Error generating lsdeps.jsonlines:', err.message);
  process.exit(1);
}

// 5. Generate declarations.json (Phase 1: Extract from docs.json)
console.log('📚 Generating declarations.json...');
try {
  const declarations = extractDeclarations();
  fs.writeFileSync(
    path.join(OUTPUT_DIR, 'declarations.json'),
    JSON.stringify(declarations, null, 2)
  );
  console.log(`✓ declarations.json generated (${Object.keys(declarations.modules).length} modules, ${declarations.stats.totalDeclarations} declarations)`);
} catch (err) {
  console.error('✗ Error generating declarations.json:', err.message);
  process.exit(1);
}

// 6. Generate type-dependencies.json (Phase 1: Type usage graph)
console.log('🔗 Generating type-dependencies.json...');
try {
  const typeDeps = extractTypeDependencies();
  fs.writeFileSync(
    path.join(OUTPUT_DIR, 'type-dependencies.json'),
    JSON.stringify(typeDeps, null, 2)
  );
  console.log(`✓ type-dependencies.json generated (${Object.keys(typeDeps.types).length} types)`);
} catch (err) {
  console.error('✗ Error generating type-dependencies.json:', err.message);
  process.exit(1);
}

// 7. Generate function-calls.json (Phase 2: Cross-module function call graph)
console.log('🔄 Generating function-calls.json...');
try {
  const functionCalls = extractFunctionCalls();
  fs.writeFileSync(
    path.join(OUTPUT_DIR, 'function-calls.json'),
    JSON.stringify(functionCalls, null, 2)
  );
  console.log(`✓ function-calls.json generated (${functionCalls.stats.totalFunctions} functions, ${functionCalls.stats.crossModuleCalls} cross-module calls)`);
} catch (err) {
  console.error('✗ Error generating function-calls.json:', err.message);
  process.exit(1);
}

console.log('\n✅ All Spago data files generated successfully!');
console.log(`   Output directory: ${OUTPUT_DIR}`);
