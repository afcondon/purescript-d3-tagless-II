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

// Ensure output directory exists
if (!fs.existsSync(OUTPUT_DIR)) {
  fs.mkdirSync(OUTPUT_DIR, { recursive: true });
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
  // Find all .purs files in src/ and .spago/
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

  for (const file of allFiles) {
    if (!file) continue;

    try {
      // Count lines in the file (excluding blank lines)
      const lineCount = execSync(`grep -c ^ "${file}" || echo 0`, { encoding: 'utf8' }).trim();
      const loc = parseInt(lineCount, 10);

      locData.loc.push({
        loc: loc,
        path: file
      });
    } catch (err) {
      // Skip files that can't be read
      console.warn(`  Warning: Could not count lines in ${file}`);
    }
  }

  // Sort by path for consistency
  locData.loc.sort((a, b) => a.path.localeCompare(b.path));

  fs.writeFileSync(
    path.join(OUTPUT_DIR, 'LOC.json'),
    JSON.stringify(locData, null, 2)
  );
  console.log(`✓ LOC.json generated (${locData.loc.length} files: ${srcFiles.length} src, ${spagoFiles.length} .spago)`);
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

console.log('\n✅ All Spago data files generated successfully!');
console.log(`   Output directory: ${OUTPUT_DIR}`);
