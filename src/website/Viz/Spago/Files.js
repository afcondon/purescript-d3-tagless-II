export function readSpago_Raw_JSON_(modulesBody) {
  return packagesBody => lsdepsBody => locBody => {
    const modules = decodeModulesFile(modulesBody);
    const packages = decodePackagesFile(packagesBody);
    const lsDeps = decodeLsDepsFile(lsdepsBody);
    const loc = decodeLOCFile(locBody);

    // Build a path lookup map from LOC data (filesystem paths)
    const pathToLOC = buildPathLookup(loc);

    // Enhance modules with LOC data using smart path matching
    const modulesWithLOC = modules.map(module => ({
      ...module,
      loc: lookupLOC(module.path, pathToLOC, loc)
    }));

    return { modules: modulesWithLOC, packages, lsDeps, loc }
  };
}

// Build a lookup that handles both exact paths and fuzzy matching
function buildPathLookup(locArray) {
  const lookup = {};

  for (const entry of locArray) {
    // Store by exact path
    lookup[entry.path] = entry.loc;

    // Also create a normalized key for fuzzy matching
    // .spago/aff/v7.0.0/src/Effect/Aff.purs -> aff::src/Effect/Aff.purs
    const parts = entry.path.split('/');
    if (parts[0] === '.spago' && parts.length >= 4) {
      const pkg = parts[1];
      const relPath = parts.slice(3).join('/');
      lookup[`${pkg}::${relPath}`] = entry.loc;
    }
  }

  return lookup;
}

// Smart LOC lookup that handles path format differences
function lookupLOC(modulePath, pathToLOC, locArray) {
  // Try exact match first
  if (pathToLOC[modulePath]) {
    return pathToLOC[modulePath];
  }

  // Try fuzzy match for .spago paths
  // .spago/p/aff/32787.../src/Effect/Aff.purs -> aff::src/Effect/Aff.purs
  const parts = modulePath.split('/');
  if (parts[0] === '.spago' && parts.length >= 4) {
    let pkg, relPath;

    if (parts[1] === 'p' && parts.length >= 5) {
      // New format: .spago/p/aff/hash/src/...
      pkg = parts[2];
      relPath = parts.slice(4).join('/');
    } else {
      // Old format or direct package: .spago/aff/v7.0.0/src/...
      pkg = parts[1];
      relPath = parts.slice(3).join('/');
    }

    const fuzzyKey = `${pkg}::${relPath}`;
    if (pathToLOC[fuzzyKey]) {
      return pathToLOC[fuzzyKey];
    }
  }

  // Default fallback
  return 10.0;
}

// module has key, path & depends
const decodeModulesFile = function (filecontents) {
  const json = JSON.parse(filecontents)
  const modules = Object.keys(json).map(key => { return { key: key, depends: json[key].depends, path: json[key].path }; })

  return modules;
}

// package has key and depends
const decodePackagesFile = function (filecontents) {
  const json = JSON.parse(filecontents)
  const packages = Object.keys(json).map(key => { return { key: key, depends: json[key].depends }; })

  return packages;
}

// package has key and depends
const decodeLOCFile = function (filecontents) {
  const json = JSON.parse(filecontents)
  return json.loc;
}

// lsdep has key === packageName, version, repo { tag, contents }
const decodeLsDepsFile = function (filecontents) {
  const jsonlines = splitIntoLines(filecontents)
  jsonlines.length = jsonlines.length - 1
  var objectArray = jsonlines.map(d => JSON.parse(d))
  return objectArray;
}

function splitIntoLines(str) {
  // See http://www.unicode.org/reports/tr18/#RL1.6
  return str.split(/\r\n|[\n\v\f\r\u0085\u2028\u2029]/);
}

