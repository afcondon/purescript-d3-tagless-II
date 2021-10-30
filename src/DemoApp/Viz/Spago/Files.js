exports.readSpago_Raw_JSON_ = modulesBody => packagesBody => lsdepsBody => locBody => {
  const modules  = decodeModulesFile(modulesBody);
  const packages = decodePackagesFile(packagesBody);
  const lsDeps   = decodeLsDepsFile(lsdepsBody);
  const loc      = decodeLOCFile(locBody);

  return { modules, packages, lsDeps, loc }
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

function splitIntoLines (str) {
  // See http://www.unicode.org/reports/tr18/#RL1.6
  return str.split(/\r\n|[\n\v\f\r\u0085\u2028\u2029]/);
}

