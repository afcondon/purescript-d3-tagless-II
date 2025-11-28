// Parse declarations.json and filter to my-project modules
export function parseDeclarationsJSON_(jsonString) {
  const data = JSON.parse(jsonString);
  const result = [];

  for (const [moduleName, moduleData] of Object.entries(data.modules)) {
    // Each module has declarations array
    const declarations = moduleData.declarations || [];

    result.push({
      name: moduleName,
      declarations: declarations.map(decl => ({
        title: decl.title || "",
        kind: decl.kind || "value"
      }))
    });
  }

  return result;
}
