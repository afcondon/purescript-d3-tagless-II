// FFI for parsing nations.json data

export function parseNationsJSON(jsonString) {
  try {
    return JSON.parse(jsonString);
  } catch (e) {
    console.error("Failed to parse nations JSON:", e);
    return [];
  }
}
