// FFI for parsing nations.json data

// Map region names from the JSON to standardized strings
// PureScript side will parse these to proper ADT constructors
function parseRegion(regionName) {
  switch (regionName) {
    case "East Asia & Pacific":
      return "EastAsiaAndPacific";
    case "Europe & Central Asia":
      return "Europe";
    case "Latin America & Caribbean":
      return "LatinAmericaAndCaribbean";
    case "Middle East & North Africa":
      return "MiddleEastAndNorthAfrica";
    case "South Asia":
      return "SouthAsia";
    case "Sub-Saharan Africa":
      return "SubSaharanAfrica";
    case "North America":
    case "America":  // Handle both "America" and "North America"
      return "NorthAmerica";
    default:
      // Return unknown string, PureScript will handle with Maybe
      console.warn("Unknown region:", regionName);
      return "Unknown";
  }
}

// Parse the nations.json file
export function parseNationsJSON(jsonString) {
  try {
    const rawData = JSON.parse(jsonString);

    // Transform the data to match our PureScript types
    return rawData.map(nation => ({
      name: nation.name,
      region: parseRegion(nation.region),
      income: nation.income || [],
      population: nation.population || [],
      lifeExpectancy: nation.lifeExpectancy || []
    }));
  } catch (error) {
    console.error("Failed to parse nations JSON:", error);
    return [];
  }
}
