// FFI for parsing nations.json data

// Import the actual PureScript Region constructors
import * as Data from './Data.purs';

// Map region names from the JSON to our PureScript Region type
// Use the actual PureScript ADT constructors, not fake objects
function parseRegion(regionName) {
  switch (regionName) {
    case "East Asia & Pacific":
      return Data.eastAsiaAndPacific;
    case "Europe & Central Asia":
      return Data.europe;
    case "Latin America & Caribbean":
      return Data.latinAmericaAndCaribbean;
    case "Middle East & North Africa":
      return Data.middleEastAndNorthAfrica;
    case "South Asia":
      return Data.southAsia;
    case "Sub-Saharan Africa":
      return Data.subSaharanAfrica;
    case "North America":
    case "America":  // Handle both "America" and "North America"
      return Data.northAmerica;
    default:
      // Default to Sub-Saharan Africa if unknown
      console.warn("Unknown region:", regionName, "defaulting to SubSaharanAfrica");
      return Data.subSaharanAfrica;
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
