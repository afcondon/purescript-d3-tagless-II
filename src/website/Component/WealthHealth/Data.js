// FFI for parsing nations.json data

// Map region names from the JSON to our PureScript Region type
function parseRegion(regionName) {
  switch (regionName) {
    case "East Asia & Pacific":
      return { constructor: "EastAsiaAndPacific" };
    case "Europe & Central Asia":
      return { constructor: "Europe" };
    case "Latin America & Caribbean":
      return { constructor: "LatinAmericaAndCaribbean" };
    case "Middle East & North Africa":
      return { constructor: "MiddleEastAndNorthAfrica" };
    case "South Asia":
      return { constructor: "SouthAsia" };
    case "Sub-Saharan Africa":
      return { constructor: "SubSaharanAfrica" };
    case "North America":
      return { constructor: "NorthAmerica" };
    default:
      // Default to Sub-Saharan Africa if unknown
      return { constructor: "SubSaharanAfrica" };
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
      lifeExpectancy: nation.life_expectancy || []
    }));
  } catch (error) {
    console.error("Failed to parse nations JSON:", error);
    return [];
  }
}
