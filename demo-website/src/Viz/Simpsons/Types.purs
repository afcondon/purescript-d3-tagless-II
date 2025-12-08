-- | Simpson's Paradox Visualization - Data Types
-- |
-- | A PureScript port of the classic Simpson's Paradox visualization
-- | by Lewis Lehe & Victor Powell (2014): https://setosa.io/simpsons/
-- |
-- | Based on UC Berkeley graduate admissions data from 1973.
module D3.Viz.Simpsons.Types where

import Prelude

import Data.Int as Int

-- =============================================================================
-- Color Palette (Flat UI colors from original)
-- =============================================================================

blue :: String
blue = "#3498DB"

black :: String
black = "#2C3E50"

red :: String
red = "#E74C3C"

purple :: String
purple = "#9b59b6"

green :: String
green = "#1abc9c"

gray :: String
gray = "#bdc3c7"

-- =============================================================================
-- Gender Type
-- =============================================================================

data Gender = Male | Female

derive instance Eq Gender
derive instance Ord Gender

instance Show Gender where
  show Male = "Male"
  show Female = "Female"

genderLabel :: Gender -> String
genderLabel Male = "men"
genderLabel Female = "women"

genderColor :: Gender -> String
genderColor Male = purple
genderColor Female = green

-- =============================================================================
-- Acceptance Rates (constant, from Berkeley 1973 data)
-- =============================================================================

-- | Acceptance rates by gender and department difficulty
-- | These are fixed - the actual admission rates for each department type
type Rates =
  { male :: { hard :: Number, easy :: Number }
  , female :: { hard :: Number, easy :: Number }
  }

-- | The actual Berkeley admission rates
-- | Men: 25.6% for hard departments, 62.4% for easy
-- | Women: 26.5% for hard departments, 79.7% for easy
-- | Note: Women actually had HIGHER acceptance rates in both department types!
rates :: Rates
rates =
  { male: { hard: 0.256, easy: 0.624 }
  , female: { hard: 0.265, easy: 0.797 }
  }

getRate :: Gender -> String -> Number
getRate Male "hard" = rates.male.hard
getRate Male "easy" = rates.male.easy
getRate Female "hard" = rates.female.hard
getRate Female "easy" = rates.female.easy
getRate _ _ = 0.0

-- =============================================================================
-- Population (constant)
-- =============================================================================

-- | Total applicants by gender
type Population = { male :: Int, female :: Int }

population :: Population
population = { male: 2691, female: 1835 }

-- =============================================================================
-- Proportions (mutable state via sliders)
-- =============================================================================

-- | What proportion of each gender applied to the easy department
-- | The hard proportion is just (1 - easy)
-- | This is what the sliders control
type Proportions =
  { easyFemale :: Number -- 0.0 to 1.0
  , easyMale :: Number -- 0.0 to 1.0
  }

-- | Default proportions (from original)
-- | Women: 60% to easy, 40% to hard
-- | Men: 40% to easy, 60% to hard
-- | This is why men have lower OVERALL acceptance despite higher per-department rates
defaultProportions :: Proportions
defaultProportions =
  { easyFemale: 0.6
  , easyMale: 0.4
  }

-- =============================================================================
-- Derived Department Data
-- =============================================================================

-- | Computed stats for one gender in one department
type DeptStats =
  { applied :: Number
  , admitted :: Number
  }

-- | All department data (computed from proportions)
type Departments =
  { easy :: { male :: DeptStats, female :: DeptStats }
  , hard :: { male :: DeptStats, female :: DeptStats }
  }

-- | Calculate department stats from proportions
calculateDepartments :: Proportions -> Departments
calculateDepartments props =
  { easy:
      { male:
          { applied: Int.toNumber population.male * props.easyMale
          , admitted: Int.toNumber population.male * props.easyMale * rates.male.easy
          }
      , female:
          { applied: Int.toNumber population.female * props.easyFemale
          , admitted: Int.toNumber population.female * props.easyFemale * rates.female.easy
          }
      }
  , hard:
      { male:
          { applied: Int.toNumber population.male * (1.0 - props.easyMale)
          , admitted: Int.toNumber population.male * (1.0 - props.easyMale) * rates.male.hard
          }
      , female:
          { applied: Int.toNumber population.female * (1.0 - props.easyFemale)
          , admitted: Int.toNumber population.female * (1.0 - props.easyFemale) * rates.female.hard
          }
      }
  }

-- =============================================================================
-- Combined Acceptance Rates
-- =============================================================================

-- | Overall acceptance rate for a gender (computed)
type CombinedRates =
  { male :: Number
  , female :: Number
  }

-- | Calculate combined acceptance rates from departments
calculateCombined :: Departments -> CombinedRates
calculateCombined depts =
  { male: (depts.easy.male.admitted + depts.hard.male.admitted) / Int.toNumber population.male
  , female: (depts.easy.female.admitted + depts.hard.female.admitted) / Int.toNumber population.female
  }

-- | Check if we have a Simpson's Paradox
-- | Paradox occurs when men have higher COMBINED rate despite lower per-department rates
isParadox :: CombinedRates -> Boolean
isParadox combined = combined.male > combined.female

-- =============================================================================
-- Application State (for Halogen)
-- =============================================================================

-- | The full application state
type State =
  { proportions :: Proportions
  , isCombined :: Boolean -- Force-directed toggle
  }

-- | Initial state
initialState :: State
initialState =
  { proportions: defaultProportions
  , isCombined: false
  }

-- | Derived data from state
type DerivedData =
  { departments :: Departments
  , combined :: CombinedRates
  , isParadox :: Boolean
  }

-- | Calculate all derived data from proportions
deriveData :: Proportions -> DerivedData
deriveData props =
  let
    depts = calculateDepartments props
    comb = calculateCombined depts
  in
    { departments: depts
    , combined: comb
    , isParadox: isParadox comb
    }

-- =============================================================================
-- Top-level Donut Chart Data (constant - shows initial Berkeley numbers)
-- =============================================================================

-- | The famous 44% vs 35% acceptance rates that started the lawsuit
overallAcceptanceRates :: { male :: Number, female :: Number }
overallAcceptanceRates =
  { male: 0.44 -- 44% of male applicants accepted
  , female: 0.35 -- 35% of female applicants accepted
  }

-- =============================================================================
-- Force-Directed Data (6 largest departments)
-- =============================================================================

-- | Real Berkeley data for the 6 largest departments
-- | Format: [accepted, applied]
type DeptData = { accepted :: Int, applied :: Int }

-- | Department names (anonymized in original as A-F)
departmentNames :: Array String
departmentNames = [ "A", "B", "C", "D", "E", "F" ]

-- | Women's data by department
womenByDept :: Array DeptData
womenByDept =
  [ { accepted: 9, applied: 2 } -- A: 82%
  , { accepted: 2, applied: 1 } -- B: 68%
  , { accepted: 18, applied: 35 } -- C: 34%
  , { accepted: 13, applied: 23 } -- D: 35%
  , { accepted: 9, applied: 28 } -- E: 24%
  , { accepted: 2, applied: 29 } -- F: 7%
  ]

-- | Men's data by department
menByDept :: Array DeptData
menByDept =
  [ { accepted: 55, applied: 36 } -- A: 62%
  , { accepted: 33, applied: 21 } -- B: 63%
  , { accepted: 12, applied: 20 } -- C: 37%
  , { accepted: 14, applied: 30 } -- D: 33%
  , { accepted: 5, applied: 13 } -- E: 28%
  , { accepted: 2, applied: 25 } -- F: 6%
  ]
