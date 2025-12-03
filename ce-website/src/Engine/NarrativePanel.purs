-- | NarrativePanel - Contextual description of the current visualization
-- |
-- | A "What Is Being Shown" panel that updates its narrative based on view state.
-- | Editorial style - explains what you're seeing, offers interaction hints.
module Engine.NarrativePanel
  ( initNarrativePanel
  , setFullViewNarrative
  , setNeighborhoodNarrative
  , setDeclarationHoverNarrative
  , clearDeclarationHoverNarrative
  , setColorPalette
  , removeNarrativePanel
  , ColorEntry
  ) where

import Prelude

import Effect (Effect)

-- | A color entry for the palette
type ColorEntry = { name :: String, color :: String }

-- | FFI declarations
foreign import initNarrativePanel_ :: Effect Unit -> Effect Unit
foreign import setFullViewNarrative_ :: String -> Int -> Int -> Effect Unit
foreign import setNeighborhoodNarrative_ :: String -> Int -> Int -> Effect Unit
foreign import setDeclarationHoverNarrative_ :: String -> String -> Int -> Int -> Effect Unit
foreign import clearDeclarationHoverNarrative_ :: Effect Unit
foreign import setColorPalette_ :: Array ColorEntry -> Effect Unit
foreign import removeNarrativePanel_ :: Effect Unit

-- | Initialize the narrative panel with a back button callback
initNarrativePanel :: Effect Unit -> Effect Unit
initNarrativePanel onBack = initNarrativePanel_ onBack

-- | Update narrative for full view (packages + modules overview)
setFullViewNarrative :: String -> Int -> Int -> Effect Unit
setFullViewNarrative projectName moduleCount packageCount =
  setFullViewNarrative_ projectName moduleCount packageCount

-- | Update narrative for neighborhood view (focused on a module)
setNeighborhoodNarrative :: String -> Int -> Int -> Effect Unit
setNeighborhoodNarrative moduleName dependsOnCount dependedByCount =
  setNeighborhoodNarrative_ moduleName dependsOnCount dependedByCount

-- | Update hint text for declaration hover
setDeclarationHoverNarrative :: String -> String -> Int -> Int -> Effect Unit
setDeclarationHoverNarrative moduleName declName callsCount calledByCount =
  setDeclarationHoverNarrative_ moduleName declName callsCount calledByCount

-- | Clear declaration hover state
clearDeclarationHoverNarrative :: Effect Unit
clearDeclarationHoverNarrative = clearDeclarationHoverNarrative_

-- | Set the color palette for the color key
setColorPalette :: Array ColorEntry -> Effect Unit
setColorPalette = setColorPalette_

-- | Remove the narrative panel from DOM
removeNarrativePanel :: Effect Unit
removeNarrativePanel = removeNarrativePanel_
