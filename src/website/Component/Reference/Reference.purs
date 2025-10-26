module PSD3.Reference.Reference where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Reference.ModuleRegistry (moduleCategories)
import PSD3.Shared.SectionNav as SectionNav
import PSD3.Understanding.TOC (renderTOC)
import PSD3.Website.Types (Route(..), Section(..))
import Type.Proxy (Proxy(..))

-- Import all module components
import PSD3.Reference.Modules.Types as Types
import PSD3.Reference.Modules.Attributes as Attributes
import PSD3.Reference.Modules.Capabilities.Selection as CapabilitiesSelection
import PSD3.Reference.Modules.Capabilities.Simulation as CapabilitiesSimulation
import PSD3.Reference.Modules.Capabilities.Sankey as CapabilitiesSankey
import PSD3.Reference.Modules.Interpreter.D3 as InterpreterD3
import PSD3.Reference.Modules.Interpreter.String as InterpreterString
import PSD3.Reference.Modules.Interpreter.MetaTree as InterpreterMetaTree
import PSD3.Reference.Modules.Data.Tree as DataTree
import PSD3.Reference.Modules.Data.Node as DataNode
import PSD3.Reference.Modules.Data.Utility as DataUtility
import PSD3.Reference.Modules.Internal.Selection.Types as InternalSelectionTypes
import PSD3.Reference.Modules.Internal.Selection.Functions as InternalSelectionFunctions
import PSD3.Reference.Modules.Internal.Simulation.Types as InternalSimulationTypes
import PSD3.Reference.Modules.Internal.Simulation.Functions as InternalSimulationFunctions
import PSD3.Reference.Modules.Internal.Simulation.Forces as InternalSimulationForces
import PSD3.Reference.Modules.Internal.Simulation.Config as InternalSimulationConfig
import PSD3.Reference.Modules.Internal.Types as InternalTypes
import PSD3.Reference.Modules.Internal.FFI as InternalFFI
import PSD3.Reference.Modules.Internal.Attributes.Instances as InternalAttributesInstances
import PSD3.Reference.Modules.Internal.Attributes.Sugar as InternalAttributesSugar
import PSD3.Reference.Modules.Internal.Axes as InternalAxes
import PSD3.Reference.Modules.Internal.Hierarchical as InternalHierarchical
import PSD3.Reference.Modules.Internal.Generators.Line as InternalGeneratorsLine
import PSD3.Reference.Modules.Internal.Scales.Scales as InternalScalesScales
import PSD3.Reference.Modules.Internal.Scales.Linear as InternalScalesLinear
import PSD3.Reference.Modules.Internal.Sankey.Types as InternalSankeyTypes
import PSD3.Reference.Modules.Internal.Sankey.Functions as InternalSankeyFunctions
import PSD3.Reference.Modules.Internal.Utility as InternalUtility
import PSD3.Reference.Modules.Internal.Zoom as InternalZoom

-- | Input from parent (receives current route)
type Input = Route

-- | Reference page state
type State =
  { currentRoute :: Route
  }

-- | Reference page actions
data Action
  = Initialize
  | Receive Route

-- | Child component slots
type Slots =
  ( sectionNav :: forall q. H.Slot q Void Unit
  , moduleComponent :: forall q. H.Slot q Void String
  )

_sectionNav = Proxy :: Proxy "sectionNav"
_moduleComponent = Proxy :: Proxy "moduleComponent"

-- | Reference page component
component :: forall q o. H.Component q Input o Aff
component = H.mkComponent
  { initialState: \route ->
      { currentRoute: route
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "reference-page" ] ]
    [ -- TOC Panel (LHS)
      renderTOC
        { title: "Page Contents"
        , items: []  -- Empty for now, will be populated when we add anchors to module pages
        , image: Just "images/reference-bookmark-deepseavent.jpeg"
        }

    -- Section Navigation (RHS) with module categories
    , HH.slot_ _sectionNav unit SectionNav.component
        { currentSection: APISection
        , currentRoute: state.currentRoute
        , sectionPages: []  -- No pages, we use module categories instead
        , moduleCategories: Just $ map toSectionNavCategory moduleCategories
        }

    -- Main content area
    , HH.main
        [ HP.classes [ HH.ClassName "reference-main" ] ]
        [ case state.currentRoute of
            Reference ->
              -- Default to showing PSD3.Capabilities.Selection (core of the library)
              renderModuleComponent "PSD3.Capabilities.Selection"
            ReferenceModule moduleName ->
              -- Route to specific module component
              renderModuleComponent moduleName
            _ ->
              -- Shouldn't happen, but handle gracefully
              renderModuleComponent "PSD3.Capabilities.Selection"
        ]
    ]

-- | Render the Reference landing page
renderReferenceLanding :: H.ComponentHTML Action Slots Aff
renderReferenceLanding =
  HH.section
    [ HP.classes [ HH.ClassName "reference-intro" ] ]
    [ HH.h1
        [ HP.classes [ HH.ClassName "reference-title" ] ]
        [ HH.text "PS<$>D3 API Reference" ]
    , HH.p
        [ HP.classes [ HH.ClassName "reference-description" ] ]
        [ HH.text "Browse the PSD3 library source code module by module. Select a module from the right panel to view its documentation and source code." ]
    ]

-- | Render the appropriate module component based on module name
renderModuleComponent :: String -> H.ComponentHTML Action Slots Aff
renderModuleComponent moduleName =
  case moduleName of
    "PSD3.Types" ->
      HH.slot _moduleComponent moduleName Types.component unit absurd
    "PSD3.Attributes" ->
      HH.slot _moduleComponent moduleName Attributes.component unit absurd
    "PSD3.Capabilities.Selection" ->
      HH.slot _moduleComponent moduleName CapabilitiesSelection.component unit absurd
    "PSD3.Capabilities.Simulation" ->
      HH.slot _moduleComponent moduleName CapabilitiesSimulation.component unit absurd
    "PSD3.Capabilities.Sankey" ->
      HH.slot _moduleComponent moduleName CapabilitiesSankey.component unit absurd
    "PSD3.Interpreter.D3" ->
      HH.slot _moduleComponent moduleName InterpreterD3.component unit absurd
    "PSD3.Interpreter.String" ->
      HH.slot _moduleComponent moduleName InterpreterString.component unit absurd
    "PSD3.Interpreter.MetaTree" ->
      HH.slot _moduleComponent moduleName InterpreterMetaTree.component unit absurd
    "PSD3.Data.Tree" ->
      HH.slot _moduleComponent moduleName DataTree.component unit absurd
    "PSD3.Data.Node" ->
      HH.slot _moduleComponent moduleName DataNode.component unit absurd
    "PSD3.Data.Utility" ->
      HH.slot _moduleComponent moduleName DataUtility.component unit absurd
    "PSD3.Internal.Selection.Types" ->
      HH.slot _moduleComponent moduleName InternalSelectionTypes.component unit absurd
    "PSD3.Internal.Selection.Functions" ->
      HH.slot _moduleComponent moduleName InternalSelectionFunctions.component unit absurd
    "PSD3.Internal.Simulation.Types" ->
      HH.slot _moduleComponent moduleName InternalSimulationTypes.component unit absurd
    "PSD3.Internal.Simulation.Functions" ->
      HH.slot _moduleComponent moduleName InternalSimulationFunctions.component unit absurd
    "PSD3.Internal.Simulation.Forces" ->
      HH.slot _moduleComponent moduleName InternalSimulationForces.component unit absurd
    "PSD3.Internal.Simulation.Config" ->
      HH.slot _moduleComponent moduleName InternalSimulationConfig.component unit absurd
    "PSD3.Internal.Types" ->
      HH.slot _moduleComponent moduleName InternalTypes.component unit absurd
    "PSD3.Internal.FFI" ->
      HH.slot _moduleComponent moduleName InternalFFI.component unit absurd
    "PSD3.Internal.Attributes.Instances" ->
      HH.slot _moduleComponent moduleName InternalAttributesInstances.component unit absurd
    "PSD3.Internal.Attributes.Sugar" ->
      HH.slot _moduleComponent moduleName InternalAttributesSugar.component unit absurd
    "PSD3.Internal.Axes" ->
      HH.slot _moduleComponent moduleName InternalAxes.component unit absurd
    "PSD3.Internal.Hierarchical" ->
      HH.slot _moduleComponent moduleName InternalHierarchical.component unit absurd
    "PSD3.Internal.Generators.Line" ->
      HH.slot _moduleComponent moduleName InternalGeneratorsLine.component unit absurd
    "PSD3.Internal.Scales.Scales" ->
      HH.slot _moduleComponent moduleName InternalScalesScales.component unit absurd
    "PSD3.Internal.Scales.Linear" ->
      HH.slot _moduleComponent moduleName InternalScalesLinear.component unit absurd
    "PSD3.Internal.Sankey.Types" ->
      HH.slot _moduleComponent moduleName InternalSankeyTypes.component unit absurd
    "PSD3.Internal.Sankey.Functions" ->
      HH.slot _moduleComponent moduleName InternalSankeyFunctions.component unit absurd
    "PSD3.Internal.Utility" ->
      HH.slot _moduleComponent moduleName InternalUtility.component unit absurd
    "PSD3.Internal.Zoom" ->
      HH.slot _moduleComponent moduleName InternalZoom.component unit absurd
    _ ->
      -- Unknown module
      HH.div
        [ HP.classes [ HH.ClassName "reference-not-found" ] ]
        [ HH.h2_ [ HH.text "Module Not Found" ]
        , HH.p_ [ HH.text $ "The module \"" <> moduleName <> "\" was not found." ]
        ]

-- | Convert ModuleRegistry category to SectionNav category format
toSectionNavCategory :: { title :: String, modules :: Array { name :: String, path :: String, description :: String } }
                     -> { title :: String, modules :: Array { name :: String, description :: String } }
toSectionNavCategory category =
  { title: category.title
  , modules: map (\m -> { name: m.name, description: m.description }) category.modules
  }

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit

  Receive route -> do
    H.modify_ _ { currentRoute = route }
