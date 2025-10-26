module PSD3.Reference.Reference where -- Reference

import Prelude

import Data.Array (find, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import PSD3.Reference.ModuleRegistry (ModuleInfo, moduleCategories, allModules)
import PSD3.Reference.ModuleViewer as ModuleViewer
import PSD3.Shared.SectionNav as SectionNav
import PSD3.Website.Types (Route(..), Section(..))
import Type.Proxy (Proxy(..))

-- | Reference page state
type State =
  { selectedModule :: Maybe ModuleInfo
  }

-- | Reference page actions
data Action
  = Initialize
  | SelectModule ModuleInfo

-- | Child component slots
type Slots =
  ( sectionNav :: forall q. H.Slot q Void Unit
  , moduleViewer :: forall q. H.Slot q Void Unit
  )

_sectionNav = Proxy :: Proxy "sectionNav"
_moduleViewer = Proxy :: Proxy "moduleViewer"

-- | Reference page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ ->
      { selectedModule: allModules !! 0  -- Default to first module (PSD3.Types)
      }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "reference-page" ] ]
    [ -- Module List Panel (LHS)
      renderModuleList state.selectedModule

    -- Section Navigation (RHS)
    , HH.slot_ _sectionNav unit SectionNav.component
        { currentSection: APISection
        , currentRoute: Reference
        , sectionPages:
            [ { route: Reference, label: "API Reference" }
            ]
        }

    -- Main content area
    , HH.main
        [ HP.classes [ HH.ClassName "reference-main" ] ]
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "reference-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "reference-title" ] ]
                [ HH.text "PS<$>D3 API Reference" ]
            , HH.p
                [ HP.classes [ HH.ClassName "reference-description" ] ]
                [ HH.text "Browse the PSD3 library source code module by module. Select a module from the left panel to view its complete source." ]
            ]

        -- Module viewer
        , case state.selectedModule of
            Nothing ->
              HH.div
                [ HP.classes [ HH.ClassName "reference-no-selection" ] ]
                [ HH.text "Select a module from the list to view its source code." ]
            Just moduleInfo ->
              HH.slot _moduleViewer unit ModuleViewer.component moduleInfo absurd
        ]
    ]

-- | Render the module list in the LHS panel
renderModuleList :: forall w. Maybe ModuleInfo -> HH.HTML w Action
renderModuleList selectedModule =
  HH.div
    [ HP.classes [ HH.ClassName "module-list-panel" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "module-list-panel__bookmark-pin" ] ]
        [ HH.img
            [ HP.src "images/reference-bookmark-deepseavent.jpeg"
            , HP.alt "Modules"
            ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "module-list-panel__main" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "module-list-panel__header" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "module-list-panel__title" ] ]
                [ HH.text "Modules" ]
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "module-list-panel__content" ] ]
            (map (renderCategory selectedModule) moduleCategories)
        ]
    ]

-- | Render a module category
renderCategory :: forall w. Maybe ModuleInfo -> _ -> HH.HTML w Action
renderCategory selectedModule category =
  HH.div
    [ HP.classes [ HH.ClassName "module-category" ] ]
    [ HH.h3
        [ HP.classes [ HH.ClassName "module-category__title" ] ]
        [ HH.text category.title ]
    , HH.ul
        [ HP.classes [ HH.ClassName "module-list" ] ]
        (map (renderModuleItem selectedModule) category.modules)
    ]

-- | Render a single module list item
renderModuleItem :: forall w. Maybe ModuleInfo -> ModuleInfo -> HH.HTML w Action
renderModuleItem selectedModule moduleInfo =
  HH.li
    [ HP.classes
        [ HH.ClassName "module-list__item"
        , HH.ClassName if isSelected then "module-list__item--selected" else ""
        ]
    ]
    [ HH.a
        [ HP.classes [ HH.ClassName "module-list__link" ]
        , HE.onClick \_ -> SelectModule moduleInfo
        , HP.href "#"
        ]
        [ HH.text moduleInfo.name ]
    ]
  where
    isSelected = case selectedModule of
      Nothing -> false
      Just selected -> selected.name == moduleInfo.name

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit

  SelectModule moduleInfo -> do
    H.modify_ _ { selectedModule = Just moduleInfo }
