module PSD3.Hierarchies where

import Prelude

import D3.Data.Tree (TreeJson_, TreeLayout(..), TreeModel, TreeType(..))
import D3.Layouts.Hierarchical (getTreeViaAJAX, makeModel)
import D3.Viz.Hierarchies (drawCirclePacking, drawIcicle, drawTreemap)
import D3.Viz.Tree.Configure as Tree
import D3Tagless.Instance.Selection (eval_D3M)
import D3Tagless.Utility (removeExistingSVG)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.RHSNavigation as RHSNav
import PSD3.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | Hierarchies page state
type State = {
  currentLayout :: HierarchyLayout,
  tree :: Maybe TreeModel
}

-- | Available hierarchy layouts
data HierarchyLayout
  -- Node-Link Diagrams
  = HorizontalTidy
  | HorizontalDendrogram
  | VerticalTidy
  | VerticalDendrogram
  | RadialTidy
  | RadialDendrogram
  -- Adjacency Diagrams
  | Icicle
  -- Enclosure Diagrams
  | CirclePacking
  | Treemap

derive instance eqHierarchyLayout :: Eq HierarchyLayout

instance showHierarchyLayout :: Show HierarchyLayout where
  show HorizontalTidy = "Horizontal Tidy Tree"
  show HorizontalDendrogram = "Horizontal Dendrogram"
  show VerticalTidy = "Vertical Tidy Tree"
  show VerticalDendrogram = "Vertical Dendrogram"
  show RadialTidy = "Radial Tidy Tree"
  show RadialDendrogram = "Radial Dendrogram"
  show Icicle = "Icicle"
  show CirclePacking = "Circle Packing"
  show Treemap = "Treemap"

layoutDescription :: HierarchyLayout -> String
layoutDescription = case _ of
  HorizontalTidy -> "Node-link diagram: Compact tidy tree with left-to-right orientation. Root on the left, efficiently uses space."
  HorizontalDendrogram -> "Node-link diagram: Horizontal dendrogram with all leaves aligned at the same level on the right."
  VerticalTidy -> "Node-link diagram: Compact tidy tree with top-down orientation. Common in organizational charts."
  VerticalDendrogram -> "Node-link diagram: Vertical dendrogram with all leaves aligned at the same level at the bottom."
  RadialTidy -> "Node-link diagram: Compact tidy tree in polar coordinates, emanating from center. Space-efficient for large hierarchies."
  RadialDendrogram -> "Node-link diagram: Radial dendrogram with all leaves at equal distance from center."
  Icicle -> "Adjacency diagram: Rectangular subdivisions showing hierarchy through relative placement. Area encodes quantitative values."
  CirclePacking -> "Enclosure diagram: Tightly nested circles showing hierarchy through containment. Area encodes values, readily shows topology."
  Treemap -> "Enclosure diagram: Space-efficient rectangular subdivisions. Area is proportional to value, shows hierarchy through nesting."

-- | Hierarchies page actions
data Action
  = Initialize
  | SelectLayout HierarchyLayout

-- | Child component slots
type Slots = ( rhsNav :: forall q. H.Slot q Void Unit )

_rhsNav = Proxy :: Proxy "rhsNav"

-- | Hierarchies page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> { currentLayout: HorizontalTidy, tree: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ -- Control Panel (LHS)
      HH.div
        [ HP.classes [ HH.ClassName "toc-panel", HH.ClassName "control-panel" ] ]
        [ HH.div
            [ HP.classes [ HH.ClassName "toc-panel__main", HH.ClassName "control-panel__main" ] ]
            [ HH.img
                [ HP.src "controller.jpeg"
                , HP.alt ""
                , HP.classes [ HH.ClassName "control-panel__icon" ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "floating-panel__header" ] ]
                [ HH.h3
                    [ HP.classes [ HH.ClassName "floating-panel__title" ] ]
                    [ HH.text "Layout Controls" ]
                , HH.button
                    [ HP.classes [ HH.ClassName "floating-panel__toggle" ]
                    , HP.type_ HP.ButtonButton
                    ]
                    [ HH.text "−" ]
                ]
            , HH.div
                [ HP.classes [ HH.ClassName "floating-panel__content", HH.ClassName "control-panel__content" ] ]
                [ HH.div
                    [ HP.classes [ HH.ClassName "control-panel__section" ] ]
                    [ HH.h4
                        [ HP.classes [ HH.ClassName "control-panel__section-title" ] ]
                        [ HH.text "Node-Link Diagrams" ]
                    , HH.div
                        [ HP.classes [ HH.ClassName "control-panel__options" ] ]
                        [ renderLayoutOption HorizontalTidy "H-Tidy" state.currentLayout
                        , renderLayoutOption HorizontalDendrogram "H-Dendro" state.currentLayout
                        , renderLayoutOption VerticalTidy "V-Tidy" state.currentLayout
                        , renderLayoutOption VerticalDendrogram "V-Dendro" state.currentLayout
                        , renderLayoutOption RadialTidy "R-Tidy" state.currentLayout
                        , renderLayoutOption RadialDendrogram "R-Dendro" state.currentLayout
                        ]
                    ]
                , HH.div
                    [ HP.classes [ HH.ClassName "control-panel__section" ] ]
                    [ HH.h4
                        [ HP.classes [ HH.ClassName "control-panel__section-title" ] ]
                        [ HH.text "Adjacency Diagrams" ]
                    , HH.div
                        [ HP.classes [ HH.ClassName "control-panel__options" ] ]
                        [ renderLayoutOption Icicle "Icicle" state.currentLayout
                        ]
                    ]
                , HH.div
                    [ HP.classes [ HH.ClassName "control-panel__section" ] ]
                    [ HH.h4
                        [ HP.classes [ HH.ClassName "control-panel__section-title" ] ]
                        [ HH.text "Enclosure Diagrams" ]
                    , HH.div
                        [ HP.classes [ HH.ClassName "control-panel__options" ] ]
                        [ renderLayoutOption CirclePacking "Circle Pack" state.currentLayout
                        , renderLayoutOption Treemap "Treemap" state.currentLayout
                        ]
                    ]
                , HH.div
                    [ HP.classes [ HH.ClassName "control-panel__current" ] ]
                    [ HH.strong_ [ HH.text "Current:" ]
                    , HH.text " "
                    , HH.text $ show state.currentLayout
                    ]
                ]
            ]
        ]

    -- Navigation Panel (RHS)
    , HH.slot_ _rhsNav unit RHSNav.component Hierarchies

    -- Page introduction
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
        [ HH.h1
            [ HP.classes [ HH.ClassName "tutorial-title" ] ]
            [ HH.text "Hierarchical Layouts" ]
        , HH.p_
            [ HH.text "Hierarchical data structures are everywhere in computing: file systems, organizational charts, taxonomies, JSON documents, and abstract syntax trees. Different visualization layouts reveal different aspects of the same hierarchical data." ]
        , HH.p_
            [ HH.text "This page demonstrates five different ways to visualize the same hierarchical dataset. Use the controls on the left to switch between layouts and explore how each representation emphasizes different relationships in the data." ]
        ]

    -- Visualization section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text $ show state.currentLayout ]
        , HH.p_
            [ HH.text $ layoutDescription state.currentLayout ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-viz-container" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "hierarchies-viz" ] ]
                [ renderLayoutPlaceholder state.currentLayout ]
            ]
        ]

    -- Code section
    , HH.section
        [ HP.classes [ HH.ClassName "tutorial-section" ] ]
        [ HH.h2
            [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
            [ HH.text "Implementation" ]
        , HH.p_
            [ HH.text "The visualization code for "
            , HH.strong_ [ HH.text $ show state.currentLayout ]
            , HH.text " demonstrates how D3's hierarchical layout algorithms transform tree data into visual coordinates."
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "tutorial-code-block" ] ]
            [ HH.pre_
                [ HH.code_
                    [ HH.text "-- Code for "
                    , HH.text $ show state.currentLayout
                    , HH.text " layout will go here\n"
                    , HH.text "-- Demonstrates D3 hierarchy layout with PureScript DSL"
                    ]
                ]
            ]
        ]
    ]

-- | Render a layout option button
renderLayoutOption :: forall w. HierarchyLayout -> String -> HierarchyLayout -> HH.HTML w Action
renderLayoutOption layout label currentLayout =
  HH.button
    [ HP.classes $ [ HH.ClassName "control-panel__option" ] <>
        if layout == currentLayout
        then [ HH.ClassName "control-panel__option--active" ]
        else []
    , HE.onClick $ const (SelectLayout layout)
    , HP.type_ HP.ButtonButton
    ]
    [ HH.text label ]

-- | Container for D3 visualization (D3 will attach to this)
renderLayoutPlaceholder :: forall w i. HierarchyLayout -> HH.HTML w i
renderLayoutPlaceholder _ =
  HH.div_ []  -- Empty div for D3 to populate

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Clear any existing viz
    _ <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.hierarchies-viz"

    -- Load Flare data
    treeJSON <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"

    -- Draw initial visualization
    case treeJSON of
      (Left err) -> pure unit
      (Right (json :: TreeJson_)) -> do
        model <- H.liftAff $ makeModel TidyTree Horizontal json
        _     <- H.liftAff $ drawLayoutViz HorizontalTidy model
        H.modify_ (\st -> st { tree = Just model } )
        pure unit

  SelectLayout layout -> do
    -- Clear existing viz
    _ <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.hierarchies-viz"

    -- Update state
    H.modify_ _ { currentLayout = layout }

    -- Redraw visualization with new layout
    state <- H.get
    case state.tree of
      Nothing -> pure unit
      (Just tree) -> do
        -- Redraw with new layout
        _ <- H.liftAff $ drawLayoutViz layout tree
        pure unit

-- | Helper to call the appropriate draw function based on layout
drawLayoutViz :: HierarchyLayout -> TreeModel -> Aff Unit
drawLayoutViz layout model =
  let selector = "div.hierarchies-viz"
  in case layout of
    -- Node-Link Diagrams
    HorizontalTidy -> do
      let updated = model { treeLayout = Horizontal, treeType = TidyTree }
      Tree.drawTree updated selector
    HorizontalDendrogram -> do
      let updated = model { treeLayout = Horizontal, treeType = Dendrogram }
      Tree.drawTree updated selector
    VerticalTidy -> do
      let updated = model { treeLayout = Vertical, treeType = TidyTree }
      Tree.drawTree updated selector
    VerticalDendrogram -> do
      let updated = model { treeLayout = Vertical, treeType = Dendrogram }
      Tree.drawTree updated selector
    RadialTidy -> do
      let updated = model { treeLayout = Radial, treeType = TidyTree }
      Tree.drawTree updated selector
    RadialDendrogram -> do
      let updated = model { treeLayout = Radial, treeType = Dendrogram }
      Tree.drawTree updated selector
    -- Adjacency Diagrams
    Icicle -> drawIcicle model.json selector
    -- Enclosure Diagrams
    CirclePacking -> drawCirclePacking model.json selector
    Treemap -> drawTreemap model.json selector
