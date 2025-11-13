module PSD3.Understanding.Hierarchies where -- understanding

import Prelude

import D3.Viz.Hierarchies (drawCirclePacking, drawIcicle, drawTreemap)
import D3.Viz.TreeVizOriented (draw, TreeOrientation(..)) as TreeViz
import D3.Viz.ClusterVizOriented (draw, ClusterOrientation(..)) as ClusterViz
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3.Data.Tree (TreeJson_)
import PSD3.Internal.Hierarchical (getTreeViaAJAX)
import PSD3.Internal.Utility (removeExistingSVG)
import PSD3.Interpreter.D3 (eval_D3M)
import PSD3.Shared.TutorialNav as TutorialNav
import PSD3.Website.Types (Route(..))

-- | Hierarchies page state
type State = {
  currentLayout :: HierarchyLayout,
  treeData :: Maybe TreeJson_
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
  | IsometricTidy
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
  show IsometricTidy = "Isometric Tidy Tree (2.5D)"
  show Icicle = "Icicle"
  show CirclePacking = "Circle Packing"
  show Treemap = "Treemap"

layoutDescription :: HierarchyLayout -> String
layoutDescription = case _ of
  HorizontalTidy -> "Node-link diagram: Compact tidy tree with left-to-right orientation. Root on the left, efficiently uses space. Pure PureScript Reingold-Tilford algorithm with horizontal projection."
  HorizontalDendrogram -> "Node-link diagram: Horizontal dendrogram with all leaves aligned at the same level on the right. Pure PureScript cluster algorithm with horizontal projection."
  VerticalTidy -> "Node-link diagram: Compact tidy tree with top-down orientation using pure PureScript Reingold-Tilford algorithm. Common in organizational charts."
  VerticalDendrogram -> "Node-link diagram: Vertical dendrogram with all leaves aligned at the same level at the bottom. Pure PureScript cluster algorithm."
  RadialTidy -> "Node-link diagram: Compact tidy tree in polar coordinates, emanating from center. Space-efficient for large hierarchies. Pure PureScript Reingold-Tilford with radial projection."
  RadialDendrogram -> "Node-link diagram: Radial dendrogram with all leaves at equal distance from center. Pure PureScript cluster algorithm with radial projection."
  IsometricTidy -> "Node-link diagram: Unique 2.5D isometric projection creates technical drawing aesthetic. Pure PureScript Reingold-Tilford with 30° cabinet projection for depth perception."
  Icicle -> "Adjacency diagram: Rectangular subdivisions showing hierarchy through relative placement. Area encodes quantitative values. Pure PureScript implementation."
  CirclePacking -> "Enclosure diagram: Tightly nested circles showing hierarchy through containment. Area encodes values, readily shows topology. Pure PureScript implementation."
  Treemap -> "Enclosure diagram: Space-efficient rectangular subdivisions. Area is proportional to value, shows hierarchy through nesting. Pure PureScript implementation."

-- | Hierarchies page actions
data Action
  = Initialize
  | SelectLayout HierarchyLayout

-- | Hierarchies page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> { currentLayout: VerticalTidy, treeData: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action () Aff
render state =
  HH.div
    [ HP.classes [ HH.ClassName "example-page" ] ]
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
                        , renderLayoutOption IsometricTidy "Isometric" state.currentLayout
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
    , TutorialNav.renderHeader Hierarchies

    -- Page content
    , HH.main
        [ HP.classes [ HH.ClassName "tutorial-content" ] ]
        [ -- Page introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "3. Hierarchical Layouts" ]
            , HH.p_
                [ HH.text "Hierarchical data structures are everywhere in computing: file systems, organizational charts, taxonomies, JSON documents, and abstract syntax trees. Different visualization layouts reveal different aspects of the same hierarchical data." ]
            , HH.p_
                [ HH.text "This page demonstrates nine different ways to visualize the same hierarchical dataset. Use the controls on the left to switch between layouts and explore how each representation emphasizes different relationships in the data. Also new in the examples on this page: the data visualization is zoomable and draggable within the white SVG rectangle. " ]
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

        -- Interactive exploration note
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Interactive Exploration" ]
            , HH.p_
                [ HH.text "This page itself is the interactive example! Use the controls on the left to switch between different hierarchical layouts and observe how the same data can be visualized in multiple ways. Each layout reveals different aspects of the hierarchical structure." ]
            , HH.p_
                [ HH.text "To explore the implementation of each layout, visit the "
                , HH.a
                    [ HP.href "#/examples"
                    , HP.classes [ HH.ClassName "tutorial-link" ]
                    ]
                    [ HH.text "Examples Gallery" ]
                , HH.text " where you can view the full source code for each visualization type."
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

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    -- Clear any existing viz
    _ <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.hierarchies-viz"

    -- Load Flare data
    treeJSON <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"

    -- Draw initial visualization
    case treeJSON of
      (Left _) -> pure unit
      (Right json) -> do
        _ <- H.liftAff $ drawLayoutViz VerticalTidy json
        H.modify_ (\st -> st { treeData = Just json } )
        pure unit

  SelectLayout layout -> do
    -- Clear existing viz
    _ <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.hierarchies-viz"

    -- Update state
    H.modify_ _ { currentLayout = layout }

    -- Redraw visualization with new layout
    state <- H.get
    case state.treeData of
      Nothing -> pure unit
      (Just json) -> do
        _ <- H.liftAff $ drawLayoutViz layout json
        pure unit

-- | Helper to call the appropriate draw function based on layout
-- | Uses parameterized TreeViz and ClusterViz for all orientations!
drawLayoutViz :: HierarchyLayout -> TreeJson_ -> Aff Unit
drawLayoutViz layout json =
  let selector = "div.hierarchies-viz"
  in case layout of
    -- Node-Link Diagrams (using new pure PS Tree and Cluster layouts with projection system)
    HorizontalTidy       -> liftEffect $ do
      (_ :: Unit) <- eval_D3M $ TreeViz.draw TreeViz.Horizontal json selector
      pure unit
    HorizontalDendrogram -> liftEffect $ do
      (_ :: Unit) <- eval_D3M $ ClusterViz.draw ClusterViz.Horizontal json selector
      pure unit
    VerticalTidy         -> liftEffect $ do
      (_ :: Unit) <- eval_D3M $ TreeViz.draw TreeViz.Vertical json selector
      pure unit
    VerticalDendrogram   -> liftEffect $ do
      (_ :: Unit) <- eval_D3M $ ClusterViz.draw ClusterViz.Vertical json selector
      pure unit
    RadialTidy           -> liftEffect $ do
      (_ :: Unit) <- eval_D3M $ TreeViz.draw TreeViz.Radial json selector
      pure unit
    RadialDendrogram     -> liftEffect $ do
      (_ :: Unit) <- eval_D3M $ ClusterViz.draw ClusterViz.Radial json selector
      pure unit
    IsometricTidy        -> liftEffect $ do
      (_ :: Unit) <- eval_D3M $ TreeViz.draw TreeViz.Isometric json selector
      pure unit
    -- Adjacency and Enclosure Diagrams (using new pure PS layouts)
    Icicle               -> drawIcicle json selector
    CirclePacking        -> drawCirclePacking json selector
    Treemap              -> drawTreemap json selector
