module PSD3.Understanding.Hierarchies where -- understanding

import Prelude

import D3.Viz.Hierarchies (drawCirclePacking, drawIcicle, drawTreemap)
import D3.Viz.Tree.Draw (draw) as Tree
import D3.Viz.Tree.Draw (treeDatum_)
import D3.Viz.Tree.Model (FlareTreeNode)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (abs, pi)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import PSD3 (class SelectionM, D3Selection_, Datum_, Selector, runD3M)
import PSD3.Data.Tree (TreeJson_, TreeLayout(..), TreeLayoutFn_, TreeModel, TreeType(..))
import PSD3.Internal.Attributes.Sugar (AlignAspectRatio_X(..), AlignAspectRatio_Y(..), AspectRatioPreserve(..), AspectRatioSpec(..), preserveAspectRatio, transform, viewBox)
import PSD3.Internal.FFI (getLayout, hNodeHeight_, hierarchyFromJSON_, runLayoutFn_, treeMinMax_, treeSetNodeSize_, treeSetSeparation_, treeSetSize_)
import PSD3.Internal.Hierarchical (getTreeViaAJAX, horizontalClusterLink, horizontalLink, makeModel, radialLink, radialSeparation, verticalClusterLink, verticalLink)
import PSD3.Internal.Scales.Scales (d3SchemeCategory10N_)
import PSD3.Internal.Utility (removeExistingSVG)
import PSD3.Interpreter.D3 (eval_D3M)
import PSD3.Shared.ExamplesNav as ExamplesNav
import PSD3.Website.Types (Route(..))
import Type.Proxy (Proxy(..))
import Utility (getWindowWidthHeight)

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
type Slots = ( examplesNav :: forall q. H.Slot q Void Unit )

_examplesNav = Proxy :: Proxy "examplesNav"

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
    , HH.slot_ _examplesNav unit ExamplesNav.component Hierarchies

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
      drawTree updated selector
    HorizontalDendrogram -> do
      let updated = model { treeLayout = Horizontal, treeType = Dendrogram }
      drawTree updated selector
    VerticalTidy -> do
      let updated = model { treeLayout = Vertical, treeType = TidyTree }
      drawTree updated selector
    VerticalDendrogram -> do
      let updated = model { treeLayout = Vertical, treeType = Dendrogram }
      drawTree updated selector
    RadialTidy -> do
      let updated = model { treeLayout = Radial, treeType = TidyTree }
      drawTree updated selector
    RadialDendrogram -> do
      let updated = model { treeLayout = Radial, treeType = Dendrogram }
      drawTree updated selector
    -- Adjacency Diagrams
    Icicle -> drawIcicle model.json selector
    -- Enclosure Diagrams
    CirclePacking -> drawCirclePacking model.json selector
    Treemap -> drawTreemap model.json selector

-- | Code from old TreeConfigure file, enables a single entry point to render any of the six forms
-- | TECHNICAL DEBT Something like this should be added to the library and this removed at some point
-- | Note the presence of a lot of library internal types and functions, doesn't belong in user code

-- | Evaluate the tree drawing script in the "d3" monad which will render it in SVG
-- TODO specialize runD3M so that this function isn't necessary
drawTree :: forall selection. TreeModel -> Selector selection -> Aff Unit
drawTree treeModel selector = liftEffect $ do
  widthHeight <- getWindowWidthHeight
  (_ :: Tuple D3Selection_ Unit) <- runD3M (configureAndRunScript widthHeight treeModel selector)
  pure unit


-- | configure function which enables Tree.script to be run for different layouts - WIP
configureAndRunScript :: forall m selection.
  Bind m =>
  MonadEffect m =>
  SelectionM selection m =>
  Tuple Number Number -> TreeModel -> Selector selection -> m selection
configureAndRunScript (Tuple width height ) model selector = 
  Tree.draw { spacing, viewbox, selector, linkPath, nodeTransform, color, layout: model.treeLayout, svg } laidOutRoot_
  where
    svg = { width, height }

    root    = hierarchyFromJSON_ model.json
    numberOfLevels = (hNodeHeight_ root) + 1.0
    spacing =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> { interChild: 10.0, interLevel: svg.width / numberOfLevels }
        Dendrogram, Vertical   -> { interChild: 10.0, interLevel: svg.height / numberOfLevels }
        Dendrogram, Radial     -> { interChild: 0.0,  interLevel: 0.0} -- not sure this is used in radial case

        TidyTree, Horizontal   -> { interChild: 10.0, interLevel: svg.width / numberOfLevels }
        TidyTree, Vertical     -> { interChild: 10.0, interLevel: svg.height / numberOfLevels}
        TidyTree, Radial       -> { interChild: 0.0,  interLevel: 0.0} -- not sure this is used in radial case

    classed =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> "dendrogram-horizontal"
        Dendrogram, Vertical   -> "dendrogram-vertical"
        Dendrogram, Radial     -> "dendrogram-radial"

        TidyTree, Horizontal   -> "tidytree-horizontal"
        TidyTree, Vertical     -> "tidytree-vertical"
        TidyTree, Radial       -> "tidytree-radial"

    layout :: TreeLayoutFn_
    layout = 
      if model.treeLayout == Radial
      then ((getLayout model.treeType)  `treeSetSize_`       [ 2.0 * pi, (svg.width / 2.0) - 100.0 ]) 
                                        `treeSetSeparation_` radialSeparation
      else
        (getLayout model.treeType)   `treeSetNodeSize_`   [ spacing.interChild, spacing.interLevel ]

    laidOutRoot_ :: FlareTreeNode
    laidOutRoot_ = layout `runLayoutFn_` root

    { xMin, xMax, yMin, yMax } = treeMinMax_ laidOutRoot_
    xExtent = abs $ xMax - xMin -- ie if tree spans from -50 to 200, it's extent is 250
    yExtent = abs $ yMax - yMin -- ie if tree spans from -50 to 200, it's extent is 250
    radialRadius = yMax  -- on the radial tree the y is the distance from origin, ie yMax == radius
    radialExtent       = 2.0 * radialRadius
    pad n = n * 1.2
    vtreeYOffset = (abs (height - yExtent)) / 2.0
    vtreeXOffset = xMin -- the left and right sides might be different so (xExtent / 2) would not necessarily be right
    htreeYOffset = xMin

    viewbox =
      case model.treeType, model.treeLayout of
        _, Vertical   -> [ viewBox vtreeXOffset (-vtreeYOffset) (pad xExtent) (pad yExtent) -- 
                         , preserveAspectRatio $ AspectRatio XMid YMid Meet ]
        _, Horizontal -> [ viewBox (-xExtent * 0.1) (pad htreeYOffset) (pad yExtent) (pad xExtent)
                         , preserveAspectRatio $ AspectRatio XMin YMid Meet ] -- x and y are reversed in horizontal layouts
        _, Radial     -> [ viewBox (-radialRadius * 1.2) (-radialRadius * 1.2)  (radialExtent * 1.2)    (radialExtent * 1.2)
                         , preserveAspectRatio $ AspectRatio XMin YMin Meet ]
      
    linkPath =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> horizontalClusterLink spacing.interLevel
        Dendrogram, Vertical   -> verticalClusterLink   spacing.interLevel 
        Dendrogram, Radial     -> radialLink treeDatum_.x treeDatum_.y

        TidyTree, Horizontal   -> horizontalLink
        TidyTree, Vertical     -> verticalLink
        TidyTree, Radial       -> radialLink treeDatum_.x treeDatum_.y

    nodeTransform =
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> [ transform [ positionXYreflected ] ]
        Dendrogram, Vertical   -> [ transform [ positionXY ] ]
        Dendrogram, Radial     -> [ transform [ radialRotateCommon, radialTranslate, rotateRadialLabels ] ]

        TidyTree, Horizontal   -> [ transform [ positionXYreflected ] ]
        TidyTree, Vertical     -> [ transform [ positionXY ] ]
        TidyTree, Radial       -> [ transform [ radialRotateCommon, radialTranslate, rotateRadialLabels ] ]

    color = d3SchemeCategory10N_ $
      case model.treeType, model.treeLayout of
        Dendrogram, Horizontal -> 1.0
        Dendrogram, Vertical   -> 2.0
        Dendrogram, Radial     -> 3.0

        TidyTree, Horizontal   -> 4.0
        TidyTree, Vertical     -> 5.0
        TidyTree, Radial       -> 6.0

radialRotate :: Number -> String
radialRotate x = show $ (x * 180.0 / pi - 90.0)

radialRotateCommon :: Datum_ -> String
radialRotateCommon d = "rotate(" <> radialRotate (treeDatum_.x d) <> ")"

radialTranslate :: Datum_ -> String
radialTranslate d = "translate(" <> show (treeDatum_.y d) <> ",0)"

rotateRadialLabels :: Datum_ -> String
rotateRadialLabels d = -- TODO replace with nodeIsOnRHS 
  "rotate(" <> 
    (if (treeDatum_.onRHS Radial d) 
    then "180"
    else "0")
    <> ")"

positionXYreflected :: Datum_ -> String  
positionXYreflected d = "translate("  <> show (treeDatum_.y d) <> "," <> show (treeDatum_.x d) <>")"

positionXY :: Datum_ -> String  
positionXY d = "translate(" <> show (treeDatum_.x d) <> "," <> show (treeDatum_.y d) <>")"