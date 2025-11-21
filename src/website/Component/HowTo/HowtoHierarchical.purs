module Component.HowTo.HowtoHierarchical where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.RoutingDSL (routeToPath)
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))

type State = Unit

data Action = Initialize

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> pure unit

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.QuadHowTo
        , prevNext: Nothing
        , pageTitle: Nothing
        }

    , HH.main_
        [ -- Intro
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Working with Hierarchical Data" ]
            , HH.p_
                [ HH.text "PSD3 supports D3's hierarchy layouts for trees, clusters, and treemaps." ]
            ]

        -- Creating Hierarchy
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Creating Hierarchy" ]

            , HH.p_ [ HH.text "Convert JSON to d3.hierarchy via FFI:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- JavaScript FFI
export const hierarchy_ = data => () => d3.hierarchy(data);
export const descendants_ = root => root.descendants();
export const links_ = root => root.links();

-- PureScript
foreign import hierarchy_ :: Json -> Effect HierarchyNode
foreign import descendants_ :: HierarchyNode -> Array HierarchyNode
foreign import links_ :: HierarchyNode -> Array HierarchyLink

root <- liftEffect $ hierarchy_ jsonData
let nodes = descendants_ root
let links = links_ root""" ]
                ]
            ]

        -- Layouts
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Layout Functions" ]

            , HH.p_ [ HH.text "Apply layouts to compute positions:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Tree layout (neat spacing)
export const tree_ = width => height => () =>
  d3.tree().size([height, width]);

-- Cluster layout (leaves aligned)
export const cluster_ = width => height => () =>
  d3.cluster().size([height, width]);

-- Treemap layout (space-filling)
export const treemap_ = width => height => () =>
  d3.treemap().size([width, height]).padding(1);

-- Apply layout to hierarchy
export const applyLayout_ = layout => root => () => layout(root);""" ]
                ]

            , HH.p_ [ HH.text "After layout, nodes have x/y coordinates:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """treeLayout <- liftEffect $ tree_ width height
liftEffect $ applyLayout_ treeLayout root

-- Now render nodes at computed positions
circles <- append Circle
  [ cx (_.y)  -- Note: tree layout swaps x/y
  , cy (_.x)
  , radius 5.0
  ] nodeEnter""" ]
                ]
            ]

        -- Link Paths
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Drawing Links" ]

            , HH.p_ [ HH.text "Generate curved paths for tree connections:" ]
            , HH.pre
                [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Horizontal curved links (for tree layout)
export const linkHorizontal_ = d3.linkHorizontal()
  .x(d => d.y)
  .y(d => d.x);

-- PureScript
paths <- append Path
  [ d (\\link -> linkHorizontal_ link)
  , stroke "#999"
  , fill "none"
  ] linkEnter""" ]
                ]
            ]

        -- Key Points
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Key Points" ]
            , HH.ul_
                [ HH.li_ [ HH.strong_ [ HH.text "hierarchy()" ], HH.text " - Converts JSON to hierarchy nodes" ]
                , HH.li_ [ HH.strong_ [ HH.text "descendants()/links()" ], HH.text " - Get flat arrays for data binding" ]
                , HH.li_ [ HH.strong_ [ HH.text "tree/cluster layouts" ], HH.text " - Swap x/y in coordinates" ]
                , HH.li_ [ HH.strong_ [ HH.text "linkHorizontal/Vertical" ], HH.text " - Curved path generators" ]
                ]
            ]

        -- Real Example
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Real Example" ]
            , HH.p_ [ HH.text "See hierarchical layouts:" ]
            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "src/website/Viz/TreeAPI/LesMisTreeExample.purs" ]
                    , HH.text " - Tree visualization"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "src/website/Component/AnimatedTreeCluster.purs" ]
                    , HH.text " - Animated transitions"
                    ]
                ]
            ]
        ]
    ]

renderHeader :: forall w i. String -> HH.HTML w i
renderHeader title =
  HH.header
    [ HP.classes [ HH.ClassName "example-header" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "example-header-left" ] ]
        [ HH.a
            [ HP.href $ "#" <> routeToPath Home
            , HP.classes [ HH.ClassName "example-logo-link" ]
            ]
            [ HH.img
                [ HP.src "assets/psd3-logo-color.svg"
                , HP.alt "PSD3 Logo"
                , HP.classes [ HH.ClassName "example-logo" ]
                ]
            ]
        , HH.a
            [ HP.href "#/howto"
            , HP.classes [ HH.ClassName "example-gallery-link" ]
            ]
            [ HH.text "How-to" ]
        , HH.div
            [ HP.classes [ HH.ClassName "example-title-container" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "example-title" ] ]
                [ HH.text title ]
            ]
        ]
    ]
