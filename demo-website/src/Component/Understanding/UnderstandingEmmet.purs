module Component.Understanding.UnderstandingEmmet where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.SiteNav as SiteNav
import PSD3.Website.Types (Route(..))
import Type.Proxy (Proxy(..))

-- | Understanding Emmet Notation page state
type State = Unit

-- | Understanding Emmet Notation page actions
data Action = Initialize

-- | Child component slots
type Slots = ()

-- | Understanding Emmet Notation page component
component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> pure unit

render :: forall m. State -> H.ComponentHTML Action Slots m
render _ =
  HH.div [ HP.classes [ HH.ClassName "understanding-page" ] ]
    [ SiteNav.render
        { logoSize: SiteNav.Large
        , quadrant: SiteNav.QuadUnderstanding
        , prevNext: Just
            { prev: Just UnderstandingTreeAPI
            , next: Just UnderstandingScenes
            }
        , pageTitle: Just "Understanding: Emmet Notation"
        }

    , HH.main [ HP.classes [ HH.ClassName "understanding-content" ] ]
        [ HH.div [ HP.classes [ HH.ClassName "editorial-content" ] ]
            [ HH.h1 [ HP.classes [ HH.ClassName "understanding-title" ] ]
                [ HH.text "Emmet Notation for D3 Visualizations" ]

            , HH.p [ HP.classes [ HH.ClassName "lead" ] ]
                [ HH.text "A concise, declarative syntax for building D3 visualizations from data. Inspired by Emmet/Zen Coding for HTML, adapted for SVG graphics and data binding." ]

            -- What is Emmet Notation?
            , HH.h2_ [ HH.text "What is Emmet Notation?" ]

            , HH.p_
                [ HH.text "Emmet notation is a compact DSL (Domain-Specific Language) for describing visualization structures. Instead of writing verbose PureScript code to build DOM trees, you can write concise expressions like:" ]

            , HH.pre [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text "j(Point)>c[cx:x,cy:y,r=5,fill=steelblue]" ]
                ]

            , HH.p_
                [ HH.text "This single expression creates a complete scatter plot: join data points to circles, bind x/y coordinates, set radius and fill color." ]

            -- Why Use Emmet?
            , HH.h2_ [ HH.text "Why Use Emmet Notation?" ]

            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Concise: " ]
                    , HH.text "Express complex visualizations in a single line"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Declarative: " ]
                    , HH.text "Describe what you want, not how to build it"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Composable: " ]
                    , HH.text "Use operators (>, +, *) to build complex structures from simple parts"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Type-safe: " ]
                    , HH.text "Parsed and validated with helpful error messages"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Learnable: " ]
                    , HH.text "Small syntax, easy to remember, quick to write"
                    ]
                ]

            -- Core Concepts
            , HH.h2_ [ HH.text "Core Concepts" ]

            , HH.h3_ [ HH.text "Elements" ]
            , HH.p_
                [ HH.text "Six SVG element types, represented by single characters:" ]

            , HH.table [ HP.classes [ HH.ClassName "syntax-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "Char" ]
                        , HH.th_ [ HH.text "Element" ]
                        , HH.th_ [ HH.text "SVG" ]
                        , HH.th_ [ HH.text "Use For" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "g" ] ]
                        , HH.td_ [ HH.text "Group" ]
                        , HH.td_ [ HH.code_ [ HH.text "<g>" ] ]
                        , HH.td_ [ HH.text "Containers, transforms" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "c" ] ]
                        , HH.td_ [ HH.text "Circle" ]
                        , HH.td_ [ HH.code_ [ HH.text "<circle>" ] ]
                        , HH.td_ [ HH.text "Scatter plots, bubbles" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "r" ] ]
                        , HH.td_ [ HH.text "Rect" ]
                        , HH.td_ [ HH.code_ [ HH.text "<rect>" ] ]
                        , HH.td_ [ HH.text "Bar charts, grids" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "p" ] ]
                        , HH.td_ [ HH.text "Path" ]
                        , HH.td_ [ HH.code_ [ HH.text "<path>" ] ]
                        , HH.td_ [ HH.text "Lines, curves, shapes" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "l" ] ]
                        , HH.td_ [ HH.text "Line" ]
                        , HH.td_ [ HH.code_ [ HH.text "<line>" ] ]
                        , HH.td_ [ HH.text "Connections, links" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "t" ] ]
                        , HH.td_ [ HH.text "Text" ]
                        , HH.td_ [ HH.code_ [ HH.text "<text>" ] ]
                        , HH.td_ [ HH.text "Labels, annotations" ]
                        ]
                    ]
                ]

            , HH.h3_ [ HH.text "Attributes" ]
            , HH.p_
                [ HH.text "Three basic ways to specify attributes:" ]

            , HH.table [ HP.classes [ HH.ClassName "syntax-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "Syntax" ]
                        , HH.th_ [ HH.text "Type" ]
                        , HH.th_ [ HH.text "Example" ]
                        , HH.th_ [ HH.text "Meaning" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "name=value" ] ]
                        , HH.td_ [ HH.text "Static" ]
                        , HH.td_ [ HH.code_ [ HH.text "r=5" ] ]
                        , HH.td_ [ HH.text "Set radius to 5" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "name:field" ] ]
                        , HH.td_ [ HH.text "Field" ]
                        , HH.td_ [ HH.code_ [ HH.text "cx:x" ] ]
                        , HH.td_ [ HH.text "Bind cx to data.x" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "name@index" ] ]
                        , HH.td_ [ HH.text "Index" ]
                        , HH.td_ [ HH.code_ [ HH.text "x@index" ] ]
                        , HH.td_ [ HH.text "Use array index" ]
                        ]
                    ]
                ]

            , HH.h3_ [ HH.text "Opaque Attributes (Round-Trip Support)" ]
            , HH.p_
                [ HH.text "Emmet notation intentionally keeps computation in PureScript. For round-trip conversion (AST → Emmet → AST), complex attributes are preserved as \"opaque\" placeholders with metadata:" ]

            , HH.table [ HP.classes [ HH.ClassName "syntax-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "Syntax" ]
                        , HH.th_ [ HH.text "Type" ]
                        , HH.th_ [ HH.text "Example" ]
                        , HH.th_ [ HH.text "Represents" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "name:COMPUTED" ] ]
                        , HH.td_ [ HH.text "Opaque" ]
                        , HH.td_ [ HH.code_ [ HH.text "cx:COMPUTED" ] ]
                        , HH.td_ [ HH.text "Computed attribute (expression, function)" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "name:SCALE" ] ]
                        , HH.td_ [ HH.text "Opaque" ]
                        , HH.td_ [ HH.code_ [ HH.text "cy:SCALE" ] ]
                        , HH.td_ [ HH.text "Scale function (e.g., yScale)" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "name:CONDITIONAL" ] ]
                        , HH.td_ [ HH.text "Opaque" ]
                        , HH.td_ [ HH.code_ [ HH.text "fill:CONDITIONAL" ] ]
                        , HH.td_ [ HH.text "Conditional logic (if-then)" ]
                        ]
                    ]
                ]

            , HH.p_
                [ HH.text "These tokens mark where complex PureScript code exists. The actual computation is stored separately as metadata, allowing structural editing while preserving behavior. This enables:" ]

            , HH.ul_
                [ HH.li_ [ HH.text "Visual editing of AST structure without losing complex logic" ]
                , HH.li_ [ HH.text "Graphical reorganization of visualizations" ]
                , HH.li_ [ HH.text "Clear separation: Emmet for structure, PureScript for computation" ]
                ]

            , HH.h3_ [ HH.text "Data Joins" ]
            , HH.p_
                [ HH.text "Bind data arrays to elements, creating one element per datum:" ]

            , HH.pre [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text "j(Point)>c[cx:x,cy:y,r=5]" ]
                ]

            , HH.p_
                [ HH.text "Four join types:" ]

            , HH.ul_
                [ HH.li_ [ HH.code_ [ HH.text "j(Type)" ], HH.text " - Simple join" ]
                , HH.li_ [ HH.code_ [ HH.text "n(Type)" ], HH.text " - Nested join (array decomposition)" ]
                , HH.li_ [ HH.code_ [ HH.text "u(Type)" ], HH.text " - Update join (enter/update/exit)" ]
                , HH.li_ [ HH.code_ [ HH.text "x(Type)" ], HH.text " - Update nested join" ]
                ]

            , HH.h3_ [ HH.text "Operators" ]
            , HH.p_
                [ HH.text "Combine elements with four operators:" ]

            , HH.table [ HP.classes [ HH.ClassName "syntax-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "Op" ]
                        , HH.th_ [ HH.text "Name" ]
                        , HH.th_ [ HH.text "Example" ]
                        , HH.th_ [ HH.text "Meaning" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text ">" ] ]
                        , HH.td_ [ HH.text "Child" ]
                        , HH.td_ [ HH.code_ [ HH.text "g>c" ] ]
                        , HH.td_ [ HH.text "Group with circle child" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "+" ] ]
                        , HH.td_ [ HH.text "Sibling" ]
                        , HH.td_ [ HH.code_ [ HH.text "c+r" ] ]
                        , HH.td_ [ HH.text "Circle and rect as siblings" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "*N" ] ]
                        , HH.td_ [ HH.text "Multiply" ]
                        , HH.td_ [ HH.code_ [ HH.text "c*3" ] ]
                        , HH.td_ [ HH.text "Three circles" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.code_ [ HH.text "()" ] ]
                        , HH.td_ [ HH.text "Group" ]
                        , HH.td_ [ HH.code_ [ HH.text "(c+r)*2" ] ]
                        , HH.td_ [ HH.text "Two circle+rect pairs" ]
                        ]
                    ]
                ]

            -- Examples
            , HH.h2_ [ HH.text "Examples" ]

            , HH.h3_ [ HH.text "Scatter Plot" ]
            , HH.pre [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text "j(Point)>c[cx:x,cy:y,r=5,fill=steelblue]" ]
                ]
            , HH.p_ [ HH.text "Join points to circles, bind x/y coordinates, set radius and color." ]

            , HH.h3_ [ HH.text "Bar Chart" ]
            , HH.pre [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text "j(Point)>r[x@index,y:y,width=40,height:y,fill=steelblue]" ]
                ]
            , HH.p_ [ HH.text "Join points to rectangles, position by index, bind height to data." ]

            , HH.h3_ [ HH.text "Bubble Chart" ]
            , HH.pre [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text "j(Country)>c[cx:gdp,cy:lifeExpectancy,r:population,fill=coral]" ]
                ]
            , HH.p_ [ HH.text "Three-dimensional data: position by GDP and life expectancy, size by population." ]

            , HH.h3_ [ HH.text "2D Grid" ]
            , HH.pre [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text "n(Board)>g>n(Row)>g>j(Cell)>r[x@index,y@index,width=20,height=20]" ]
                ]
            , HH.p_ [ HH.text "Nested joins for hierarchical data: boards contain rows, rows contain cells." ]

            -- Interactive Demos
            , HH.h2_ [ HH.text "Interactive Learning Path" ]

            , HH.p_
                [ HH.text "Three demos progressively introduce Emmet notation:" ]

            , HH.div [ HP.classes [ HH.ClassName "demo-cards" ] ]
                [ -- Demo 1: AST Visualized
                  HH.div [ HP.classes [ HH.ClassName "demo-card" ] ]
                    [ HH.div [ HP.classes [ HH.ClassName "demo-number" ] ]
                        [ HH.text "1" ]
                    , HH.h3 [ HP.classes [ HH.ClassName "demo-title" ] ]
                        [ HH.text "AST Visualized" ]
                    , HH.p [ HP.classes [ HH.ClassName "demo-description" ] ]
                        [ HH.text "See what you're building. Visualize the Abstract Syntax Tree structure with interactive tree layouts. Understand how Emmet expressions map to visual structures." ]
                    , HH.a
                        [ HP.href "#/tree-builder3"
                        , HP.classes [ HH.ClassName "demo-link" ]
                        ]
                        [ HH.text "Explore AST Visualizer →" ]
                    ]

                , -- Demo 2: AST Notation
                  HH.div [ HP.classes [ HH.ClassName "demo-card" ] ]
                    [ HH.div [ HP.classes [ HH.ClassName "demo-number" ] ]
                        [ HH.text "2" ]
                    , HH.h3 [ HP.classes [ HH.ClassName "demo-title" ] ]
                        [ HH.text "AST Notation" ]
                    , HH.p [ HP.classes [ HH.ClassName "demo-description" ] ]
                        [ HH.text "Learn how to write it. Practice Emmet syntax with live parsing and validation. See example patterns and experiment with modifications." ]
                    , HH.a
                        [ HP.href "#/simple-tree-builder"
                        , HP.classes [ HH.ClassName "demo-link" ]
                        ]
                        [ HH.text "Practice Notation →" ]
                    ]

                , -- Demo 3: Chart Builder
                  HH.div [ HP.classes [ HH.ClassName "demo-card" ] ]
                    [ HH.div [ HP.classes [ HH.ClassName "demo-number" ] ]
                        [ HH.text "3" ]
                    , HH.h3 [ HP.classes [ HH.ClassName "demo-title" ] ]
                        [ HH.text "Chart Builder" ]
                    , HH.p [ HP.classes [ HH.ClassName "demo-description" ] ]
                        [ HH.text "Now build it yourself. Create real charts with Emmet recipes. Copy templates, modify attributes, and see results with live datasets." ]
                    , HH.a
                        [ HP.href "#/chart-builder"
                        , HP.classes [ HH.ClassName "demo-link" ]
                        ]
                        [ HH.text "Build Charts →" ]
                    ]
                ]

            -- Full Grammar Reference
            , HH.h2_ [ HH.text "Full Grammar Reference" ]

            , HH.p_
                [ HH.text "For complete grammar specification including BNF, all valid type names, operator precedence, and error messages, see: "
                , HH.a
                    [ HP.href "https://github.com/afcondon/purescript-d3-tagless-II/blob/main/notes/EMMET_GRAMMAR.md"
                    , HP.target "_blank"
                    ]
                    [ HH.text "EMMET_GRAMMAR.md" ]
                ]

            -- Advantages
            , HH.h2_ [ HH.text "Advantages Over Direct AST Construction" ]

            , HH.table [ HP.classes [ HH.ClassName "comparison-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "Emmet" ]
                        , HH.th_ [ HH.text "Direct PureScript" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_
                            [ HH.pre_
                                [ HH.code_ [ HH.text "j(Point)>c[cx:x,cy:y,r=5]" ] ]
                            ]
                        , HH.td_
                            [ HH.pre_
                                [ HH.code_ [ HH.text """joinData "circles" "circle" myData $ \d ->
  elem Circle
    [ cx d.x
    , cy d.y
    , radius 5.0
    ]""" ] ]
                            ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.text "14 characters" ]
                        , HH.td_ [ HH.text "~120 characters" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.text "1 line" ]
                        , HH.td_ [ HH.text "6 lines" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.text "Immediate visual scan" ]
                        , HH.td_ [ HH.text "Parse structure mentally" ]
                        ]
                    ]
                ]

            , HH.p_
                [ HH.text "Emmet notation is ideal for rapid prototyping, teaching, and sharing visualization patterns. For complex visualizations with custom logic, direct PureScript AST construction provides more flexibility." ]

            -- Round-Trip Conversion
            , HH.h2_ [ HH.text "Round-Trip Conversion (AST ↔ Emmet)" ]

            , HH.p_
                [ HH.text "The full PSD3 AST supports features that Emmet notation intentionally omits (computed attributes, scale functions, conditional logic, GUP behaviors). For visual structure editing while preserving these features, the system supports round-trip conversion:" ]

            , HH.pre [ HP.classes [ HH.ClassName "code-block" ] ]
                [ HH.code_
                    [ HH.text """-- Convert AST to Emmet + metadata
let emmetData = toEmmetWithMetadata myComplexAST

-- Emmet string: "j(Point)>c[cx:COMPUTED,cy:SCALE,fill:CONDITIONAL]"
-- Metadata: { opaque features at each node path }

-- Edit structure graphically, preserving opaque features
let edited = editStructure emmetData.emmetString

-- Convert back with metadata substitution
let newAST = fromEmmetWithMetadata { emmetData | emmetString = edited }""" ]
                ]

            , HH.h3_ [ HH.text "How It Works" ]
            , HH.ol_
                [ HH.li_
                    [ HH.strong_ [ HH.text "AST → Emmet: " ]
                    , HH.text "Complex attributes become opaque tokens (COMPUTED, SCALE, CONDITIONAL). The actual functions are stored in metadata, indexed by node path."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Edit Structure: " ]
                    , HH.text "Users can reorganize elements, add siblings, change nesting—all in Emmet notation."
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Emmet → AST: " ]
                    , HH.text "Parser recognizes opaque tokens, converter looks up metadata by path, original attributes are restored."
                    ]
                ]

            , HH.h3_ [ HH.text "Use Cases" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Visual AST editors that preserve computation" ]
                , HH.li_ [ HH.text "Graphical reorganization of complex visualizations" ]
                , HH.li_ [ HH.text "Teaching tool: show structure in Emmet, computation in PureScript" ]
                , HH.li_ [ HH.text "Export/import for structural templates with custom logic" ]
                ]

            -- When to Use
            , HH.h2_ [ HH.text "When to Use Emmet Notation" ]

            , HH.h3_ [ HH.text "Best For:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Rapid prototyping and experimentation" ]
                , HH.li_ [ HH.text "Teaching and learning D3 concepts" ]
                , HH.li_ [ HH.text "Sharing visualization patterns" ]
                , HH.li_ [ HH.text "Standard chart types (scatter, bar, line)" ]
                , HH.li_ [ HH.text "Interactive editors and builders" ]
                ]

            , HH.h3_ [ HH.text "Consider Direct AST For:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Complex custom logic or conditionals" ]
                , HH.li_ [ HH.text "Dynamic structure based on data" ]
                , HH.li_ [ HH.text "Highly interactive behaviors" ]
                , HH.li_ [ HH.text "Production applications requiring full type safety" ]
                ]

            -- Next Steps
            , HH.h2_ [ HH.text "Next Steps" ]

            , HH.ol_
                [ HH.li_
                    [ HH.text "Start with "
                    , HH.a [ HP.href "#/tree-builder3" ] [ HH.text "AST Visualized" ]
                    , HH.text " to see how expressions map to structures"
                    ]
                , HH.li_
                    [ HH.text "Practice syntax with "
                    , HH.a [ HP.href "#/simple-tree-builder" ] [ HH.text "AST Notation" ]
                    ]
                , HH.li_
                    [ HH.text "Build real charts with "
                    , HH.a [ HP.href "#/chart-builder" ] [ HH.text "Chart Builder" ]
                    ]
                , HH.li_
                    [ HH.text "Read the "
                    , HH.a
                        [ HP.href "https://github.com/afcondon/purescript-d3-tagless-II/blob/main/notes/EMMET_GRAMMAR.md"
                        , HP.target "_blank"
                        ]
                        [ HH.text "full grammar reference" ]
                    ]
                , HH.li_
                    [ HH.text "Explore "
                    , HH.a [ HP.href "#/understanding/tree-api" ] [ HH.text "TreeAPI" ]
                    , HH.text " for declarative visualization composition"
                    ]
                ]
            ]
        ]
    ]
