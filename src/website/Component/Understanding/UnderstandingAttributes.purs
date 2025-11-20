module Component.Understanding.UnderstandingAttributes where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.Mermaid (mermaidDiagram, triggerMermaidRendering)
import PSD3.Website.Types (Route(..))
import PSD3.RoutingDSL (routeToPath)

-- | State for the Attributes page
type State = Unit

-- | Actions for the Attributes page
data Action = Initialize

-- | Attributes page component
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
  Initialize -> liftEffect triggerMermaidRendering

-- | Mermaid diagram showing attribute type system
attributeTypeDiagram :: String
attributeTypeDiagram = """
graph TD
    subgraph "Attribute Values"
        Static["Static Value<br/><code>cx 100.0</code>"]
        Datum["Datum Function<br/><code>cx (\\d -> d.x)</code>"]
        DatumIdx["Datum+Index<br/><code>cx (\\d i -> d.x + i*10)</code>"]
    end

    subgraph "Type Resolution"
        IsAttr[IsAttribute typeclass]
        Resolve[Resolved at compile time]
    end

    Static --> IsAttr
    Datum --> IsAttr
    DatumIdx --> IsAttr
    IsAttr --> Resolve

    style Static fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style Datum fill:#e8dcc6,stroke:#8b7355,stroke-width:2px
    style DatumIdx fill:#d4c4b0,stroke:#8b7355,stroke-width:2px
"""

-- | Mermaid diagram showing Contravariant attribute pattern
contravariantDiagram :: String
contravariantDiagram = """
graph LR
    subgraph "Contravariant Pattern"
        Attr["Attribute datum"]
        CMap["cmap f attr"]
        AttrNew["Attribute newDatum"]
    end

    Attr --> CMap
    CMap --> AttrNew

    note["f :: newDatum -> datum<br/>Transforms data BEFORE attribute sees it"]

    style Attr fill:#e8dcc6,stroke:#8b7355,stroke-width:2px
    style CMap fill:#d4d8e8,stroke:#555b8b,stroke-width:2px
    style AttrNew fill:#c4e8d3,stroke:#558b73,stroke-width:2px
"""

-- | Mermaid diagram showing attribute composition
compositionDiagram :: String
compositionDiagram = """
graph TB
    subgraph "Attribute Composition"
        A[Array of Attribute datum]
        B[Apply to Selection]
        C[Type-safe styling]
    end

    A --> B
    B --> C

    style A fill:#f5e6d3,stroke:#8b7355,stroke-width:2px
    style B fill:#e8dcc6,stroke:#8b7355,stroke-width:2px
    style C fill:#d4c4b0,stroke:#8b7355,stroke-width:2px
"""

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "tutorial-page" ] ]
    [ -- Header with navigation
      renderHeader

    , HH.main_
        [ -- Introduction
          HH.section
            [ HP.classes [ HH.ClassName "tutorial-section", HH.ClassName "tutorial-intro" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "tutorial-title" ] ]
                [ HH.text "Type-Safe Attribute System" ]
            , HH.p_
                [ HH.text "A key innovation of D3 is the natural way to go from static to data-driven attributes. PSD3v2 preserves this flexibility while adding compile-time type safety through typeclasses and phantom types." ]

            -- Section navigation cards
            , HH.div
                [ HP.classes [ HH.ClassName "section-nav-cards" ] ]
                [ renderNavCard "Attribute Variants" "#attribute-variants" "Static, datum, and indexed"
                , renderNavCard "Contravariant Pattern" "#contravariant" "Transform data before attributes"
                , renderNavCard "Type Safety" "#type-safety" "Compile-time guarantees"
                , renderNavCard "Common Attributes" "#common-attributes" "SVG and HTML attributes"
                ]
            ]

        -- Section 1: Attribute Variants
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "attribute-variants"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Attribute Variants" ]
            , HH.p_
                [ HH.text "Attributes in PSD3v2 can take three forms, all with the same clean syntax:" ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram attributeTypeDiagram (Just "attribute-type-diagram") ]

            , HH.table
                [ HP.classes [ HH.ClassName "tutorial-table" ] ]
                [ HH.thead_
                    [ HH.tr_
                        [ HH.th_ [ HH.text "Form" ]
                        , HH.th_ [ HH.text "Example" ]
                        , HH.th_ [ HH.text "Use Case" ]
                        ]
                    ]
                , HH.tbody_
                    [ HH.tr_
                        [ HH.td_ [ HH.strong_ [ HH.text "Static" ] ]
                        , HH.td_ [ HH.code_ [ HH.text "radius 10.0" ] ]
                        , HH.td_ [ HH.text "Same value for all elements" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.strong_ [ HH.text "Datum Function" ] ]
                        , HH.td_ [ HH.code_ [ HH.text "cx (\\d -> d.x)" ] ]
                        , HH.td_ [ HH.text "Value depends on bound data" ]
                        ]
                    , HH.tr_
                        [ HH.td_ [ HH.strong_ [ HH.text "Datum + Index" ] ]
                        , HH.td_ [ HH.code_ [ HH.text "x (\\d i -> 50.0 + i * 100.0)" ] ]
                        , HH.td_ [ HH.text "Value depends on data and position" ]
                        ]
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Key: "
                , HH.text "The "
                , HH.code_ [ HH.text "IsAttribute" ]
                , HH.text " typeclass resolves all three forms at compile time."
                ]
            ]

        -- Section 2: Contravariant Pattern
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "contravariant"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "The Contravariant Pattern" ]
            , HH.p_
                [ HH.text "Attributes form a "
                , HH.em_ [ HH.text "Contravariant Functor" ]
                , HH.text ". This means you can transform the input data type before the attribute sees it."
                ]

            , HH.div
                [ HP.classes [ HH.ClassName "diagram-container" ] ]
                [ mermaidDiagram contravariantDiagram (Just "contravariant-diagram") ]

            , HH.p_
                [ HH.text "Why contravariant? Regular functors transform "
                , HH.em_ [ HH.text "outputs" ]
                , HH.text ". Attributes transform "
                , HH.em_ [ HH.text "inputs" ]
                , HH.text " (the datum they receive)."
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Example: "
                , HH.code_ [ HH.text "cmap _.name (textContent identity)" ]
                , HH.text " extracts the name field before setting text content."
                ]
            ]

        -- Section 3: Type Safety
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "type-safety"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Type Safety Guarantees" ]
            , HH.p_
                [ HH.text "The attribute system provides several compile-time guarantees:" ]

            , HH.ul_
                [ HH.li_
                    [ HH.strong_ [ HH.text "Datum type matching" ]
                    , HH.text " - Attributes must match the selection's datum type"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "Attribute value types" ]
                    , HH.text " - "
                    , HH.code_ [ HH.text "radius" ]
                    , HH.text " requires Number, "
                    , HH.code_ [ HH.text "fill" ]
                    , HH.text " requires String"
                    ]
                , HH.li_
                    [ HH.strong_ [ HH.text "No missing attributes" ]
                    , HH.text " - Arrays of attributes are explicit and type-checked"
                    ]
                ]

            , HH.p_
                [ HH.text "Common compile-time errors:" ]

            , HH.ul_
                [ HH.li_
                    [ HH.code_ [ HH.text "radius \"10\"" ]
                    , HH.text " - Type error: expected Number, got String"
                    ]
                , HH.li_
                    [ HH.code_ [ HH.text "cx (\\d -> d.x)" ]
                    , HH.text " on wrong datum type - Type error: field 'x' not found"
                    ]
                ]

            , HH.p
                [ HP.classes [ HH.ClassName "code-highlight" ] ]
                [ HH.text "Philosophy: "
                , HH.text "Runtime errors in D3 become compile-time errors in PSD3v2."
                ]
            ]

        -- Section 4: Common Attributes
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ]
            , HP.id "common-attributes"
            ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Common Attributes" ]
            , HH.p_
                [ HH.text "PSD3v2 provides typed wrappers for all common SVG and HTML attributes:" ]

            , HH.h3_ [ HH.text "Positioning" ]
            , HH.p_
                [ HH.code_ [ HH.text "cx, cy, x, y, x1, y1, x2, y2" ]
                , HH.text " - coordinate positions"
                ]

            , HH.h3_ [ HH.text "Dimensions" ]
            , HH.p_
                [ HH.code_ [ HH.text "width, height, radius, rx, ry" ]
                , HH.text " - size attributes"
                ]

            , HH.h3_ [ HH.text "Styling" ]
            , HH.p_
                [ HH.code_ [ HH.text "fill, stroke, strokeWidth, fillOpacity, strokeOpacity" ]
                , HH.text " - colors and strokes"
                ]

            , HH.h3_ [ HH.text "Text" ]
            , HH.p_
                [ HH.code_ [ HH.text "textContent, textAnchor, fontSize, fontWeight" ]
                , HH.text " - text styling"
                ]

            , HH.h3_ [ HH.text "Transforms" ]
            , HH.p_
                [ HH.code_ [ HH.text "transform, translate, rotate, scale" ]
                , HH.text " - geometric transforms"
                ]

            , HH.h3_ [ HH.text "General" ]
            , HH.p_
                [ HH.code_ [ HH.text "class_, id_, d (for paths), viewBox" ]
                , HH.text " - common utilities"
                ]
            ]

        -- Summary
        , HH.section
            [ HP.classes [ HH.ClassName "tutorial-section" ] ]
            [ HH.h2
                [ HP.classes [ HH.ClassName "tutorial-section-title" ] ]
                [ HH.text "Summary" ]
            , HH.p_
                [ HH.text "PSD3v2's attribute system:" ]
            , HH.ul_
                [ HH.li_ [ HH.text "Three forms: static, datum function, datum+index" ]
                , HH.li_ [ HH.text "Resolved at compile time via IsAttribute typeclass" ]
                , HH.li_ [ HH.text "Contravariant functor for data transformation" ]
                , HH.li_ [ HH.text "Type-safe: datum types, value types, completeness" ]
                , HH.li_ [ HH.text "Comprehensive coverage of SVG and HTML attributes" ]
                ]
            ]
        ]
    ]

-- | Render a navigation card
renderNavCard :: forall w i. String -> String -> String -> HH.HTML w i
renderNavCard title href description =
  HH.a
    [ HP.href href
    , HP.classes [ HH.ClassName "section-nav-card" ]
    ]
    [ HH.h3_ [ HH.text title ]
    , HH.p_ [ HH.text description ]
    ]

-- | Render header with navigation
renderHeader :: forall w i. HH.HTML w i
renderHeader =
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
            [ HP.href "#/understanding"
            , HP.classes [ HH.ClassName "example-gallery-link" ]
            ]
            [ HH.text "Understanding" ]
        , HH.div
            [ HP.classes [ HH.ClassName "example-title-container" ] ]
            [ HH.h1
                [ HP.classes [ HH.ClassName "example-title" ] ]
                [ HH.text "Type-Safe Attributes" ]
            ]
        ]
    ]
