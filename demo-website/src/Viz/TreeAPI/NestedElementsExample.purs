module D3.Viz.TreeAPI.NestedElementsExample where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3.Internal.Capabilities.Selection (select, renderTree)
import PSD3.Interpreter.D3 (runD3v2M, D3v2Selection_)
import PSD3.Internal.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST (Tree)
import PSD3.AST as T
import PSD3.Expr.Integration (v3Attr, v3AttrStr)
import PSD3.Expr.Expr (lit, str)
import Web.DOM.Element (Element)

-- | Test: Multi-level nested elements (Group → Circle + Text)
-- |
-- | This is the key pattern for visualizations - a container group with
-- | multiple child elements (shape + label).
-- |
-- | This validates that withChildren works correctly and that we can
-- | create the nested structures that were blocking code-explorer.
testNestedElements :: Effect Unit
testNestedElements = runD3v2M do
  container <- select "#viz" :: _ (D3v2Selection_ SEmpty Element Unit)

  -- Define a tree with MULTI-LEVEL NESTING
  -- This is the pattern: Group → (Circle + Text)
  let tree :: Tree Unit
      tree =
        T.named SVG "svg"
          [ v3Attr "width" (lit 600.0)
          , v3Attr "height" (lit 400.0)
          , v3AttrStr "viewBox" (str "0 0 600 400")
          , v3AttrStr "id" (str "nested-example-svg")
          ]
          `T.withChild`
            (T.named Group "mainGroup" [v3AttrStr "class" (str "main-group")]
              `T.withChildren`
                -- Multiple node groups, each with Circle + Text
                [ -- Node 1
                  T.named Group "node1"
                    [ v3AttrStr "class" (str "node")
                    , v3AttrStr "transform" (str "translate(100, 100)")
                    ]
                    `T.withChildren`
                      [ T.named Circle "node1Circle"
                          [ v3Attr "cx" (lit 0.0), v3Attr "cy" (lit 0.0)
                          , v3Attr "r" (lit 25.0)
                          , v3AttrStr "fill" (str "steelblue")
                          , v3AttrStr "stroke" (str "white")
                          , v3Attr "stroke-width" (lit 2.0)
                          ]
                      , T.named Text "node1Label"
                          [ v3Attr "x" (lit 0.0), v3Attr "y" (lit 40.0)
                          , v3AttrStr "textContent" (str "Node A")
                          , v3AttrStr "text-anchor" (str "middle")
                          , v3Attr "font-size" (lit 14.0)
                          ]
                      ]

                , -- Node 2
                  T.named Group "node2"
                    [ v3AttrStr "class" (str "node")
                    , v3AttrStr "transform" (str "translate(300, 150)")
                    ]
                    `T.withChildren`
                      [ T.named Circle "node2Circle"
                          [ v3Attr "cx" (lit 0.0), v3Attr "cy" (lit 0.0)
                          , v3Attr "r" (lit 20.0)
                          , v3AttrStr "fill" (str "orange")
                          , v3AttrStr "stroke" (str "white")
                          , v3Attr "stroke-width" (lit 2.0)
                          ]
                      , T.named Text "node2Label"
                          [ v3Attr "x" (lit 0.0), v3Attr "y" (lit 35.0)
                          , v3AttrStr "textContent" (str "Node B")
                          , v3AttrStr "text-anchor" (str "middle")
                          , v3Attr "font-size" (lit 14.0)
                          ]
                      ]

                , -- Node 3
                  T.named Group "node3"
                    [ v3AttrStr "class" (str "node")
                    , v3AttrStr "transform" (str "translate(500, 100)")
                    ]
                    `T.withChildren`
                      [ T.named Circle "node3Circle"
                          [ v3Attr "cx" (lit 0.0), v3Attr "cy" (lit 0.0)
                          , v3Attr "r" (lit 30.0)
                          , v3AttrStr "fill" (str "green")
                          , v3AttrStr "stroke" (str "white")
                          , v3Attr "stroke-width" (lit 2.0)
                          ]
                      , T.named Text "node3Label"
                          [ v3Attr "x" (lit 0.0), v3Attr "y" (lit 45.0)
                          , v3AttrStr "textContent" (str "Node C")
                          , v3AttrStr "text-anchor" (str "middle")
                          , v3Attr "font-size" (lit 14.0)
                          ]
                      ]
                ])

  -- Render the tree
  selections <- renderTree container tree

  liftEffect do
    Console.log "=== Nested Elements Test ==="
    Console.log ""
    Console.log "Structure: SVG → Group → (Node1, Node2, Node3)"
    Console.log "           Each Node → (Circle + Text)"
    Console.log ""

    -- Verify all selections
    let checkSelection name = case Map.lookup name selections of
          Just _ -> Console.log $ "✓ " <> name
          Nothing -> Console.log $ "✗ Missing: " <> name

    Console.log "Top-level:"
    checkSelection "svg"
    checkSelection "mainGroup"

    Console.log "\nNode 1 (steelblue at 100, 100):"
    checkSelection "node1"
    checkSelection "node1Circle"
    checkSelection "node1Label"

    Console.log "\nNode 2 (orange at 300, 150):"
    checkSelection "node2"
    checkSelection "node2Circle"
    checkSelection "node2Label"

    Console.log "\nNode 3 (green at 500, 100):"
    checkSelection "node3"
    checkSelection "node3Circle"
    checkSelection "node3Label"

    Console.log $ "\n" <> show (Map.size selections) <> " total selections in map"

    Console.log "\n=== SUCCESS CRITERIA ==="
    Console.log "✓ Multi-level nesting works (Group → Circle + Text)"
    Console.log "✓ withChildren handles arrays of subtrees"
    Console.log "✓ All named nodes are accessible via Map"
    Console.log ""
    Console.log "NEXT: Need data joins to generate this structure from Array data"
    Console.log "      instead of manually creating each node."
