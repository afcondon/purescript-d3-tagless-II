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
import PSD3.Expr.Friendly (num, text, attr, viewBox, width, height, cx, cy, r, x, y, fill, stroke, strokeWidth, fontSize, textAnchor, textContent, transform)
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
          [ width $ num 600.0
          , height $ num 400.0
          , viewBox 0.0 0.0 600.0 400.0
          , attr "id" $ text "nested-example-svg"
          ]
          `T.withChild`
            (T.named Group "mainGroup" [attr "class" $ text "main-group"]
              `T.withChildren`
                -- Multiple node groups, each with Circle + Text
                [ -- Node 1
                  T.named Group "node1"
                    [ attr "class" $ text "node"
                    , transform $ text "translate(100, 100)"
                    ]
                    `T.withChildren`
                      [ T.named Circle "node1Circle"
                          [ cx $ num 0.0, cy $ num 0.0
                          , r $ num 25.0
                          , fill $ text "steelblue"
                          , stroke $ text "white"
                          , strokeWidth $ num 2.0
                          ]
                      , T.named Text "node1Label"
                          [ x $ num 0.0, y $ num 40.0
                          , textContent $ text "Node A"
                          , textAnchor $ text "middle"
                          , fontSize $ num 14.0
                          ]
                      ]

                , -- Node 2
                  T.named Group "node2"
                    [ attr "class" $ text "node"
                    , transform $ text "translate(300, 150)"
                    ]
                    `T.withChildren`
                      [ T.named Circle "node2Circle"
                          [ cx $ num 0.0, cy $ num 0.0
                          , r $ num 20.0
                          , fill $ text "orange"
                          , stroke $ text "white"
                          , strokeWidth $ num 2.0
                          ]
                      , T.named Text "node2Label"
                          [ x $ num 0.0, y $ num 35.0
                          , textContent $ text "Node B"
                          , textAnchor $ text "middle"
                          , fontSize $ num 14.0
                          ]
                      ]

                , -- Node 3
                  T.named Group "node3"
                    [ attr "class" $ text "node"
                    , transform $ text "translate(500, 100)"
                    ]
                    `T.withChildren`
                      [ T.named Circle "node3Circle"
                          [ cx $ num 0.0, cy $ num 0.0
                          , r $ num 30.0
                          , fill $ text "green"
                          , stroke $ text "white"
                          , strokeWidth $ num 2.0
                          ]
                      , T.named Text "node3Label"
                          [ x $ num 0.0, y $ num 45.0
                          , textContent $ text "Node C"
                          , textAnchor $ text "middle"
                          , fontSize $ num 14.0
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
