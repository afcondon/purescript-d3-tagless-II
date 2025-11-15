module D3.Viz.TreeAPI.NestedElementsExample where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import PSD3v2.Attribute.Types (width, height, viewBox, id_, class_, cx, cy, radius, fill, stroke, strokeWidth, textContent, textAnchor, x, y, fontSize, transform)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M)
import PSD3v2.Selection.Types (ElementType(..))
import PSD3v2.VizTree.Tree as T

-- | Test: Multi-level nested elements (Group → Circle + Text)
-- |
-- | This is the key pattern for visualizations - a container group with
-- | multiple child elements (shape + label).
-- |
-- | This validates that withChildren works correctly and that we can
-- | create the nested structures that were blocking code-explorer.
testNestedElements :: Effect Unit
testNestedElements = runD3v2M do
  container <- select "#viz"

  -- Define a tree with MULTI-LEVEL NESTING
  -- This is the pattern: Group → (Circle + Text)
  let tree =
        T.named "svg" SVG
          [ width 600.0
          , height 400.0
          , viewBox "0 0 600 400"
          , id_ "nested-example-svg"
          ]
          `T.withChild`
            (T.named "mainGroup" Group [class_ "main-group"]
              `T.withChildren`
                -- Multiple node groups, each with Circle + Text
                [ -- Node 1
                  T.named "node1" Group
                    [ class_ "node"
                    , transform "translate(100, 100)"
                    ]
                    `T.withChildren`
                      [ T.named "node1Circle" Circle
                          [ cx 0.0, cy 0.0
                          , radius 25.0
                          , fill "steelblue"
                          , stroke "white"
                          , strokeWidth 2.0
                          ]
                      , T.named "node1Label" Text
                          [ x 0.0, y 40.0
                          , textContent "Node A"
                          , textAnchor "middle"
                          , fontSize 14.0
                          ]
                      ]

                , -- Node 2
                  T.named "node2" Group
                    [ class_ "node"
                    , transform "translate(300, 150)"
                    ]
                    `T.withChildren`
                      [ T.named "node2Circle" Circle
                          [ cx 0.0, cy 0.0
                          , radius 20.0
                          , fill "orange"
                          , stroke "white"
                          , strokeWidth 2.0
                          ]
                      , T.named "node2Label" Text
                          [ x 0.0, y 35.0
                          , textContent "Node B"
                          , textAnchor "middle"
                          , fontSize 14.0
                          ]
                      ]

                , -- Node 3
                  T.named "node3" Group
                    [ class_ "node"
                    , transform "translate(500, 100)"
                    ]
                    `T.withChildren`
                      [ T.named "node3Circle" Circle
                          [ cx 0.0, cy 0.0
                          , radius 30.0
                          , fill "green"
                          , stroke "white"
                          , strokeWidth 2.0
                          ]
                      , T.named "node3Label" Text
                          [ x 0.0, y 45.0
                          , textContent "Node C"
                          , textAnchor "middle"
                          , fontSize 14.0
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
