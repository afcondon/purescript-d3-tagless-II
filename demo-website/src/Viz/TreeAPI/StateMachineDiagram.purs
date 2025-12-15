-- | State Machine Diagram Visualization
-- |
-- | Renders a state machine using the StateMachine layout from psd3-layout.
-- | This demonstrates the phantom type state machine used in PSD3's Selection API.
module D3.Viz.TreeAPI.StateMachineDiagram
  ( startStateMachine
  , phantomTypesMachine
  ) where

import Prelude

import Data.Array (concat)
import Effect (Effect)
import DataViz.Layout.StateMachine (layout, StateMachine, LayoutState, LayoutTransition, transitionPathD, arrowheadPathD, initialArrowPathD, stateEllipse, stateFinalRing)
import PSD3v3.Integration (v3Attr, v3AttrStr)
import PSD3v3.Expr (lit, str)
import PSD3v2.Capabilities.Selection (select, renderTree)
import PSD3v2.Interpreter.D3v2 (runD3v2M, D3v2Selection_)
import PSD3v2.Selection.Types (ElementType(..), SEmpty)
import PSD3.AST as T
import Web.DOM.Element (Element)

-- | The phantom type state machine for PSD3 Selection API
-- | Shows states as (SelectionStatus, ParentStatus) pairs
phantomTypesMachine :: StateMachine String
phantomTypesMachine =
  { states:
      [ { id: "start"
        , label: "Unselected"
        , isInitial: true
        , isFinal: false
        , extra: "No selection exists yet"
        }
      , { id: "selected"
        , label: "Selected\n(Bindable, Bindable)"
        , isInitial: false
        , isFinal: false
        , extra: "Selection exists, no data bound"
        }
      , { id: "bound"
        , label: "Data Bound\n(Binds, Bindable)"
        , isInitial: false
        , isFinal: false
        , extra: "Data bound, can enter/update"
        }
      , { id: "entered"
        , label: "Entered\n(Binds, Binds)"
        , isInitial: false
        , isFinal: false
        , extra: "In enter selection"
        }
      , { id: "rendered"
        , label: "Rendered"
        , isInitial: false
        , isFinal: true
        , extra: "Elements in DOM, Effect produced"
        }
      ]
  , transitions:
      [ { from: "start", to: "selected", label: "select" }
      , { from: "selected", to: "bound", label: "bindData" }
      , { from: "bound", to: "entered", label: "enter" }
      , { from: "entered", to: "entered", label: "attr / style" }
      , { from: "entered", to: "rendered", label: "interpret" }
      , { from: "bound", to: "bound", label: "update attrs" }
      ]
  }

-- | Render a state machine diagram
startStateMachine :: String -> Effect Unit
startStateMachine selector = do
  let machine = phantomTypesMachine
      laid = layout machine
      config = { width: laid.width, height: laid.height }

  runD3v2M do
    container <- select selector :: _ (D3v2Selection_ SEmpty Element Unit)

    let tree :: T.Tree Unit
        tree =
          T.named SVG "svg"
            [ v3Attr "width" (lit config.width)
            , v3Attr "height" (lit config.height)
            , v3AttrStr "viewBox" (str ("0 0 " <> show config.width <> " " <> show config.height))
            , v3AttrStr "class" (str "state-machine-diagram")
            , v3AttrStr "style" (str "font-family: monospace;")
            ]
            `T.withChildren`
              [ -- Defs for arrowhead marker
                T.elem Defs []
                  `T.withChild`
                    T.named Group "arrow-marker"
                      [ v3AttrStr "id" (str "arrowhead") ]

              -- Transitions layer (below states)
              , T.named Group "transitions"
                  [ v3AttrStr "class" (str "transitions") ]
                  `T.withChildren`
                    (map transitionElement laid.transitions)

              -- States layer
              , T.named Group "states"
                  [ v3AttrStr "class" (str "states") ]
                  `T.withChildren`
                    (map stateElement laid.states)

              -- Initial arrow
              , T.elem Group []
                  `T.withChildren`
                    [ T.elem Path
                        [ v3AttrStr "d" (str (initialArrowPathD laid.initialArrow 35.0))
                        , v3AttrStr "stroke" (str "#333")
                        , v3Attr "stroke-width" (lit 2.0)
                        , v3AttrStr "fill" (str "none")
                        , v3AttrStr "marker-end" (str "url(#arrowhead-marker)")
                        ]
                    , T.elem Path
                        [ v3AttrStr "d" (str (arrowheadPathD
                            (laid.initialArrow.x + 35.0)
                            laid.initialArrow.y
                            0.0
                            8.0))
                        , v3AttrStr "fill" (str "#333")
                        ]
                    ]
              ]

    _ <- renderTree container tree
    pure unit

-- | Create SVG elements for a state
stateElement :: LayoutState String -> T.Tree Unit
stateElement ls =
  let
    pos = stateEllipse ls.position
    isFinal = ls.state.isFinal
    isInitial = ls.state.isInitial
  in
    T.elem Group []
      `T.withChildren` concat
        [ -- Outer ellipse
          [ T.elem Circle
              [ v3Attr "cx" (lit pos.cx)
              , v3Attr "cy" (lit pos.cy)
              , v3Attr "r" (lit pos.rx)  -- Use rx as radius for circle
              , v3AttrStr "fill" (str (if isInitial then "#e8f5e9" else if isFinal then "#fff3e0" else "#e3f2fd"))
              , v3AttrStr "stroke" (str (if isInitial then "#4caf50" else if isFinal then "#ff9800" else "#2196f3"))
              , v3Attr "stroke-width" (lit 2.0)
              ]
          ]
        -- Inner ring for final states
        , if isFinal
            then
              let inner = stateFinalRing ls.position 5.0
              in [ T.elem Circle
                     [ v3Attr "cx" (lit inner.cx)
                     , v3Attr "cy" (lit inner.cy)
                     , v3Attr "r" (lit inner.rx)
                     , v3AttrStr "fill" (str "none")
                     , v3AttrStr "stroke" (str "#ff9800")
                     , v3Attr "stroke-width" (lit 2.0)
                     ]
                 ]
            else []
        -- Label
        , [ T.elem Text
              [ v3Attr "x" (lit pos.cx)
              , v3Attr "y" (lit pos.cy)
              , v3AttrStr "text-anchor" (str "middle")
              , v3AttrStr "dominant-baseline" (str "middle")
              , v3Attr "font-size" (lit 11.0)
              , v3AttrStr "fill" (str "#333")
              , v3AttrStr "textContent" (str ls.state.label)
              ]
          ]
        ]

-- | Create SVG elements for a transition
transitionElement :: LayoutTransition -> T.Tree Unit
transitionElement lt =
  let
    pathD = transitionPathD lt.path
    arrowD = arrowheadPathD lt.path.endX lt.path.endY lt.path.angle 8.0
  in
    T.elem Group []
      `T.withChildren`
        [ -- Arrow path
          T.elem Path
            [ v3AttrStr "d" (str pathD)
            , v3AttrStr "stroke" (str "#666")
            , v3Attr "stroke-width" (lit 1.5)
            , v3AttrStr "fill" (str "none")
            ]
        -- Arrowhead
        , T.elem Path
            [ v3AttrStr "d" (str arrowD)
            , v3AttrStr "fill" (str "#666")
            ]
        -- Label
        , T.elem Text
            [ v3Attr "x" (lit lt.path.labelX)
            , v3Attr "y" (lit lt.path.labelY)
            , v3AttrStr "text-anchor" (str "middle")
            , v3Attr "font-size" (lit 10.0)
            , v3AttrStr "fill" (str "#666")
            , v3AttrStr "font-style" (str "italic")
            , v3AttrStr "textContent" (str lt.transition.label)
            ]
        ]
