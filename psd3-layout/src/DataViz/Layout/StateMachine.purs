-- | DataViz.Layout.StateMachine
-- |
-- | State machine layout for visualization.
-- | Provides types and layout algorithms for rendering state machine diagrams
-- | with states as circles/ovals and transitions as curved arrows.
-- |
-- | Example usage:
-- | ```purescript
-- | import DataViz.Layout.StateMachine (layout, StateMachine)
-- |
-- | myMachine :: StateMachine Unit
-- | myMachine =
-- |   { states:
-- |       [ { id: "s0", label: "Start", isInitial: true, isFinal: false, extra: unit }
-- |       , { id: "s1", label: "Running", isInitial: false, isFinal: false, extra: unit }
-- |       , { id: "s2", label: "Done", isInitial: false, isFinal: true, extra: unit }
-- |       ]
-- |   , transitions:
-- |       [ { from: "s0", to: "s1", label: "begin" }
-- |       , { from: "s1", to: "s1", label: "tick" }
-- |       , { from: "s1", to: "s2", label: "finish" }
-- |       ]
-- |   }
-- |
-- | rendered = layout myMachine
-- | -- rendered.states contains positioned states
-- | -- rendered.transitions contains arrow paths
-- | ```
module DataViz.Layout.StateMachine
  ( module DataViz.Layout.StateMachine.Types
  , module DataViz.Layout.StateMachine.Layout
  , module DataViz.Layout.StateMachine.Path
  ) where

import DataViz.Layout.StateMachine.Types (LayoutState, LayoutTransition, State, StateMachine, StateMachineLayout, StatePosition, Transition, TransitionPath)
import DataViz.Layout.StateMachine.Layout (LayoutConfig, circularLayout, defaultConfig, gridLayout, layout, layoutWithConfig)
import DataViz.Layout.StateMachine.Path (arrowheadPathD, initialArrowPathD, selfLoopPathD, stateEllipse, stateFinalRing, transitionPathD)
