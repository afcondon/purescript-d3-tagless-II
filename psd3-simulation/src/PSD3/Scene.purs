-- | PSD3 Scene System
-- |
-- | Declarative scene orchestration for complex interactive visualizations.
-- |
-- | The scene system provides:
-- | - **SceneConfig**: Define visualization states with rules and layouts
-- | - **SceneEngine**: Orchestrate transitions between scenes
-- | - **Rules**: CSS-like selector + transform pattern for node manipulation
-- | - **Transitions**: Smooth interpolation between positions with easing
-- |
-- | == Quick Start
-- |
-- | 1. Define scene configurations:
-- |
-- | ```purescript
-- | treeScene :: SceneConfig MyNode
-- | treeScene =
-- |   { name: "Tree"
-- |   , initRules: [ ruleAll "pinAll" pinAtCurrent ]
-- |   , layout: computeTreeLayout
-- |   , finalRules: \_ -> [ rule "unpinModules" isModule unpin ]
-- |   , stableMode: Static
-- |   , transition: defaultTransition
-- |   }
-- | ```
-- |
-- | 2. Create an engine with your simulation adapter:
-- |
-- | ```purescript
-- | engine <- createEngine
-- |   { getNodes: Sim.getNodes mySimulation
-- |   , capturePositions: Transition.capturePositions
-- |   , interpolatePositions: \s t p -> Sim.interpolatePositionsInPlace s t p mySimulation
-- |   , updatePositions: \p -> Sim.updatePositionsInPlace p mySimulation
-- |   , applyRulesInPlace: \r -> Sim.applyRulesInPlace r mySimulation
-- |   , reinitializeForces: myReinitForces
-- |   , reheat: Sim.reheat mySimulation
-- |   }
-- | ```
-- |
-- | 3. Transition to scenes and tick:
-- |
-- | ```purescript
-- | transitionTo treeScene engine
-- |
-- | Sim.onTick (\_ -> void $ tick engine) mySimulation
-- | ```
-- |
-- | == Architecture
-- |
-- | The scene system uses two "engines":
-- |
-- | - **Interpolation Engine**: During transitions, smoothly moves nodes from
-- |   start positions to target positions using configurable easing.
-- |
-- | - **Physics Engine**: After transitions complete (in Physics mode),
-- |   delegates to the D3 force simulation for emergent behavior.
-- |
-- | == Three-Phase Lifecycle
-- |
-- | Each scene transition follows three phases:
-- |
-- | 1. **Initialize** (`initRules`): Prepare starting state. Example: move
-- |    tree nodes to root position for "grow from root" animation.
-- |
-- | 2. **Transition** (`layout`): Interpolation engine smoothly moves nodes
-- |    to target positions computed by the layout function.
-- |
-- | 3. **Finalize** (`finalRules`): Set up stable state. Example: unpin
-- |    nodes so forces can take over, or set gridX/gridY for position forces.
module PSD3.Scene
  ( module Engine
  , module Rules
  , module Transition
  , module Types
  ) where

import PSD3.Scene.Engine
  ( SceneEngine
  , EngineAdapter
  , createEngine
  , transitionTo
  , tick
  , tickWithDelta
  , getCurrentScene
  , isTransitioning
  , getTransitionProgress
  ) as Engine

import PSD3.Scene.Rules
  ( rule
  , ruleAll
  , pinAtCurrent
  , pinAt
  , unpin
  , setPosition
  , applyRules
  , applyFirstMatch
  ) as Rules

import PSD3.Scene.Transition
  ( capturePositions
  , capturePositionsBy
  , calculateProgress
  , isComplete
  , lookupPosition
  ) as Transition

import PSD3.Scene.Types
  ( SceneConfig
  , TransitionConfig
  , TransitionState
  , EngineMode(..)
  , PositionMap
  , Position
  , NodeRule
  , defaultTransition
  ) as Types
