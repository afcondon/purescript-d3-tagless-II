-- | Scene Rules
-- |
-- | Common rule builders and transforms for scene orchestration.
-- |
-- | Rules are applied with first-match-wins semantics (CSS-like cascade).
-- | Each rule has a selector (predicate) and a transform function.
-- |
-- | This module provides:
-- | - Rule builders for creating rules
-- | - Common transforms (pin, unpin, setGridXY)
-- | - Rule application functions
module PSD3.Scene.Rules
  ( -- * Rule Builders
    rule
  , ruleAll
    -- * Common Transforms
  , pinAtCurrent
  , pinAt
  , unpin
  , setPosition
    -- * Rule Application
  , applyRules
  , applyFirstMatch
    -- * Re-exports
  , module Types
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import PSD3.Scene.Types (NodeRule) as Types

-- =============================================================================
-- Rule Builders
-- =============================================================================

-- | Build a rule with name, selector, and transform.
-- |
-- | Example:
-- | ```purescript
-- | pinPackages :: NodeRule MyNode
-- | pinPackages = rule "pinPackages" isPackage pinAtCurrent
-- | ```
rule
  :: forall node
   . String                  -- ^ Rule name (for debugging)
  -> (node -> Boolean)       -- ^ Selector predicate
  -> (node -> node)          -- ^ Transform function
  -> Types.NodeRule node
rule name select apply =
  { name, select, apply }

-- | Build a rule that applies to all nodes.
-- |
-- | Example:
-- | ```purescript
-- | pinAll :: NodeRule MyNode
-- | pinAll = ruleAll "pinAll" pinAtCurrent
-- | ```
ruleAll
  :: forall node
   . String                  -- ^ Rule name
  -> (node -> node)          -- ^ Transform function
  -> Types.NodeRule node
ruleAll name apply =
  { name, select: const true, apply }

-- =============================================================================
-- Common Transforms
-- =============================================================================

-- | Pin a node at its current position.
-- |
-- | Sets fx = x, fy = y so the node won't move during simulation.
-- |
-- | Type signature uses row polymorphism - works with any node type
-- | that has the required fields.
pinAtCurrent
  :: forall r
   . { x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r }
  -> { x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r }
pinAtCurrent n = n
  { fx = Nullable.notNull n.x
  , fy = Nullable.notNull n.y
  }

-- | Pin a node at a specific position.
-- |
-- | Sets x, y, fx, fy all to the given coordinates.
pinAt
  :: forall r
   . Number                  -- ^ Target X
  -> Number                  -- ^ Target Y
  -> { x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r }
  -> { x :: Number, y :: Number, fx :: Nullable Number, fy :: Nullable Number | r }
pinAt targetX targetY n = n
  { x = targetX
  , y = targetY
  , fx = Nullable.notNull targetX
  , fy = Nullable.notNull targetY
  }

-- | Unpin a node (clear fx/fy).
-- |
-- | Allows the node to move freely under force simulation.
unpin
  :: forall r
   . { fx :: Nullable Number, fy :: Nullable Number | r }
  -> { fx :: Nullable Number, fy :: Nullable Number | r }
unpin n = n
  { fx = Nullable.null
  , fy = Nullable.null
  }

-- | Set a node's position without pinning.
-- |
-- | Updates x, y but leaves fx, fy unchanged.
setPosition
  :: forall r
   . Number                  -- ^ Target X
  -> Number                  -- ^ Target Y
  -> { x :: Number, y :: Number | r }
  -> { x :: Number, y :: Number | r }
setPosition targetX targetY n = n
  { x = targetX
  , y = targetY
  }

-- =============================================================================
-- Rule Application
-- =============================================================================

-- | Apply rules to an array of nodes (first matching rule wins).
-- |
-- | For each node, finds the first rule whose selector matches,
-- | then applies that rule's transform. If no rule matches, the
-- | node is returned unchanged.
-- |
-- | This is CSS-like cascade semantics - rule order matters.
applyRules
  :: forall node
   . Array (Types.NodeRule node)
  -> Array node
  -> Array node
applyRules rules nodes = map (applyFirstMatch rules) nodes

-- | Apply the first matching rule to a single node.
-- |
-- | Returns the node unchanged if no rule matches.
applyFirstMatch
  :: forall node
   . Array (Types.NodeRule node)
  -> node
  -> node
applyFirstMatch rules node =
  case Array.find (\r -> r.select node) rules of
    Just r -> r.apply node
    Nothing -> node
