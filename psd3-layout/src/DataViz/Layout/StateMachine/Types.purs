-- | DataViz.Layout.StateMachine.Types
-- |
-- | Types for state machine visualization.
-- | A state machine diagram shows states as circles/ovals
-- | and transitions as labeled arrows between them.
module DataViz.Layout.StateMachine.Types
  ( State
  , Transition
  , StateMachine
  , StatePosition
  , TransitionPath
  , LayoutState
  , LayoutTransition
  , StateMachineLayout
  ) where

-- | A state in the state machine
-- | The `extra` field allows attaching arbitrary data (e.g., phantom type info)
type State extra =
  { id :: String           -- Unique identifier
  , label :: String        -- Display label
  , isInitial :: Boolean   -- Has incoming arrow from nowhere
  , isFinal :: Boolean     -- Double circle
  , extra :: extra         -- User-defined extra data
  }

-- | A transition between states
type Transition =
  { from :: String         -- Source state id
  , to :: String           -- Target state id
  , label :: String        -- Transition label (e.g., operation name)
  }

-- | Complete state machine definition
type StateMachine extra =
  { states :: Array (State extra)
  , transitions :: Array Transition
  }

-- | Computed position for a state
type StatePosition =
  { cx :: Number           -- Center x
  , cy :: Number           -- Center y
  , rx :: Number           -- Horizontal radius (for ellipse)
  , ry :: Number           -- Vertical radius (for ellipse)
  }

-- | Computed path for a transition arrow
-- | Uses quadratic bezier for curved arrows
type TransitionPath =
  { startX :: Number       -- Arrow start point
  , startY :: Number
  , controlX :: Number     -- Bezier control point
  , controlY :: Number
  , endX :: Number         -- Arrow end point (at state edge)
  , endY :: Number
  , labelX :: Number       -- Position for the label
  , labelY :: Number
  , angle :: Number        -- Angle at endpoint for arrowhead
  , isSelfLoop :: Boolean  -- Special rendering for self-transitions
  }

-- | A state with computed layout
type LayoutState extra =
  { state :: State extra
  , position :: StatePosition
  }

-- | A transition with computed layout
type LayoutTransition =
  { transition :: Transition
  , path :: TransitionPath
  }

-- | Complete layout output ready for rendering
type StateMachineLayout extra =
  { states :: Array (LayoutState extra)
  , transitions :: Array (LayoutTransition)
  , width :: Number
  , height :: Number
  , initialArrow :: { x :: Number, y :: Number, angle :: Number }  -- Arrow pointing to initial state
  }
