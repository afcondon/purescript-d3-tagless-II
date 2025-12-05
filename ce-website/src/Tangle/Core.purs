-- | Tangle - Reactive documents with embedded controls
-- |
-- | Inspired by Bret Victor's Tangle.js, this module provides a type-safe way
-- | to create "explorable explanations" - documents where the text itself
-- | contains interactive controls that update the underlying state.
-- |
-- | Key concepts:
-- | - **TangleDoc**: A structured document with embedded controls
-- | - **Control**: Different interaction types (toggle, cycle, adjust, etc.)
-- | - **Tangle typeclass**: State types that can describe themselves as TangleDocs
-- |
-- | The document is bidirectional: state generates the document, and user
-- | interactions on controls update the state.
module Tangle.Core
  ( -- * Document structure
    TangleDoc(..)
  , TangleSegment(..)
  , Control(..)
  , AdjustConfig
  , CycleConfig

  -- * Building documents
  , text
  , control
  , toggle
  , cycle
  , adjust
  , display
  , action
  , (<+>)
  , concat

  -- * Typeclass
  , class Tangle
  , describe
  , applyControl

  -- * Document operations
  , extractControls
  , toPlainText

  -- * Control helpers
  , controlDisplayText
  , controlId
  , isInteractive
  , cycleNext
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

-- =============================================================================
-- Document Structure
-- =============================================================================

-- | A Tangle document is a sequence of text and controls
-- |
-- | This is intentionally NOT a string with markup - it's structured data
-- | that can be rendered to HTML, plain text, or any other format.
newtype TangleDoc = TangleDoc (Array TangleSegment)

derive instance newtypeTangleDoc :: Newtype TangleDoc _
-- Note: No Eq instance because segments contain functions

-- | A segment is either plain text or a control
data TangleSegment
  = TextSegment String
  | ControlSegment Control

-- Note: TangleSegment doesn't have Eq because Control contains a function

-- | Controls are the interactive elements
-- |
-- | Each control has:
-- | - An ID for identifying which control was activated
-- | - The current display value
-- | - Interaction behavior
data Control
  = Toggle
      { id :: String
      , current :: Boolean
      , trueLabel :: String
      , falseLabel :: String
      }
  | Cycle
      { id :: String
      , current :: String
      , options :: Array String  -- Click cycles through these
      }
  | Adjust
      { id :: String
      , current :: Number
      , min :: Number
      , max :: Number
      , step :: Number
      , format :: Number -> String  -- How to display the number
      }
  | Display
      { id :: String
      , value :: String  -- Non-interactive, just shows a value
      }
  | Action
      { id :: String
      , label :: String       -- What to display
      , actionValue :: String -- Value sent when clicked (one-way, doesn't cycle)
      }

-- Note: Control doesn't have Eq because Adjust contains a function (format)

-- | Configuration for adjustable numbers
type AdjustConfig =
  { min :: Number
  , max :: Number
  , step :: Number
  , format :: Number -> String
  }

-- | Configuration for cycle controls
type CycleConfig =
  { options :: Array String
  }

-- =============================================================================
-- Smart Constructors
-- =============================================================================

-- | Plain text segment
text :: String -> TangleDoc
text s = TangleDoc [ TextSegment s ]

-- | Embed a control
control :: Control -> TangleDoc
control c = TangleDoc [ ControlSegment c ]

-- | Boolean toggle - click to flip between true/false
-- |
-- | Example: "Show [all] modules" where clicking "all" toggles to "project only"
toggle :: String -> Boolean -> String -> String -> TangleDoc
toggle id current trueLabel falseLabel =
  control $ Toggle { id, current, trueLabel, falseLabel }

-- | Cycle through options - click to advance to next
-- |
-- | Example: "Using [treemap] layout" - click cycles: treemap → tree → force → treemap
cycle :: String -> String -> Array String -> TangleDoc
cycle id current options =
  control $ Cycle { id, current, options }

-- | Adjustable number - drag left/right to change
-- |
-- | Example: "Showing [42] modules" - drag to adjust the count
adjust :: String -> Number -> AdjustConfig -> TangleDoc
adjust id current config =
  control $ Adjust
    { id
    , current
    , min: config.min
    , max: config.max
    , step: config.step
    , format: config.format
    }

-- | Display-only value (non-interactive)
-- |
-- | Example: "Found [123] results" where 123 is computed, not changeable
display :: String -> String -> TangleDoc
display id value =
  control $ Display { id, value }

-- | Action control - clickable one-way trigger
-- |
-- | Unlike cycle/toggle which update state bidirectionally, an action fires
-- | a fixed value when clicked. Useful for "escape hatch" navigation like
-- | clicking "Neighborhood" to go back to overview.
-- |
-- | Example: "[Neighborhood] of Data.Array" where clicking "Neighborhood" sends "back"
action :: String -> String -> String -> TangleDoc
action id label actionValue =
  control $ Action { id, label, actionValue }

-- | Concatenate two documents
infixr 5 append as <+>

-- | Concatenate multiple documents
concat :: forall f. Foldable f => f TangleDoc -> TangleDoc
concat = foldMap identity

-- =============================================================================
-- Instances
-- =============================================================================

instance semigroupTangleDoc :: Semigroup TangleDoc where
  append (TangleDoc a) (TangleDoc b) = TangleDoc (a <> b)

instance monoidTangleDoc :: Monoid TangleDoc where
  mempty = TangleDoc []

-- =============================================================================
-- Typeclass
-- =============================================================================

-- | Types that can be represented as interactive Tangle documents
-- |
-- | Laws:
-- | - `applyControl` should be a no-op for control IDs not in `describe`
-- | - After `applyControl id value state`, the control with that ID in
-- |   `describe` should reflect the new value
class Tangle state where
  -- | Generate a document describing this state
  describe :: state -> TangleDoc

  -- | Apply a control change. The ControlDelta varies by control type:
  -- | - Toggle: "true" or "false"
  -- | - Cycle: the selected option string
  -- | - Adjust: the new numeric value as string
  applyControl :: String -> String -> state -> state

-- =============================================================================
-- Document Operations
-- =============================================================================

-- | Extract all controls from a document
extractControls :: TangleDoc -> Array Control
extractControls (TangleDoc segments) = Array.mapMaybe getControl segments
  where
  getControl (ControlSegment c) = Just c
  getControl _ = Nothing

-- | Convert to plain text (for accessibility, debugging)
toPlainText :: TangleDoc -> String
toPlainText (TangleDoc segments) = Array.foldMap segmentText segments
  where
  segmentText (TextSegment s) = s
  segmentText (ControlSegment c) = controlText c

  controlText (Toggle { current, trueLabel, falseLabel }) =
    if current then trueLabel else falseLabel
  controlText (Cycle { current }) = current
  controlText (Adjust { current, format }) = format current
  controlText (Display { value }) = value
  controlText (Action { label }) = label

-- =============================================================================
-- Control Helpers
-- =============================================================================

-- | Get the display text for a control
controlDisplayText :: Control -> String
controlDisplayText (Toggle { current, trueLabel, falseLabel }) =
  if current then trueLabel else falseLabel
controlDisplayText (Cycle { current }) = current
controlDisplayText (Adjust { current, format }) = format current
controlDisplayText (Display { value }) = value
controlDisplayText (Action { label }) = label

-- | Get the ID of a control
controlId :: Control -> String
controlId (Toggle { id }) = id
controlId (Cycle { id }) = id
controlId (Adjust { id }) = id
controlId (Display { id }) = id
controlId (Action { id }) = id

-- | Check if a control is interactive
isInteractive :: Control -> Boolean
isInteractive (Display _) = false
isInteractive _ = true

-- | Compute next value for a cycle control
cycleNext :: String -> Array String -> String
cycleNext current options =
  case Array.findIndex (_ == current) options of
    Nothing -> current
    Just idx ->
      let nextIdx = (idx + 1) `mod` Array.length options
      in Array.index options nextIdx # fromMaybe current
  where
  fromMaybe def = case _ of
    Nothing -> def
    Just v -> v
