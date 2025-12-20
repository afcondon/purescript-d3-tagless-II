module PSD3.Music.Interpreter.WebAudio
  ( MusicSelection_
  , MusicM
  , runMusicM
  , initMusicContext
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldl, traverse_)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..))
import PSD3.Internal.Behavior.Types (Behavior)
import PSD3.Internal.Capabilities.Selection (class SelectionM)
import PSD3.Internal.Selection.Types (ElementType, JoinResult(..), SBoundOwns, SBoundInherits, SEmpty, SExiting, SPending)
import PSD3.AST (Tree)
import PSD3.Music.Internal.FFI (AudioContext, createAudioContext, scheduleNote, NoteParams)
import Web.DOM.Element (Element)

-- =============================================================================
-- Selection Types
-- =============================================================================

-- | Music selection type for audio sonification
-- |
-- | Instead of DOM elements, this represents scheduled audio events.
-- | The phantom types work the same way as in D3 for type safety.
newtype MusicSelection_ (state :: Type) (parent :: Type) (datum :: Type)
  = MusicSelection_ (MusicSelectionImpl datum)

-- | Internal representation of a music selection
data MusicSelectionImpl datum
  = AudioContextSel
      { contextId :: String
      , data :: Array datum
      }
  | AudioEventsSel
      { events :: Array (AudioEventWithData datum)
      , data :: Array datum
      }

-- | Audio event paired with its source datum
type AudioEventWithData datum =
  { event :: NoteParams
  , datum :: datum
  , index :: Int
  }

-- =============================================================================
-- Monad
-- =============================================================================

-- | The Music interpreter monad
-- |
-- | Carries an AudioContext reference for scheduling notes.
-- | Uses ReaderT pattern to thread the context through computations.
newtype MusicM a = MusicM (Ref AudioContext -> Effect a)

instance Functor MusicM where
  map f (MusicM ma) = MusicM \ctx -> map f (ma ctx)

instance Apply MusicM where
  apply (MusicM ff) (MusicM fa) = MusicM \ctx -> do
    f <- ff ctx
    a <- fa ctx
    pure (f a)

instance Applicative MusicM where
  pure a = MusicM \_ -> pure a

instance Bind MusicM where
  bind (MusicM ma) f = MusicM \ctx -> do
    a <- ma ctx
    case f a of
      MusicM mb -> mb ctx

instance Monad MusicM

instance MonadEffect MusicM where
  liftEffect eff = MusicM \_ -> eff

-- | Initialize audio context and run a music program
-- |
-- | Creates the Web Audio context and executes the computation.
-- | Must be called in response to user interaction (browser requirement).
initMusicContext :: MusicM Unit -> Effect Unit
initMusicContext (MusicM program) = do
  ctx <- createAudioContext
  ctxRef <- Ref.new ctx
  program ctxRef

-- | Run a music program with an existing context ref
runMusicM :: Ref AudioContext -> MusicM ~> Effect
runMusicM ctxRef (MusicM program) = program ctxRef

-- =============================================================================
-- Attribute Extraction
-- =============================================================================

-- | Extract audio parameters from attributes
-- |
-- | Attributes come from the PSD3 attribute system. We interpret
-- | specific attribute names as audio parameters.
extractNoteParams :: forall datum. Int -> datum -> Array (Attribute datum) -> NoteParams
extractNoteParams index datum attrs =
  let defaults =
        { time: toNumber index * 0.5  -- Default: 500ms apart
        , frequency: 440.0             -- Default: A4
        , duration: 0.3                -- Default: 300ms
        , volume: 0.5                  -- Default: moderate volume
        , waveform: "sine"             -- Default: pure tone
        }
  in foldl (applyAttribute index datum) defaults attrs

-- | Apply a single attribute to note parameters
applyAttribute :: forall datum. Int -> datum -> NoteParams -> Attribute datum -> NoteParams
applyAttribute index datum params attr = case attr of
  IndexedAttr (AttributeName "time") _ f ->
    case f datum index of
      NumberValue n -> params { time = n }
      _ -> params

  IndexedAttr (AttributeName "pitch") _ f ->
    case f datum index of
      NumberValue n -> params { frequency = n }
      _ -> params

  IndexedAttr (AttributeName "duration") _ f ->
    case f datum index of
      NumberValue n -> params { duration = n }
      _ -> params

  IndexedAttr (AttributeName "volume") _ f ->
    case f datum index of
      NumberValue n -> params { volume = n }
      _ -> params

  IndexedAttr (AttributeName "timbre") _ f ->
    case f datum index of
      StringValue s -> params { waveform = s }
      _ -> params

  _ -> params  -- Ignore unknown or non-indexed attributes

-- =============================================================================
-- SelectionM Instance
-- =============================================================================

instance SelectionM MusicSelection_ MusicM where

  select selector = MusicM \_ -> do
    pure $ MusicSelection_ $ AudioContextSel
      { contextId: selector
      , data: []
      }

  selectAll selector (MusicSelection_ parent) = MusicM \_ -> do
    pure $ MusicSelection_ $ AudioContextSel
      { contextId: selector
      , data: []
      }

  openSelection (MusicSelection_ parent) selector = MusicM \_ -> do
    pure $ MusicSelection_ $ AudioContextSel
      { contextId: selector
      , data: []
      }

  selectAllWithData selector (MusicSelection_ parent) = MusicM \_ -> do
    -- For music, we don't really have child elements with data
    -- Just return an empty selection (stub for now)
    pure $ MusicSelection_ $ AudioEventsSel
      { events: []
      , data: []
      }

  -- | renderData: The high-level API for enter-update-exit
  -- |
  -- | For audio, we just schedule all the notes. In a more sophisticated
  -- | implementation, update would modify playing notes and exit would stop them.
  renderData elemType foldableData selector (MusicSelection_ emptySelection) enterAttrs updateAttrs exitAttrs = MusicM \ctxRef -> do
    let dataArray = Array.fromFoldable foldableData

    case enterAttrs of
      Just mkAttrs -> do
        -- Schedule notes for each datum
        ctx <- Ref.read ctxRef
        traverse_ (\(Tuple idx datum) -> do
          let attrs = mkAttrs datum
          let noteParams = extractNoteParams idx datum attrs
          scheduleNote ctx noteParams
        ) (Array.mapWithIndex Tuple dataArray)

      Nothing -> pure unit

    pure $ MusicSelection_ $ AudioEventsSel
      { events: []  -- TODO: track scheduled events
      , data: dataArray
      }

  -- | appendData: Simple data append (just enter, no update/exit)
  -- |
  -- | This is the key method for the Parabola demo.
  -- | Takes data and attributes, schedules notes accordingly.
  appendData elemType foldableData attrs (MusicSelection_ emptySelection) = MusicM \ctxRef -> do
    let dataArray = Array.fromFoldable foldableData

    -- Schedule a note for each datum
    ctx <- Ref.read ctxRef
    traverse_ (\(Tuple idx datum) -> do
      let noteParams = extractNoteParams idx datum attrs
      scheduleNote ctx noteParams
    ) (Array.mapWithIndex Tuple dataArray)

    pure $ MusicSelection_ $ AudioEventsSel
      { events: []  -- TODO: track events if needed
      , data: dataArray
      }

  -- Remaining methods: minimal stubs for now

  joinData foldableData selector (MusicSelection_ emptySelection) = MusicM \_ -> do
    pure $ JoinResult
      { enter: MusicSelection_ $ AudioEventsSel { events: [], data: [] }
      , update: MusicSelection_ $ AudioEventsSel { events: [], data: [] }
      , exit: MusicSelection_ $ AudioEventsSel { events: [], data: [] }
      }

  joinDataWithKey foldableData keyFn selector (MusicSelection_ emptySelection) = MusicM \_ -> do
    pure $ JoinResult
      { enter: MusicSelection_ $ AudioEventsSel { events: [], data: [] }
      , update: MusicSelection_ $ AudioEventsSel { events: [], data: [] }
      , exit: MusicSelection_ $ AudioEventsSel { events: [], data: [] }
      }

  updateJoin (MusicSelection_ emptySelection) _elemType foldableData keyFn selector = MusicM \_ -> do
    pure $ JoinResult
      { enter: MusicSelection_ $ AudioEventsSel { events: [], data: [] }
      , update: MusicSelection_ $ AudioEventsSel { events: [], data: [] }
      , exit: MusicSelection_ $ AudioEventsSel { events: [], data: [] }
      }

  append elemType attrs (MusicSelection_ pendingSelection) = MusicM \_ -> do
    pure $ MusicSelection_ $ AudioEventsSel { events: [], data: [] }

  setAttrs attrs (MusicSelection_ boundSelection) = MusicM \_ -> do
    pure $ MusicSelection_ $ AudioEventsSel { events: [], data: [] }

  setAttrsExit attrs (MusicSelection_ exitingSelection) = MusicM \_ -> do
    pure $ MusicSelection_ $ AudioEventsSel { events: [], data: [] }

  remove (MusicSelection_ exitingSelection) = MusicM \_ -> do
    pure unit

  clear selector = MusicM \_ -> do
    pure unit

  merge (MusicSelection_ sel1) (MusicSelection_ sel2) = MusicM \_ -> do
    pure $ MusicSelection_ $ AudioEventsSel { events: [], data: [] }

  appendChild elemType attrs (MusicSelection_ emptySelection) = MusicM \_ -> do
    pure $ MusicSelection_ $ AudioContextSel { contextId: "child", data: [] }

  appendChildInheriting elemType attrs (MusicSelection_ boundSelection) = MusicM \_ -> do
    pure $ MusicSelection_ $ AudioEventsSel { events: [], data: [] }

  on behavior (MusicSelection_ selection) = MusicM \_ -> do
    pure $ MusicSelection_ selection

  renderTree (MusicSelection_ parent) tree = MusicM \_ -> do
    pure Map.empty
