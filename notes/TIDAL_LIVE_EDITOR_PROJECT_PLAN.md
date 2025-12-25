# Tidal Live Visual Editor - Complete Project Plan

## Project Overview

Build a standalone web application that connects to a live TidalCycles process, providing:
1. **Real-time visualization** of active patterns (trees, sunbursts, metrics)
2. **Visual pattern editing** through direct manipulation of the tree structure
3. **Bidirectional sync** between visual editor and Tidal REPL
4. **Collaborative potential** (multiple clients seeing/editing same patterns)

This is a **separate repository** that will:
- Import PSD3 as a published library (once available)
- Reference but not modify the TidalCycles source
- Be deployable as a standalone application

## Repository Structure

```
tidal-live/
├── README.md
├── LICENSE
├── .gitignore
│
├── docs/
│   ├── ARCHITECTURE.md          -- System design
│   ├── PROTOCOL.md              -- WebSocket message spec
│   ├── TIDAL_INTEGRATION.md     -- How to hook into Tidal
│   └── DEPLOYMENT.md            -- Running the full stack
│
├── backend/                     -- Haskell WebSocket server
│   ├── tidal-bridge.cabal
│   ├── stack.yaml
│   ├── package.yaml
│   ├── Setup.hs
│   ├── src/
│   │   ├── Main.hs
│   │   ├── TidalBridge/
│   │   │   ├── Server.hs        -- WebSocket server
│   │   │   ├── TidalHooks.hs    -- Intercept Tidal patterns
│   │   │   ├── PatternCodec.hs  -- Tidal ↔ JSON serialization
│   │   │   ├── Protocol.hs      -- Message types
│   │   │   └── Session.hs       -- Client session management
│   │   └── BootTidal.hs         -- Custom BootTidal with hooks
│   ├── test/
│   │   └── Spec.hs
│   └── README.md
│
├── frontend/                    -- PureScript web UI
│   ├── spago.yaml
│   ├── packages.dhall
│   ├── package.json             -- npm deps for bundling
│   ├── src/
│   │   ├── Main.purs
│   │   ├── TidalLive/
│   │   │   ├── App.purs         -- Main Halogen app
│   │   │   ├── WebSocket.purs   -- WS client wrapper
│   │   │   ├── State.purs       -- App state management
│   │   │   └── Protocol.purs    -- Message codecs (matches backend)
│   │   ├── Component/
│   │   │   ├── LiveViz.purs     -- Real-time pattern visualizer
│   │   │   ├── Editor.purs      -- Visual tree editor
│   │   │   ├── TextEditor.purs  -- Mini-notation text editor
│   │   │   ├── TrackPanel.purs  -- Track list & controls
│   │   │   └── Toolbar.purs     -- Top toolbar (connect, BPM, etc)
│   │   ├── Viz/                 -- Visualization components
│   │   │   ├── CombinatorForest.purs  -- Adapted from PSD3 demo
│   │   │   ├── SunburstGrid.purs
│   │   │   └── PatternTree.purs
│   │   └── Data/
│   │       ├── PatternTree.purs -- Core pattern AST (copy from PSD3 demo)
│   │       ├── MiniNotation.purs -- Parser (copy from PSD3 demo)
│   │       └── Track.purs       -- Track state model
│   ├── public/
│   │   ├── index.html
│   │   ├── styles.css
│   │   └── assets/
│   ├── test/
│   │   └── Main.purs
│   └── README.md
│
├── protocol/                    -- Shared protocol spec
│   └── schema.json              -- JSON schema for messages
│
├── scripts/
│   ├── dev.sh                   -- Start full dev stack
│   ├── build.sh                 -- Build both backend & frontend
│   └── deploy.sh                -- Deploy to server
│
└── docker/
    ├── Dockerfile.backend
    ├── Dockerfile.frontend
    └── docker-compose.yml       -- Full stack for deployment
```

## Dependencies & Prerequisites

### Backend (Haskell)

```yaml
# stack.yaml
resolver: lts-21.25  # Recent GHC 9.4.x

packages:
  - .

extra-deps:
  - tidal-1.9.5
  # Or use local path if developing against Tidal HEAD:
  # - ../Tidal

dependencies:
  # Core
  - base >= 4.7 && < 5
  - tidal >= 1.9.5

  # WebSocket server
  - websockets >= 0.12
  - warp >= 3.3
  - wai >= 3.2
  - wai-websockets >= 3.0

  # JSON
  - aeson >= 2.0
  - aeson-pretty

  # Concurrency
  - async
  - stm

  # Utilities
  - text
  - bytestring
  - containers
  - unordered-containers
  - vector
  - uuid
  - time
```

### Frontend (PureScript)

```yaml
# spago.yaml
package:
  name: tidal-live-frontend
  dependencies:
    # Core
    - prelude
    - effect
    - console
    - aff

    # Halogen UI
    - halogen
    - halogen-hooks
    - halogen-subscriptions

    # WebSocket
    - web-socket  # purescript-web-socket
    - web-storage # For persisting state

    # JSON
    - argonaut-core
    - argonaut-codecs
    - argonaut-generic

    # Data structures
    - arrays
    - maybe
    - either
    - tuples
    - ordered-collections
    - lists

    # PSD3 (once published to registry)
    # For now, copy needed modules:
    # - PatternTree type
    # - MiniNotation parser
    # - Visualization components

    # Later: use published packages
    # - psd3-selection
    # - psd3-layout
```

```json
// package.json
{
  "name": "tidal-live-frontend",
  "scripts": {
    "build": "spago bundle-app --to public/bundle.js",
    "dev": "spago bundle-app --watch --to public/bundle.js",
    "serve": "python3 -m http.server 8080 --directory public"
  },
  "devDependencies": {
    "purescript": "^0.15.15",
    "spago": "^0.21.0"
  }
}
```

## Phase-by-Phase Implementation

### Phase 0: Setup & Scaffolding (Week 1)

**Goal:** Get basic project structure working with hello-world on both sides.

**Tasks:**
1. Create repo, initialize git
2. Set up Haskell backend:
   - `stack init`
   - Basic `Main.hs` that prints "Hello from Tidal Bridge"
   - Verify Tidal dependency resolves
3. Set up PureScript frontend:
   - `spago init`
   - Basic Halogen app rendering "Hello Tidal Live"
   - Serve on localhost:8080
4. Write `scripts/dev.sh`:
   ```bash
   #!/bin/bash
   # Terminal 1: Backend
   stack run &
   # Terminal 2: Frontend
   cd frontend && npm run dev
   ```

**Deliverable:** Both backend and frontend run independently, no connection yet.

---

### Phase 1: WebSocket Connection (Week 2)

**Goal:** Establish bidirectional WebSocket connection, send "ping/pong" messages.

#### Backend Tasks:

**1. Create WebSocket server** (`src/TidalBridge/Server.hs`):

```haskell
module TidalBridge.Server (startServer) where

import qualified Network.WebSockets as WS
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Handler.Warp (run)
import Data.Aeson (encode, decode)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map

data ServerState = ServerState
  { clients :: TVar (Map.Map ClientId WS.Connection)
  , nextClientId :: TVar Int
  }

type ClientId = Int

newServerState :: IO ServerState
newServerState = ServerState
  <$> newTVarIO Map.empty
  <*> newTVarIO 0

application :: ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  clientId <- atomically $ do
    cid <- readTVar (nextClientId state)
    modifyTVar' (nextClientId state) (+1)
    modifyTVar' (clients state) (Map.insert cid conn)
    pure cid

  putStrLn $ "Client " <> show clientId <> " connected"

  WS.withPingThread conn 30 (pure ()) $
    handleMessages state clientId conn

handleMessages :: ServerState -> ClientId -> WS.Connection -> IO ()
handleMessages state clientId conn =
  -- For now, just echo
  forever $ do
    msg <- WS.receiveData conn
    putStrLn $ "Received from " <> show clientId <> ": " <> show msg
    WS.sendTextData conn msg

startServer :: IO ()
startServer = do
  state <- newServerState
  putStrLn "Tidal Bridge Server starting on ws://localhost:9160"
  run 9160 $ websocketsOr WS.defaultConnectionOptions (application state) backupApp
  where
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
```

**2. Update Main.hs:**

```haskell
module Main where

import TidalBridge.Server (startServer)

main :: IO ()
main = startServer
```

**3. Test with `wscat`:**

```bash
stack run &
npm install -g wscat
wscat -c ws://localhost:9160
> {"type": "ping"}
< {"type": "ping"}
```

#### Frontend Tasks:

**1. Create WebSocket client** (`src/TidalLive/WebSocket.purs`):

```purescript
module TidalLive.WebSocket where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Web.Socket.WebSocket as WS
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.Event.EventTypes as WT

type WSConfig =
  { url :: String
  , onMessage :: String -> Effect Unit
  , onOpen :: Effect Unit
  , onClose :: Effect Unit
  }

connect :: WSConfig -> Aff WS.WebSocket
connect config = makeAff \cb -> do
  ws <- WS.create config.url []

  WS.addEventListener WT.onOpen (\_ -> config.onOpen) false (WS.toEventTarget ws)
  WS.addEventListener WT.onClose (\_ -> config.onClose) false (WS.toEventTarget ws)
  WS.addEventListener WT.onMessage handleMessage false (WS.toEventTarget ws)

  cb (Right ws)
  pure nonCanceler
  where
    handleMessage event =
      case ME.data_ event of
        Just msg -> config.onMessage msg
        Nothing -> pure unit

send :: WS.WebSocket -> String -> Effect Unit
send ws msg = WS.sendString ws msg
```

**2. Integrate into main app** (`src/Main.purs`):

```purescript
module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import TidalLive.WebSocket as WS
import Web.Socket.WebSocket (WebSocket)

type State =
  { connected :: Boolean
  , ws :: Maybe WebSocket
  , messages :: Array String
  }

data Action
  = Initialize
  | HandleMessage String
  | SendPing

component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { connected: false, ws: Nothing, messages: [] }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render state =
  HH.div_
    [ HH.h1_ [ HH.text "Tidal Live Editor" ]
    , HH.p_ [ HH.text if state.connected then "✓ Connected" else "⨯ Disconnected" ]
    , HH.button [ HE.onClick \_ -> SendPing ] [ HH.text "Send Ping" ]
    , HH.ul_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.messages
    ]

handleAction = case _ of
  Initialize -> do
    H.liftEffect $ launchAff_ do
      ws <- WS.connect
        { url: "ws://localhost:9160"
        , onMessage: \msg -> handleAction (HandleMessage msg)
        , onOpen: H.modify_ _ { connected = true }
        , onClose: H.modify_ _ { connected = false }
        }
      H.modify_ _ { ws = Just ws }

  HandleMessage msg ->
    H.modify_ \s -> s { messages = s.messages <> [msg] }

  SendPing -> do
    state <- H.get
    case state.ws of
      Just ws -> H.liftEffect $ WS.send ws "{\"type\":\"ping\"}"
      Nothing -> pure unit
```

**Deliverable:** Frontend connects to backend via WebSocket, can send/receive messages.

---

### Phase 2: Pattern Serialization (Week 3-4)

**Goal:** Define protocol, serialize Tidal patterns to JSON, parse in frontend.

#### Protocol Definition (`protocol/schema.json`):

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "definitions": {
    "PatternTree": {
      "oneOf": [
        {
          "type": "object",
          "properties": {
            "tag": { "const": "Sound" },
            "contents": { "type": "string" }
          },
          "required": ["tag", "contents"]
        },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "Rest" }
          },
          "required": ["tag"]
        },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "Sequence" },
            "contents": {
              "type": "array",
              "items": { "$ref": "#/definitions/PatternTree" }
            }
          },
          "required": ["tag", "contents"]
        },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "Parallel" },
            "contents": {
              "type": "array",
              "items": { "$ref": "#/definitions/PatternTree" }
            }
          },
          "required": ["tag", "contents"]
        },
        {
          "type": "object",
          "properties": {
            "tag": { "const": "Fast" },
            "contents": {
              "type": "array",
              "items": [
                { "type": "number" },
                { "$ref": "#/definitions/PatternTree" }
              ],
              "minItems": 2,
              "maxItems": 2
            }
          },
          "required": ["tag", "contents"]
        }
      ]
    },
    "Combinator": {
      "type": "object",
      "properties": {
        "label": { "type": "string" },
        "enabled": { "type": "boolean" }
      },
      "required": ["label", "enabled"]
    },
    "PatternUpdate": {
      "type": "object",
      "properties": {
        "type": { "const": "patternUpdate" },
        "track": { "type": "string" },
        "miniNotation": { "type": "string" },
        "patternTree": { "$ref": "#/definitions/PatternTree" },
        "combinators": {
          "type": "array",
          "items": { "$ref": "#/definitions/Combinator" }
        },
        "timestamp": { "type": "number" }
      },
      "required": ["type", "track", "patternTree"]
    }
  }
}
```

#### Backend: Pattern Codec (`src/TidalBridge/PatternCodec.hs`):

**Challenge:** Tidal's `Pattern a` is infinite and lazy. We need AST extraction.

**Approach:** Hook into mini-notation parser BEFORE evaluation.

```haskell
module TidalBridge.PatternCodec where

import Data.Aeson
import Sound.Tidal.Context
import qualified Sound.Tidal.Parse as Parse

-- Our intermediate representation (matches PureScript PatternTree)
data PatternTree
  = Sound String
  | Rest
  | Sequence [PatternTree]
  | Parallel [PatternTree]
  | Fast Double PatternTree
  | Slow Double PatternTree
  | Choice [PatternTree]
  deriving (Show, Eq, Generic)

instance ToJSON PatternTree
instance FromJSON PatternTree

-- Parse mini-notation to AST (without evaluating to Pattern)
-- This is the KEY function - may need to modify Tidal or use internal parser
parseMiniNotationToAST :: String -> Either String PatternTree
parseMiniNotationToAST code =
  -- Option 1: If Tidal exposes AST before Pattern
  case Parse.parseBP code of
    Left err -> Left (show err)
    Right ast -> Right (convertTidalAST ast)  -- Need to define conversion

  -- Option 2: Parse ourselves (copy Tidal's parser)
  -- This is more work but gives full control

-- Convert Tidal's internal AST to our PatternTree
-- (Implementation depends on Tidal's internals)
convertTidalAST :: Parse.TPat a -> PatternTree
convertTidalAST = ...

-- Generate mini-notation code from AST
astToMiniNotation :: PatternTree -> String
astToMiniNotation = \case
  Sound s -> s
  Rest -> "~"
  Sequence children ->
    "[" <> intercalate " " (map astToMiniNotation children) <> "]"
  Parallel children ->
    "[" <> intercalate "," (map astToMiniNotation children) <> "]"
  Fast n child ->
    astToMiniNotation child <> "*" <> show (round n)
  Slow n child ->
    astToMiniNotation child <> "/" <> show (round n)
  Choice children ->
    "<" <> intercalate " " (map astToMiniNotation children) <> ">"
```

**Note:** The Tidal parser integration is the trickiest part. Study:
- `Sound.Tidal.Parse` in Tidal source
- Estuary's approach to this problem
- May need to maintain parallel parser or contribute AST extraction to Tidal

#### Frontend: Pattern Codec (`src/Data/PatternTree.purs`):

**Good news:** You already have this! Copy from PSD3 demo:

```purescript
module Data.PatternTree where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, (.:), (:=), (~>), jsonEmptyObject)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)

data PatternTree
  = Sequence (Array PatternTree)
  | Parallel (Array PatternTree)
  | Choice (Array PatternTree)
  | Sound String
  | Rest
  | Fast Number PatternTree
  | Slow Number PatternTree
  | Euclidean Int Int PatternTree
  | Degrade Number PatternTree
  | Repeat Int PatternTree
  | Elongate Number PatternTree

derive instance Generic PatternTree _
derive instance Eq PatternTree
derive instance Ord PatternTree

instance DecodeJson PatternTree where
  decodeJson = genericDecodeJson

instance EncodeJson PatternTree where
  encodeJson = genericEncodeJson
```

**Copy from PSD3 demo:**
- `Component/PatternTree.purs` - PatternTree type
- `Component/MiniNotation.purs` - Mini-notation parser (if needed)
- Keep these as **vendored copies** for now
- Once PSD3 is published, use as dependency

**Deliverable:** Can parse "bd sd hh*2" → JSON → PatternTree in PureScript

---

### Phase 3: Tidal Integration (Week 5-6)

**Goal:** Hook into live Tidal session, broadcast pattern updates to frontend.

#### Backend: Tidal Hooks (`src/TidalBridge/TidalHooks.hs`):

```haskell
module TidalBridge.TidalHooks where

import Sound.Tidal.Context
import TidalBridge.Protocol
import TidalBridge.PatternCodec
import Control.Concurrent.STM
import qualified Data.Map as Map

-- State to track active patterns
data TidalState = TidalState
  { activePatterns :: TVar (Map String PatternTree)
  , stream :: Stream
  }

-- Create instrumented stream that captures patterns
makeHookedStream :: (String -> PatternTree -> IO ()) -> IO Stream
makeHookedStream onPatternUpdate = do
  stream <- startStream defaultConfig
  -- Wrap streamReplace to intercept
  let hookedReplace = \k v -> do
        -- Try to extract AST from mini-notation
        case parseMiniNotationToAST (show v) of
          Right ast -> onPatternUpdate k ast
          Left err -> putStrLn $ "Failed to parse: " <> err
        -- Call original streamReplace
        streamReplace stream k v
  pure $ stream { streamReplace = hookedReplace }

-- Modified BootTidal.hs with hooks
makeHookedTidal :: (String -> PatternTree -> IO ()) -> IO (Stream, ParamPattern -> IO ())
makeHookedTidal onUpdate = do
  stream <- makeHookedStream onUpdate

  let d1 pat = streamReplace stream "d1" pat
      d2 pat = streamReplace stream "d2" pat
      -- ... d3-d16

      hush = streamHush stream
      solo = streamSolo stream

  pure (stream, d1)  -- Return what's needed for REPL
```

#### Custom REPL (`src/Main.hs`):

Option 1: **Embedded REPL** (simpler, self-contained)

```haskell
module Main where

import TidalBridge.Server
import TidalBridge.TidalHooks
import TidalBridge.Protocol
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Aeson (encode)

main :: IO ()
main = do
  -- State shared between Tidal and WebSocket server
  patternsVar <- newTVarIO Map.empty

  -- Callback when pattern changes
  let onPatternUpdate track ast = do
        atomically $ modifyTVar patternsVar (Map.insert track ast)
        -- Broadcast to all WebSocket clients
        broadcastPatternUpdate track ast

  -- Start Tidal with hooks
  (stream, _) <- makeHookedTidal onPatternUpdate

  -- Start WebSocket server
  forkIO $ startServer patternsVar

  -- Start embedded REPL
  putStrLn "Tidal Live Bridge - Ready!"
  putStrLn "Type Tidal code or :quit to exit"
  repl stream

  where
    repl stream = do
      putStr "tidal> "
      hFlush stdout
      line <- getLine
      case line of
        ":quit" -> pure ()
        code -> do
          -- Eval Tidal code
          -- This requires running GHCi programmatically or hint library
          evalTidalCode stream code
          repl stream
```

Option 2: **External GHCi** (more standard, uses existing Tidal REPL)

```bash
# Start GHCi with custom BootTidal
ghci -ghci-script BootTidalHooked.hs

# BootTidalHooked.hs connects to our WS server
# Intercepts d1/d2/etc and sends updates via HTTP or another socket
```

**Recommended:** Start with Option 1 (embedded) for MVP, migrate to Option 2 for production.

**Deliverable:** Typing `d1 "bd sd hh"` in backend REPL → broadcasts JSON to frontend.

---

### Phase 4: Live Visualization (Week 7)

**Goal:** Frontend receives patterns, renders with existing viz components.

#### Copy Viz Components from PSD3 Demo:

**Files to vendor:**
1. `Viz/PatternTree/CombinatorTree.purs` - Combinator forest
2. `Viz/PatternTree/Sunburst.purs` - Sunburst diagrams
3. `Viz/PatternTree/Types.purs` - Shared types
4. `Component/AlgoraveViz.purs` - Main viz component (adapt)

**Adapt AlgoraveViz for live updates:**

```purescript
module Component.LiveViz where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Data.Map as Map
import TidalLive.Protocol (PatternUpdate(..))
import Viz.CombinatorForest (drawCombinatorForest)

type Track =
  { name :: String
  , pattern :: PatternTree
  , combinators :: Array Combinator
  , active :: Boolean
  }

type State =
  { tracks :: Map String Track
  , selectedLayout :: LayoutMode
  }

data Action
  = ReceivePatternUpdate PatternUpdate
  | ToggleTrack String
  | SwitchLayout LayoutMode

-- When pattern update arrives from WebSocket
handleAction (ReceivePatternUpdate update) = do
  state <- H.get
  let newTrack =
        { name: update.track
        , pattern: update.patternTree
        , combinators: update.combinators
        , active: true
        }
  H.modify_ _ { tracks = Map.insert update.track newTrack state.tracks }
  -- Trigger re-render
  rerenderViz

-- Render uses existing components
render state =
  HH.div_
    [ renderToolbar state
    , HH.div [ HP.ref vizRef ]  -- SVG container
        []
    ]

-- After render, use Effect to call D3 viz
rerenderViz = do
  state <- H.get
  let tracksArray = Map.values state.tracks
  H.liftEffect $
    drawCombinatorForest "#viz-container"
      (buildCombinatorTrees tracksArray)
      (trackMetadata tracksArray)
```

**Integration with WebSocket:**

```purescript
-- In Main.purs
handleWSMessage :: String -> Action
handleWSMessage json =
  case parsePatternUpdate json of
    Right update -> LiveVizAction (ReceivePatternUpdate update)
    Left err -> LogError err
```

**Deliverable:** Typing Tidal code → instantly see tree/sunburst update in browser!

---

### Phase 5: Pattern Editing (Week 8-9)

**Goal:** Click to edit patterns, send changes back to Tidal.

#### Click Handlers on Viz:

Already implemented in CombinatorTree refactor:
- Click combinator → toggle enable/disable
- Click sunburst → future: edit pattern

**Add editing logic:**

```purescript
-- In LiveViz component
data Action
  = ...
  | ToggleCombinator String Int  -- track, combinator index
  | EditPattern String PatternTree

handleAction (ToggleCombinator track combIdx) = do
  state <- H.get
  case Map.lookup track state.tracks of
    Just t -> do
      let newCombs = toggleAt combIdx t.combinators
      let newTrack = t { combinators = newCombs }
      H.modify_ _ { tracks = Map.insert track newTrack state.tracks }
      -- Send to backend
      sendPatternEdit track (rebuildPattern newTrack)
    Nothing -> pure unit

sendPatternEdit :: String -> PatternTree -> H.HalogenM State Action ...
sendPatternEdit track tree = do
  let msg = encodeJson { type: "editPattern", track, patternTree: tree }
  H.liftEffect $ sendWebSocket msg
```

#### Backend: Handle Edits (`src/TidalBridge/Server.hs`):

```haskell
handleMessage :: Message -> ServerState -> IO ()
handleMessage msg state = case msg of
  EditPattern track tree -> do
    -- Convert tree back to mini-notation
    let code = astToMiniNotation tree
    -- Evaluate in Tidal
    streamReplace (tidalStream state) track (parseBP code)
    -- Broadcast update to all clients
    broadcastPatternUpdate track tree
  ...
```

**Deliverable:** Click to toggle combinator → Tidal pattern updates → sound changes!

---

### Phase 6: Text Editor (Week 10)

**Goal:** Monaco-style editor for typing mini-notation directly.

#### Add CodeMirror or Monaco:

```purescript
module Component.TextEditor where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Effect.Aff (Aff)
import Web.HTML.HTMLTextAreaElement as TextArea

type State =
  { code :: String
  , track :: String
  }

data Action
  = UpdateCode String
  | SendToTidal

render state =
  HH.div_
    [ HH.label_ [ HH.text ("Edit " <> state.track) ]
    , HH.textarea
        [ HP.value state.code
        , HE.onValueInput UpdateCode
        ]
    , HH.button [ HE.onClick \_ -> SendToTidal ]
        [ HH.text "Send to Tidal" ]
    ]

handleAction = case _ of
  UpdateCode newCode ->
    H.modify_ _ { code = newCode }

  SendToTidal -> do
    state <- H.get
    -- Parse mini-notation
    case parseMiniNotation state.code of
      Right tree -> sendPatternEdit state.track tree
      Left err -> showError err
```

**Future:** Full Monaco integration with syntax highlighting, autocomplete.

**Deliverable:** Type `bd*4 sd hh` → click Send → pattern updates live.

---

### Phase 7: Polish & Deploy (Week 11-12)

#### Polish:

1. **Error handling**
   - Parse errors shown in UI
   - Connection status indicator
   - Reconnect on disconnect

2. **State persistence**
   - Save patterns to localStorage
   - Restore on reload

3. **UI/UX**
   - Smooth transitions
   - Loading states
   - Keyboard shortcuts

4. **Performance**
   - Debounce visualization updates
   - Virtual scrolling for many tracks

#### Deployment:

**Docker Compose:**

```yaml
# docker-compose.yml
version: '3.8'
services:
  backend:
    build:
      context: ./backend
      dockerfile: Dockerfile
    ports:
      - "9160:9160"
    environment:
      - TIDAL_LATENCY=0.2

  frontend:
    build:
      context: ./frontend
      dockerfile: Dockerfile
    ports:
      - "8080:80"
    depends_on:
      - backend
```

**Deploy to:**
- Local: `docker-compose up`
- VPS: DigitalOcean, Linode, etc.
- Could even deploy to Fly.io or Railway

---

## Technical Deep Dives

### A. Pattern Serialization Strategy

The **hardest part** of this project is extracting AST from Tidal.

**Options ranked by difficulty:**

1. **String round-trip** (easiest, lossy)
   - Tidal code → parse to Pattern → show pattern → send string
   - Frontend parses string back to AST
   - **Pro:** No Tidal changes needed
   - **Con:** Loses structure, fragile

2. **Sampled events** (medium, partial structure)
   - Query Pattern for events in 0-4 cycle window
   - Infer structure from event timing
   - **Pro:** Works with evaluated patterns
   - **Con:** Loses some combinator info

3. **Mini-notation parser fork** (hard, complete)
   - Fork Tidal's parser, add AST extraction
   - Maintain parallel to Tidal
   - **Pro:** Full AST access
   - **Con:** Maintenance burden

4. **Contribute to Tidal** (ideal, long-term)
   - Add AST extraction to Tidal itself
   - PR to upstream
   - **Pro:** Proper solution
   - **Con:** Requires Tidal maintainer buy-in

**Recommendation for MVP:** Option 3 (parser fork), with plan to upstream (Option 4).

### B. Collaborative Editing

If multiple clients edit the same pattern:

**Conflict resolution strategies:**

1. **Last-write-wins** (simplest)
   - Most recent edit wins
   - No conflict resolution

2. **Operational Transform** (standard)
   - Transform concurrent operations
   - Like Google Docs
   - Library: ShareDB (JS), could wrap in PureScript

3. **CRDTs** (elegant)
   - Conflict-free replicated data types
   - Always converge
   - More complex for tree structures

**Recommendation:** Start with last-write-wins, add optimistic UI updates.

### C. Latency Handling

From user edit → sound change:

```
User clicks (0ms)
  → Frontend state update (0ms)
  → Send WebSocket (5ms)
  → Backend receives (10ms)
  → Tidal evaluates (20ms)
  → OSC to SuperDirt (25ms)
  → Sound plays (50-100ms total)
```

**To maintain responsiveness:**

1. **Optimistic updates** - Change viz immediately
2. **Confirmation** - Backend sends ack
3. **Rollback** - If server rejects, revert UI

### D. Testing Strategy

**Backend tests:**
- Pattern codec round-trips
- WebSocket message handling
- Tidal hook interception

**Frontend tests:**
- PatternTree codecs
- Component rendering
- WebSocket mock

**Integration tests:**
- Full stack with Docker
- Selenium/Playwright for E2E

---

## Resources & Prior Art

### Study These Projects:

1. **Estuary** - https://github.com/dktr0/estuary
   - PureScript + Haskell, exactly this architecture
   - Has solved Tidal ↔ WebSocket already
   - STUDY THIS FIRST!

2. **TidalVibe** - https://github.com/tidalcycles/tidalvibe
   - Visual pattern editor (older, simpler)

3. **Strudel** - https://strudel.tidalcycles.org
   - Web-based Tidal (JS port)
   - Different approach (no backend), but good UX reference

### Documentation:

- TidalCycles docs: https://tidalcycles.org/
- Tidal source: https://github.com/tidalcycles/Tidal
- PSD3 chimeric viz: `notes/CHIMERIC_VIZ_FOR_TIDAL.md` (this repo)

---

## Success Criteria

**MVP (Minimum Viable Product):**
- [ ] Backend connects to Tidal
- [ ] Frontend connects to backend via WebSocket
- [ ] Type `d1 "bd sd hh"` → see visualization update
- [ ] Click to toggle combinator → hear sound change

**V1 (Usable Product):**
- [ ] All basic Tidal patterns visualized correctly
- [ ] Visual editing for basic operations
- [ ] Text editor for direct code input
- [ ] Deploy to public server

**V2 (Production Quality):**
- [ ] Collaborative editing
- [ ] Pattern library / presets
- [ ] Export/import patterns
- [ ] Mobile-friendly UI

---

## Timeline Estimate

| Phase | Duration | Deliverable |
|-------|----------|-------------|
| 0. Setup | 1 week | Scaffolding works |
| 1. WebSocket | 1 week | Connection established |
| 2. Serialization | 2 weeks | Codec works both ways |
| 3. Tidal Integration | 2 weeks | Backend captures patterns |
| 4. Visualization | 1 week | Frontend renders live |
| 5. Editing | 2 weeks | Click to edit |
| 6. Text Editor | 1 week | Type to edit |
| 7. Polish | 2 weeks | Production ready |
| **Total** | **12 weeks** | **~3 months** |

Assumes ~20 hrs/week effort, one developer.

---

## Open Questions

1. **Tidal version compatibility?**
   - Target Tidal 1.9.x (current stable)
   - Or Tidal 2.0 (if/when released)?

2. **SuperCollider integration?**
   - Show which samples are loaded?
   - Visualize audio levels?
   - Scope/waveforms?

3. **Multiple backends?**
   - Could support multiple Tidal instances (band mode)
   - Each musician has their own backend
   - All visualized in one frontend

4. **Persistence?**
   - Save sessions to disk?
   - Git integration for pattern versioning?
   - Database for shared pattern library?

---

## Next Steps for Implementation

**When starting this project:**

1. **Clone this plan** to new `tidal-live` repo
2. **Study Estuary** source code first (1-2 days)
3. **Prototype pattern codec** separately (test Tidal AST extraction)
4. **Follow phases sequentially** - don't skip ahead
5. **Vendor PSD3 components** - copy needed modules initially
6. **Iterate quickly** - get something rendering ASAP, then refine

**First commit should be:**
```bash
mkdir tidal-live
cd tidal-live
git init
cp PATH_TO_THIS_PLAN.md ./README.md
# Follow Phase 0...
```

Good luck, future Claude! This is a great project with clear prior art to learn from.
The foundation (chimeric viz) is already proven. Now it's just integration work.

---

**Document version:** 1.0
**Last updated:** 2025-12-25
**Author:** Claude (Opus 4.5)
**For:** Future implementation of Tidal Live Visual Editor
