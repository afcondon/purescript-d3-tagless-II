module PSD3.CodeAtlas.Tabs.ExpandableBubbles where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad (when)
import Data.Array (filter, sort)
import Data.Array as Array
import Data.Foldable (maximum, traverse_)
import Data.Int as Data.Int
import Data.Lens (use)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (foldl)
import Data.Nullable (null, toNullable)
import Data.Number (cos, log, sqrt)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (mkEffectFn3)
import Halogen as H
import PSD3.Capabilities.Selection (class SelectionM, appendTo, attach, mergeSelections, on, openSelection, selectUnder, setAttributes, updateJoin)
import PSD3.Capabilities.Simulation (class SimulationM, class SimulationM2, addTickFunction, init, start, update)
import PSD3.CodeAtlas.Types (DeclarationsData, FunctionCallsData, ModuleGraphData, ModuleInfo)
import PSD3.Data.Node (D3Link_Unswizzled, D3Link_Swizzled, D3_SimulationNode(..), D3_VxyFxy, D3_XY)
import PSD3.Internal.Attributes.Sugar (classed, fill, onMouseEventEffectful, radius, remove, strokeColor, strokeOpacity, strokeWidth, text, transform', viewBox, width, height, x, x1, x2, y, y1, y2)
import PSD3.Internal.FFI (addModuleArrowMarker_, clearHighlights_, d3SelectionSelectAll_, drawInterModuleDeclarationLinks_, expandNodeById_, filterToConnectedNodes_, hideDetailsPanel_, highlightConnectedNodes_, keyIsID_, populateDetailsList_, setDetailsModuleName_, showDetailsPanel_, showModuleLabels_, simdragHorizontal_, switchToSpotlightForces_, unpinAllNodes_, unsafeSetField_, updateBubbleRadii_, updateNodeExpansion_)
import PSD3.Internal.Selection.Types (Behavior(..), DragBehavior(..), SelectionAttribute(..))
import PSD3.Internal.Simulation.Config as F
import PSD3.Internal.Simulation.Forces (createForce, createLinkForce)
import PSD3.Internal.Simulation.Types (D3SimulationState_, ForceType(..), RegularForceType(..), Step(..), allNodes, _handle)
import PSD3.Internal.Types (D3Selection_, Datum_, Element(..), Index_, MouseEvent(..))
import PSD3.Internal.Attributes.Instances (AttributeSetter(..), Attr(..), AttrBuilder(..))
import PSD3.Internal.Zoom (ScaleExtent(..), ZoomExtent(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Utility (getWindowWidthHeight)

-- | Module node data - extended to track expansion state
type BubbleNodeData row =
  ( id :: String
  , name :: String
  , loc :: Int
  , path :: String
  , expanded :: Boolean  -- New: track if this module is expanded
  | row
  )

type BubbleNode = D3_SimulationNode (BubbleNodeData + D3_XY + D3_VxyFxy + ())
type BubbleNodeRecord = Record (BubbleNodeData + D3_XY + D3_VxyFxy + ())

-- | Unsafe unboxing functions
unboxBubbleNode :: Datum_ -> BubbleNodeRecord
unboxBubbleNode datum =
  let (D3SimNode d) = unsafeCoerce datum
  in d

-- | Accessor functions for bubble node data
datum_ = {
    id: _.id <<< unboxBubbleNode
  , x: _.x <<< unboxBubbleNode
  , y: _.y <<< unboxBubbleNode
  , name: _.name <<< unboxBubbleNode
  , loc: _.loc <<< unboxBubbleNode
  , path: _.path <<< unboxBubbleNode
  , expanded: _.expanded <<< unboxBubbleNode
}

-- | Compute node radius from LOC
-- | When expanded, we'll multiply this by a factor
nodeRadius :: Boolean -> Int -> Number
nodeRadius expanded loc =
  let baseRadius = (sqrt (Data.Int.toNumber loc)) * 0.15 + 2.0  -- Reduced even more
  in if expanded
     then baseRadius * 4.0  -- Expanded nodes are 4x larger to show labeled declarations
     else baseRadius

-- | Compute color from module path
nodeColor :: String -> String
nodeColor path
  | String.take 4 path == "src/" =
      if String.contains (Pattern "src/lib/PSD3") path then "#2E7D32"
      else if String.contains (Pattern "src/lib/") path then "#4CAF50"
      else if String.contains (Pattern "src/website/") path then "#D32F2F"
      else "#1976D2"
  | otherwise = "#757575"

-- | Convert module graph data to bubble nodes (all start collapsed)
modulesToBubbleNodes :: Array ModuleInfo -> Array BubbleNode
modulesToBubbleNodes modules =
  Array.mapWithIndex (\i m ->
    let toNumber = Data.Int.toNumber
        theta = toNumber i * 0.5
        r = 50.0 + toNumber i * 2.0
    in
      D3SimNode
        { id: m.name
        , name: m.name
        , loc: m.loc
        , path: m.path
        , expanded: false  -- All start collapsed
        , x: r * cos theta
        , y: 0.0
        , vx: 0.0
        , vy: 0.0
        , fx: null
        , fy: null
        }
  ) modules

-- | Convert module dependencies to simulation links
modulesToLinks :: Array ModuleInfo -> Array D3Link_Unswizzled
modulesToLinks modules =
  let sourceModuleNames = Set.fromFoldable $ modules <#> _.name
      unpackLink :: D3Link_Unswizzled -> { id :: String, source :: String, target :: String }
      unpackLink = unsafeCoerce
      packLink :: { id :: String, source :: String, target :: String } -> D3Link_Unswizzled
      packLink = unsafeCoerce
  in Array.concat $ modules <#> \m ->
       filter (\link -> Set.member (unpackLink link).target sourceModuleNames)
         (m.depends <#> \dep -> packLink { id: m.name <> "->" <> dep, source: m.name, target: dep })

-- | Accessor functions for dependency links
unboxLink :: forall r. Datum_ -> { source :: BubbleNodeRecord, target :: BubbleNodeRecord | r }
unboxLink = unsafeCoerce

link_ = {
    source: _.source <<< unboxLink
  , target: _.target <<< unboxLink
}

-- | Build adjacency map for connected nodes lookup
buildAdjacencyMap :: Array D3Link_Unswizzled -> Map String (Set String)
buildAdjacencyMap links =
  let unpackLink :: D3Link_Unswizzled -> { source :: String, target :: String }
      unpackLink = unsafeCoerce
      addEdge acc link =
        let linkRec = unpackLink link
            sourceSet = fromMaybe Set.empty $ Map.lookup linkRec.source acc
            targetSet = fromMaybe Set.empty $ Map.lookup linkRec.target acc
        in acc
           # Map.insert linkRec.source (Set.insert linkRec.target sourceSet)
           # Map.insert linkRec.target (Set.insert linkRec.source targetSet)
  in foldl addEdge Map.empty links

-- | Build "depended on by" map - for each module, list modules that depend on it
-- | If A depends on B, then B is "depended on by" A
buildDependedOnByMap :: Array ModuleInfo -> Map String (Set String)
buildDependedOnByMap modules =
  let addDependencies acc m =
        foldl (\acc' dep ->
          let existingSet = fromMaybe Set.empty $ Map.lookup dep acc'
              newSet = Set.insert m.name existingSet
          in Map.insert dep newSet acc'
        ) acc m.depends
  in foldl addDependencies Map.empty modules

-- | Initialize the expandable bubbles graph
initialize :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  ModuleGraphData ->
  DeclarationsData ->
  m { svg :: D3Selection_
    , zoomGroup :: D3Selection_
    , nodesGroup :: D3Selection_
    , linksGroup :: D3Selection_
    , bubbleNodes :: Array BubbleNode
    , bubbleLinks :: Array D3Link_Unswizzled
    , declarationsData :: DeclarationsData
    , adjacencyMap :: Map String (Set String)
    , modulesMap :: Map String ModuleInfo
    , dependedOnByMap :: Map String (Set String)
    , detailsPanel :: D3Selection_
    , detailsModuleName :: D3Selection_
    , dependenciesList :: D3Selection_
    , dependedOnByList :: D3Selection_
    }
initialize graphData declsData = do
  liftEffect $ Console.log "=== INITIALIZE START ==="
  let sourceModules = filter (\m -> String.take 4 m.path == "src/") graphData.modules
  liftEffect $ Console.log $ "Found " <> show (Array.length sourceModules) <> " source modules"

  let bubbleNodes = modulesToBubbleNodes sourceModules
  liftEffect $ Console.log $ "Created " <> show (Array.length bubbleNodes) <> " bubble nodes"

  let bubbleLinks = modulesToLinks sourceModules
  liftEffect $ Console.log $ "Created " <> show (Array.length bubbleLinks) <> " links"

  let adjacencyMap = buildAdjacencyMap bubbleLinks
  liftEffect $ Console.log $ "Built adjacency map with " <> show (Map.size adjacencyMap) <> " entries"

  let modulesMap = Map.fromFoldable $ sourceModules <#> \m -> Tuple m.name m
  liftEffect $ Console.log $ "Built modules map with " <> show (Map.size modulesMap) <> " entries"

  let dependedOnByMap = buildDependedOnByMap sourceModules
  liftEffect $ Console.log $ "Built dependedOnBy map with " <> show (Map.size dependedOnByMap) <> " entries"

  liftEffect $ Console.log $ "Expandable Bubbles initialized with " <> show (Array.length sourceModules) <> " modules"

  -- Log LOC values and computed radii for debugging
  liftEffect $ Array.take 5 sourceModules # traverse_ \m -> do
    let r = nodeRadius false m.loc
    Console.log $ m.name <> " - LOC: " <> show m.loc <> ", radius: " <> show r

  (Tuple w h) <- liftEffect getWindowWidthHeight
  liftEffect $ Console.log $ "Window size: " <> show w <> "x" <> show h

  root <- attach "div.svg-container"
  liftEffect $ Console.log "Attached to svg-container"

  svg <- appendTo root Svg [ viewBox (-w / 2.0) (-h / 2.0) w h, classed "bubble-graph" ]
  liftEffect $ Console.log "Created SVG"

  -- Add details panel (outside SVG, as sibling)
  liftEffect $ Console.log "About to attach to expandable-bubbles-container"
  container <- attach "div.expandable-bubbles-container"
  liftEffect $ Console.log "Attached to expandable-bubbles-container"

  detailsPanel <- appendTo container Div [ classed "hover-details-panel hidden" ]
  liftEffect $ Console.log "Created details panel"

  -- Panel sections - content will be populated by FFI functions
  detailsModuleName <- appendTo detailsPanel Div [ classed "details-module-name" ]
  dependenciesList <- appendTo detailsPanel Div [ classed "details-list dependencies-list" ]
  dependedOnByList <- appendTo detailsPanel Div [ classed "details-list depended-on-by-list" ]
  liftEffect $ Console.log "Created panel sections"

  -- Add arrowhead marker definition for module links via FFI
  liftEffect $ addModuleArrowMarker_ svg

  zoomGroup <- appendTo svg Group [ classed "zoom-group" ]
  linksGroup <- appendTo zoomGroup Group [ classed "link", strokeColor "#999", strokeOpacity 0.4 ]
  nodesGroup <- appendTo zoomGroup Group [ classed "node", strokeColor "#fff", strokeWidth 1.5 ]

  -- Add fixed legend (outside zoom group so it stays fixed)
  legendGroup <- appendTo svg Group [
      classed "legend"
    , transform' (\_ -> "translate(" <> show (w/2.0 - 150.0) <> "," <> show (-h/2.0 + 30.0) <> ")")
    ]

  -- Legend title
  _ <- appendTo legendGroup Text [
      text "Declaration Types"
    , x 0.0
    , y 0.0
    , classed "legend-title"
    ]

  -- Legend items
  let legendItems =
        [ {name: "Functions/Values", color: "#2196F3", yOffset: 25.0}
        , {name: "Foreign Functions", color: "#00BCD4", yOffset: 50.0}
        , {name: "Data Types", color: "#4CAF50", yOffset: 75.0}
        , {name: "Type Classes", color: "#9C27B0", yOffset: 100.0}
        , {name: "Type Synonyms", color: "#FF9800", yOffset: 125.0}
        , {name: "Instances", color: "#E91E63", yOffset: 150.0}
        ]

  -- Draw each legend item
  _ <- traverse_ (\item -> do
    itemGroup <- appendTo legendGroup Group [
        transform' (\_ -> "translate(0," <> show item.yOffset <> ")")
      , classed "legend-item"
      ]

    -- Color circle
    _ <- appendTo itemGroup Circle [
        radius 8.0
      , fill item.color
      , x 0.0
      , y 0.0
      ]

    -- Label
    _ <- appendTo itemGroup Text [
        text item.name
      , x 15.0
      , y 5.0
      , classed "legend-label"
      ]

    pure unit
  ) legendItems

  -- Define forces - we have TWO collision forces that we toggle between
  let compactCollisionRadius :: Datum_ -> Index_ -> Number
      compactCollisionRadius datum _ =
        let node = unsafeCoerce datum :: BubbleNodeRecord
            baseRadius = nodeRadius false node.loc  -- Always use collapsed size
        in baseRadius + 2.0  -- Minimal padding for compact view

      spotlightCollisionRadius :: Datum_ -> Index_ -> Number
      spotlightCollisionRadius datum _ =
        let node = unsafeCoerce datum :: BubbleNodeRecord
            baseRadius = nodeRadius node.expanded node.loc
            padding = if node.expanded then 25.0 else 5.0
        in baseRadius + padding

      forces =
        [ createForce "manyBody-compact" (RegularForce ForceManyBody) allNodes [ F.strength (-50.0), F.theta 0.9, F.distanceMin 1.0 ]  -- Weak repulsion for compact
        , createForce "manyBody-spotlight" (RegularForce ForceManyBody) allNodes [ F.strength (-150.0), F.theta 0.9, F.distanceMin 1.0 ]  -- Moderate repulsion for spotlight
        , createForce "collision-compact" (RegularForce ForceCollide) allNodes [ F.radius compactCollisionRadius, F.strength 0.9, F.iterations 3.0 ]  -- Initial compact view
        , createForce "collision-spotlight" (RegularForce ForceCollide) allNodes [ F.radius spotlightCollisionRadius, F.strength 0.9, F.iterations 3.0 ]  -- Spotlight mode
        , createForce "center" (RegularForce ForceCenter) allNodes [ F.x 0.0, F.y 0.0, F.strength 0.2 ]
        , createLinkForce Nothing [ F.distance 100.0 ]  -- Shorter links
        ]
      activeForces = Set.fromFoldable [ "manyBody-compact", "collision-compact", "center", "links" ]  -- Start with compact forces

  -- Initialize simulation
  _ <- init
    { nodes: bubbleNodes
    , links: bubbleLinks
    , forces
    , activeForces
    , config: { alpha: 1.0, alphaTarget: 0.0, alphaMin: 0.001, alphaDecay: 0.0228, velocityDecay: 0.4 }
    , keyFn: keyIsID_
    , ticks: Map.fromFoldable []
    }

  -- Add zoom behavior
  _ <- svg `on` Zoom
    { extent: ZoomExtent { top: 0.0, left: 0.0, bottom: h, right: w }
    , scale: ScaleExtent 0.1 4.0
    , name: "BubbleGraph"
    , target: zoomGroup
    }

  pure
    { svg
    , zoomGroup
    , nodesGroup
    , linksGroup
    , bubbleNodes
    , bubbleLinks
    , declarationsData: declsData
    , adjacencyMap
    , modulesMap
    , dependedOnByMap
    , detailsPanel
    , detailsModuleName
    , dependenciesList
    , dependedOnByList
    }

-- | Update the graph with new data (including expansion states)
updateGraph :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  { nodesGroup :: D3Selection_
  , linksGroup :: D3Selection_
  , nodes :: Array BubbleNode
  , links :: Array D3Link_Unswizzled
  , inSpotlightMode :: Boolean
  } ->
  m Unit
updateGraph { nodesGroup, linksGroup, nodes, links, inSpotlightMode } = do
  -- Update simulation data
  enhanced <- update
    { nodes: Just nodes
    , links: Just links
    , nodeFilter: Nothing
    , linkFilter: Nothing
    , activeForces: Nothing
    , config: Nothing
    , keyFn: keyIsID_
    }

  -- Open selections
  node <- openSelection nodesGroup (show Group)
  link <- openSelection linksGroup (show Line)

  -- Apply General Update Pattern to nodes
  node' <- updateJoin node Group enhanced.nodes keyIsID_

  -- Enter: create new groups with circles
  nodeEnter <- appendTo node'.enter Group [ classed "node-group" ]
  _ <- appendTo nodeEnter Circle
    [ radius (\d -> nodeRadius (datum_.expanded d) (datum_.loc d))
    , fill (nodeColor <<< datum_.path)
    , classed "node-circle"
    ]
  _ <- appendTo nodeEnter Text
    [ text datum_.name
    , classed "node-label"
    , y (\d -> negate $ nodeRadius (datum_.expanded d) (datum_.loc d))  -- Position at top edge
    , fill (if inSpotlightMode then "#555" else "transparent")  -- Dark gray in spotlight mode
    , strokeWidth 0.0  -- No stroke
    , AttrT (AttributeSetter "text-anchor" (StringAttr (Static "middle")))  -- Center horizontally
    , AttrT (AttributeSetter "dominant-baseline" (StringAttr (Static "baseline")))  -- Align to bottom of text
    ]

  -- Exit: remove old nodes
  setAttributes node'.exit [ remove ]

  -- Update: modify existing nodes
  setAttributes node'.update [ classed "node-group" ]
  updateCircles <- selectUnder node'.update (show Circle)
  setAttributes updateCircles
    [ radius (\d -> nodeRadius (datum_.expanded d) (datum_.loc d))
    , fill (nodeColor <<< datum_.path)
    ]
  updateLabels <- selectUnder node'.update (show Text)
  setAttributes updateLabels
    [ text datum_.name
    , y (\d -> negate $ nodeRadius (datum_.expanded d) (datum_.loc d))  -- Position at top edge
    , fill (if inSpotlightMode then "#555" else "transparent")  -- Dark gray in spotlight mode
    ]

  -- Merge enter and update selections
  mergedNodes <- mergeSelections nodeEnter node'.update

  -- Add drag behavior
  _ <- mergedNodes `on` Drag (CustomDrag "bubbleGraph" simdragHorizontal_)

  -- Apply General Update Pattern to links
  link' <- updateJoin link Line enhanced.links keyIsID_

  -- Enter: create new lines with arrowheads
  linkEnter <- appendTo link'.enter Line
    [ strokeWidth 1.5
    , strokeColor "#999"
    , AttrT (AttributeSetter "marker-end" (StringAttr (Static "url(#module-arrow)")))
    ]

  -- Exit: remove old links
  setAttributes link'.exit [ remove ]

  -- Update: modify existing links
  setAttributes link'.update
    [ strokeWidth 1.5
    , strokeColor "#999"
    , AttrT (AttributeSetter "marker-end" (StringAttr (Static "url(#module-arrow)")))
    ]

  -- Merge enter and update selections
  mergedLinks <- mergeSelections linkEnter link'.update

  -- Set up tick functions for animation
  let translateNode :: Datum_ -> String
      translateNode d = "translate(" <> show (datum_.x d) <> "," <> show (datum_.y d) <> ")"
  addTickFunction "nodes" $ Step mergedNodes [ transform' translateNode ]
  addTickFunction "links" $ Step mergedLinks
    [ x1 (_.x <<< link_.source)
    , y1 (_.y <<< link_.source)
    , x2 (_.x <<< link_.target)
    , y2 (_.y <<< link_.target)
    ]

-- | Main entry point: draw the expandable bubbles graph
drawExpandableBubbles :: forall row m.
  Bind m =>
  MonadEffect m =>
  MonadState { simulation :: D3SimulationState_ | row } m =>
  SelectionM D3Selection_ m =>
  SimulationM D3Selection_ m =>
  SimulationM2 D3Selection_ m =>
  ModuleGraphData ->
  DeclarationsData ->
  FunctionCallsData ->
  String ->
  m Unit
drawExpandableBubbles graphData declsData callsData selector = do
  liftEffect $ Console.log "=== drawExpandableBubbles called ==="
  -- Initialize the graph
  initResult <- initialize graphData declsData
  liftEffect $ Console.log "=== initialize returned ==="
  let { svg
      , zoomGroup
      , nodesGroup
      , linksGroup
      , bubbleNodes
      , bubbleLinks
      , declarationsData
      , adjacencyMap
      , modulesMap
      , dependedOnByMap
      , detailsPanel
      , detailsModuleName
      , dependenciesList
      , dependedOnByList
      } = initResult

  -- Get simulation handle for reheating
  simHandle <- use _handle

  -- Track whether we've already filtered to a subgraph
  hasFilteredRef <- liftEffect $ Ref.new false

  -- Click handler: first click prunes + expands, subsequent clicks just expand/collapse
  let onClick _ datum _ = do
        let clickedId = datum_.id datum
            clickedNode = unboxBubbleNode datum

        hasFiltered <- Ref.read hasFilteredRef

        -- Only filter on the first click
        when (not hasFiltered) do
          let connected = fromMaybe Set.empty $ Map.lookup clickedId adjacencyMap
              connectedIds = Set.toUnfoldable connected :: Array String
              allConnected = Array.cons clickedId connectedIds

          Console.log $ "First click: filtering to " <> show (Array.length allConnected) <> " connected modules"
          liftEffect $ pure $ filterToConnectedNodes_ simHandle keyIsID_ allConnected
          Ref.write true hasFilteredRef

          -- Enter spotlight mode: show all labels
          Console.log "About to call showModuleLabels_"
          liftEffect $ showModuleLabels_ nodesGroup
          Console.log "Called showModuleLabels_"

          -- Switch from compact collision force to spotlight collision force
          Console.log "Switching to spotlight collision force"
          switchToSpotlightForces_ simHandle

        -- Toggle the expanded field directly on the node (it's a mutable JS object)
        let newExpanded = not clickedNode.expanded
        unsafeSetField_ "expanded" newExpanded datum

        Console.log $ clickedId <> if newExpanded then " expanded" else " collapsed"

        -- Expand/collapse with internal structure
        pure $ updateNodeExpansion_ simHandle nodeRadius declarationsData callsData datum

        -- ALSO expand all directly connected nodes (1-hop neighbors)
        let connected = fromMaybe Set.empty $ Map.lookup clickedId adjacencyMap
            connectedIds = Set.toUnfoldable connected :: Array String

        Console.log $ "Expanding " <> show (Array.length connectedIds) <> " connected modules"
        traverse_ (\connectedId ->
          expandNodeById_ simHandle nodeRadius declarationsData callsData connectedId newExpanded
        ) connectedIds

        -- Update inter-module declaration links
        drawInterModuleDeclarationLinks_ zoomGroup nodeRadius declarationsData callsData

  -- Render initial state with all nodes collapsed
  updateGraph
    { nodesGroup
    , linksGroup
    , nodes: bubbleNodes
    , links: bubbleLinks
    , inSpotlightMode: false
    }

  -- Mouseover handler: show module details
  let onMouseOver _ datum _ = do
        let hoveredId = datum_.id datum
        Console.log $ "Mouse over: " <> hoveredId
        case Map.lookup hoveredId modulesMap of
          Nothing -> Console.log $ "Module not found in map: " <> hoveredId
          Just moduleInfo -> do
            Console.log $ "Found module info, showing panel for: " <> moduleInfo.name
            -- Show the panel
            showDetailsPanel_ detailsPanel

            -- Set module name
            setDetailsModuleName_ detailsModuleName moduleInfo.name

            -- Populate dependencies list (sorted alphabetically)
            populateDetailsList_ dependenciesList (sort moduleInfo.depends)

            -- Populate depended-on-by list (sorted alphabetically)
            let dependedOnBy = fromMaybe Set.empty $ Map.lookup hoveredId dependedOnByMap
                dependedOnByList' = sort $ Set.toUnfoldable dependedOnBy :: Array String
            populateDetailsList_ dependedOnByList dependedOnByList'

  -- Mouseout handler: hide module details
  let onMouseOut _ _ _ = hideDetailsPanel_ detailsPanel

  -- Add event handlers to node groups
  initialNodes <- openSelection nodesGroup (show Group)
  setAttributes initialNodes
    [ onMouseEventEffectful MouseClick onClick
    , onMouseEventEffectful MouseEnter onMouseOver
    , onMouseEventEffectful MouseLeave onMouseOut
    ]

  -- Start the simulation
  start
  pure unit
