module Stories.Spago.Lenses where


import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

-- chooseSimNodes :: (SpagoSimNode -> Boolean) -> State -> Maybe (Array SpagoSimNode)
-- chooseSimNodes fn state = filter fn <$> preview _modelNodes state

-- chooseSimLinks :: (SpagoGraphLinkID -> Boolean) -> State -> Maybe (Array SpagoGraphLinkID)
-- chooseSimLinks fn state = filter fn <$> preview _modelLinks state

-- COMPOSITIONs of optics 

-- -- BOILERPLATE record field accessors
-- _selections :: forall a r. Lens' { selections :: a | r } a
-- _selections = prop (Proxy :: Proxy "selections")

-- _data :: forall a r. Lens' { "data" :: a | r } a
-- _data = prop (Proxy :: Proxy "data")

-- _graph :: forall a r. Lens' { graph :: a | r } a
-- _graph = prop (Proxy :: Proxy "graph")

-- _tree :: forall a r. Lens' { tree :: a | r } a
-- _tree = prop (Proxy :: Proxy "tree")

-- _maps :: forall a r. Lens' { maps :: a | r } a
-- _maps = prop (Proxy :: Proxy "maps")

-- _key :: forall a r. Lens' { key :: a | r } a
-- _key = prop (Proxy :: Proxy "key")



-- _inSim :: forall a r. Lens' { simDataCooked :: a | r } a
-- _inSim = prop (Proxy :: Proxy "simDataCooked")

-- _forces :: forall a r. Lens' { activeForces :: a | r } a
-- _forces = prop (Proxy :: Proxy "activeForces")


-- _nodesForSim :: forall p. 
--      Strong p
--   => Choice p
--   => p (Array SpagoSimNode) (Array SpagoSimNode)
--   -> p State State 
-- _nodesForSim  = _forSim <<< _Just <<< _data <<< _nodes

-- _linksForSim :: forall p. 
--      Strong p
--   => Choice p
--   => p (Array SpagoGraphLinkID) (Array SpagoGraphLinkID)
--   -> p State State
-- _linksForSim  = _forSim <<< _Just <<< _data <<< _links

-- _nodesInSim :: forall p. 
--      Strong p
--   => Choice p
--   => p (Array SpagoSimNode) (Array SpagoSimNode)
--   -> p State State 
-- _nodesInSim  = _inSim <<< _Just <<< _data <<< _nodes

-- _linksInSim :: forall t378 p t394 t397.
--   Strong p =>
--   Choice p =>
--   p (Array SpagoGraphLinkRecord) (Array SpagoGraphLinkRecord) -> 
--   p { simDataCooked :: Maybe { "data" :: { links :: (Array SpagoGraphLinkRecord) | t397 } | t394 } | t378 } 
--     { simDataCooked :: Maybe { "data" :: { links :: (Array SpagoGraphLinkRecord) | t397 } | t394 } | t378 }
-- _linksInSim  = _inSim <<< _Just <<< _data <<< _links

-- _countDataNodes :: State -> Int
-- _countDataNodes state = length $ fromMaybe [] $ preview _nodesInSim state

-- _countDataLinks :: State -> Int
-- _countDataLinks state = length $ fromMaybe [] $ preview _linksInSim state

-- _nodeSelection :: forall p. 
--      Strong p
--   => Choice p
--   => p D3Selection_ D3Selection_
--   -> p State State 
-- _nodeSelection = _inSim <<< _Just <<< _selections <<< _nodes

-- _linksShown :: forall p. 
--      Strong p
--   => Choice p
--   => p D3Selection_ D3Selection_
--   -> p State State 
-- _linksShown = _inSim <<< _Just <<< _selections <<< _links
-- _forSim :: forall a r. Lens' { Staging :: a | r } a
-- _forSim = prop (Proxy :: Proxy "Staging")

-- _state :: forall a r. Lens' { simulationState :: a | r } a
-- _state = prop (Proxy :: Proxy "simulationState")

-- BOILERPLATE - getters and setters
-- _nodeSelectionGet :: State -> Maybe D3Selection_
-- _nodeSelectionGet = preview _nodeSelection 

-- _nodeSelectionSet :: (D3Selection_ -> D3Selection_) -> State -> State
-- _nodeSelectionSet = over _nodeSelection 

-- _linksShownGet :: State -> Maybe D3Selection_
-- _linksShownGet = preview _linksShown 

-- _linksShownSet :: (D3Selection_ -> D3Selection_) -> State -> State
-- _linksShownSet = over _linksShown 

-- _nodeDataGet :: State -> Maybe (Array SpagoSimNode)
-- _nodeDataGet = preview _dataNodes 

-- _nodeDataSet :: (Array SpagoSimNode -> Array SpagoSimNode) -> State -> State
-- _nodeDataSet = over _dataNodes 

-- _linkDataGet :: State -> Maybe (Array (D3LinkSwizzled SpagoSimNode SpagoLinkData))
-- _linkDataGet :: State -> Maybe (Array SpagoGraphLinkID)
-- _linkDataGet = preview _dataLinks 

-- _linkDataSet :: (Array (D3LinkSwizzled SpagoSimNode SpagoLinkData) -> Array (D3LinkSwizzled SpagoSimNode SpagoLinkData)) -> State -> State
-- _linkDataSet = over _dataLinks 
