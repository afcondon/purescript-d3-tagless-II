# ABOUT Interpreters

## Interpreter for Selections

```purescript
class (Monad m) <= SelectionM selection m where
  appendTo    :: selection -> Element -> Array (SelectionAttribute) -> m selection
  attach      :: Selector selection                                  -> m selection
  simpleJoin  :: ∀ datum. selection -> Element -> (Array datum) -> (Datum_ -> Index_)
                                                                     -> m selection
```

**(as you can see, actual grammar a bit richer, but still small)**

The full `SelectionM` type class includes:

```purescript
class (Monad m) <= SelectionM selection m where
  appendTo         :: selection -> Element -> Array (SelectionAttribute) -> m selection
  attach           :: Selector selection                                  -> m selection
  simpleJoin       :: ∀ datum. selection -> Element -> (Array datum) -> (Datum_ -> Index_)
                                                                          -> m selection
  selectUnder      :: selection -> Selector selection                    -> m selection
  filterSelection  :: selection -> Selector selection                    -> m selection
  mergeSelections  :: selection -> selection                             -> m selection
  setAttributes    :: selection -> Array (SelectionAttribute)            -> m Unit
  on               :: selection -> Behavior selection                    -> m Unit
  updateJoin       :: ∀ datum. selection -> Element -> (Array datum) -> (Datum_ -> Index_)
                      -> m { enter :: selection, exit :: selection, update :: selection }
```

## Interpreter for Simulations

**(a lot more complicated, much Effect, many State etc)**

The `SimulationM` type class extends `SelectionM` with physics simulation capabilities:

```purescript
class (Monad m, SelectionM selection m) <= SimulationM selection m | m -> selection where
  -- control
  start :: m Unit
  stop  :: m Unit

  -- config
  setConfigVariable :: SimVariable -> m Unit

  -- management of forces
  actualizeForces :: Map Label ForceStatus -> m Unit

  -- management of data (nodes and links)
  setNodes :: forall d. Array (D3_SimulationNode d) -> m (Array (D3_SimulationNode d))
  setLinks :: forall d r id. (Eq id) =>
              Array (D3Link id r) -> Array (D3_SimulationNode d) -> (Datum_ -> Index_)
              -> m (Array (D3LinkSwizzled (D3_SimulationNode d) r))

  -- updating with selections (less type-safe but necessary for dynamic updates)
  setNodesFromSelection :: selection -> m Unit
  setLinksFromSelection :: selection -> (Datum_ -> Boolean) -> m Unit

  -- merge new data with existing simulation state
  mergeNewDataWithSim :: forall d r id. (Eq id) =>
    selection ->                                    -- nodes selection
    (Datum_ -> Index_) ->                          -- nodes keyFn
    selection ->                                    -- links selection
    (Datum_ -> Index_) ->                          -- links KeyFn
    RawData d r id ->                              -- links and nodes raw data
    m { links :: (Array (D3LinkSwizzled (D3_SimulationNode d) r))
      , nodes :: (Array (D3_SimulationNode d))
      }

  -- tick functions
  addTickFunction    :: Label -> Step selection -> m Unit
  removeTickFunction :: Label                    -> m Unit
```

## Multiple Interpreters FTW

The Finally Tagless encoding enables multiple interpretations of the same visualization code:

* **"D3" interpreter** - consumes "script" and produces visualisation in the DOM - just like D3.js script would

* **"Printer" interpreter** - consumes "script" and produces a textual representation of the script actions - in principle, this could be extended to output the equivalent JavaScript.

* **"Meta" interpreter** - consumes "script" and produces...another Tree structure, representing the structure of the visualisation itself. This Tree structure can then be fed back in to, for example, the "D3" interpreter to produce a diagrammatic documentation of the script.

