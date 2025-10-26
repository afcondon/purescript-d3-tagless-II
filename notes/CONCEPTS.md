## GRAMMAR 

D3.js is, naturally, a JavaScript idiomatic library, making use of function chaining as a readable and easily-learnt API. 

PS<$>D3 formalizes this slightly whle retaining the general shape of the pattern and core concepts such as Selection, Attribute.

The basic grammar of PS<$>D3 is just this:

* we can *attach* our visualisation into the DOM, typically hooking into something like <div id="viz">
* we can build up whatever additional structure we want inside that, using *appendTo* to for example put an <svg> with some <g>'s inside the <div id="viz">
* we can apply *attributes* to the elements we've created: styles, ids, css classes and such
* we can apply *behaviors* to the elements we've created: things like zoom, drag and click event-handlers
* we can *join* some array of data at this point, such that when we next *add* something, say a <circle>, we will put _n_ <circle>s in, not just one.

In D3 the thing that is being acted on in this chain of functions is The Selection...

## THE SELECTIONM MONAD

Where functions would be chained in JavaScript, the natural counterpart in PureScript is a Monad, specifically the SelectionM Monad which embodies the core functions above and a few others. 

It looks like this:
```
class (Monad m) <= SelectionM selection m where
  attach          :: Selector selection -> m selection
  appendTo        :: selection -> Element -> Array (SelectionAttribute) -> m selection
  setAttributes   :: selection -> Array (SelectionAttribute) -> m Unit
  on              :: selection -> Behavior selection -> m Unit
  simpleJoin      :: ∀ datum.  selection -> Element -> (Array datum) -> (Datum_ -> Index_) -> m selection
  updateJoin      :: ∀ datum.  selection -> Element -> (Array datum) -> (Datum_ -> Index_) -> m { enter :: selection, exit :: selection, update :: selection }

  selectUnder     :: selection -> Selector selection -> m selection
  filterSelection :: selection -> Selector selection -> m selection
  mergeSelections :: selection -> selection -> m selection
  openSelection   :: selection -> Selector selection -> m selection
``` 

## FINALLY TAGLESS and INTERPRETERS

We want the library to be extensible and, in fact, D3 while it's the inspiration and a core target, is not the only possible implementation of the SelectionM monad. For this reason we choose the design pattern called Finally Tagless encoding and implement *interpreters* for the SelectionM. 

Furthermore, using this pattern, we can now *extend* the SelectionM monad as can be seen in the library where we provide a SimulationM monad which extends the our static DOM element visualizations by allowing them to move under the control of a Force Layout algorithm.

The SimulationM monad looks like:
```
class (Monad m, SelectionM selection m) <= SimulationM selection m | m -> selection where
  start :: m Unit
  stop  :: m Unit
  setConfigVariable    :: SimVariable -> m Unit
  actualizeForces:: Set Label -> m Unit
  setNodes :: forall d.   Array (D3_SimulationNode d) -> m (Array (D3_SimulationNode d))
  setLinks :: forall d r id. (Eq id) => Array (D3Link id r) -> Array (D3_SimulationNode d) -> (Datum_ -> Index_ ) -> m (Array (D3LinkSwizzled (D3_SimulationNode d) r))
  setNodesFromSelection :: selection -> m Unit
  setLinksFromSelection :: selection -> (Datum_ -> Boolean) -> m Unit
  mergeNewDataWithSim :: forall d r id. (Eq id) =>
    selection -> -- nodes selection
    (Datum_ -> Index_) -> -- nodes keyFn
    selection -> -- links selection
    (Datum_ -> Index_) -> -- links KeyFn
    RawData d r id -> -- links and nodes raw data
    m { links :: (Array (D3LinkSwizzled (D3_SimulationNode d) r)), nodes :: (Array (D3_SimulationNode d))}
  simulationHandle :: m D3Simulation_
  addTickFunction    :: Label -> Step selection -> m Unit
  removeTickFunction :: Label                   -> m Unit
```

## TYPE-SAFE ATTRIBUTE SYSTEM

A key innovation of D3 is the easy, natural way one can go from static to data-driven attributes, for example going from putting _n_ identical circles into the DOM vs putting _n_ circles of differing colours and sizes into the DOM. This is the essence (with join/simulation) of Data Drive Documents, ie D3.

In PureScript we need a little bit of typeclass trickery to get the same clean syntax, and we use it to enable interchangeable, type-safe attribute setters.
For example, we can use an attribute *cx* (the center x position of a circle) which can take:
* a numeric value OR,
* a lambda function which takes the datum we are using to a numeric value OR,
* a lambda function which takes the datum AND the index of that datum in the selection 