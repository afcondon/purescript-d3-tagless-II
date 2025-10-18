handleAction :: forall m. 
  Bind m => 
  MonadAff m => 
  MonadState State m =>
  Action -> m Unit
handleAction = case _ of

  ToggleCard _cardState -> _cardState %= not

  Initialize -> do
    notebook' <- traverse substituteSnippetCells lesMisNotebook
    _notebook .= notebook'

    response <- H.liftAff $ AJAX.get ResponseFormat.string "./data/miserables.json"
    let graph = readGraphFromFileContents response

    (_forceStatuses <<< _forceStatus forceNames.center)       %= (const ForceActive)
    (_forceStatuses <<< _forceStatus forceNames.manyBodyNeg)  %= (const ForceActive)
    (_forceStatuses <<< _forceStatus forceNames.collision)    %= (const ForceActive)
    (_forceStatuses <<< _forceStatus forceNames.links)        %= (const ForceActive)

    (_forceStatuses <<< _forceStatus forceNames.manyBodyPos)  %= (const ForceDisabled)
    
    runWithD3_Simulation do
      statuses <- use _forceStatuses
      actualizeForces statuses
      LesMis.draw graph "div.svg-container"

  Finalize ->  pure unit

  ToggleForce name -> do
    toggleForceByName name
    runWithD3_Simulation do
      statuses <- use _forceStatuses
      actualizeForces statuses
      setConfigVariable $ Alpha 0.7
      start

  Freeze  -> runWithD3_Simulation $ setConfigVariable $ Alpha 0.0
  Reheat  -> do
    runWithD3_Simulation do
      setConfigVariable $ Alpha 0.7
      start
