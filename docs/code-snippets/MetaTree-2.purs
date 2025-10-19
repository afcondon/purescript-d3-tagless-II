handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard _cardState -> _cardState %= not

  Initialize -> do
    text <- H.liftAff $ readSnippetFiles "MetaTreeDraw"
    _drawCode .= text
    text <- H.liftAff $ readSnippetFiles "MetaTreeEvaluator"
    _evaluatorCode .= text
    text <- H.liftAff $ readSnippetFiles "MetaTreeHandleActions"
    _handlerCode .= text
    detached <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.d3story"

    treeJSON <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"

    case treeJSON of
      (E.Left err) -> pure unit
      (E.Right (tree :: TreeJson_)) -> do
        _     <- H.liftAff $ drawMetaTree tree
        pure unit
    pure unit
