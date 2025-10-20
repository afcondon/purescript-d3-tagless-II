handleAction :: forall m. Bind m => MonadAff m => MonadState State m => 
  Action -> m Unit
handleAction = case _ of
  ToggleCard _cardState -> _cardState %= not

  Initialize -> do
    detached <- H.liftEffect $ eval_D3M $ removeExistingSVG "div.svg-container"

    text <- H.liftAff $ readSnippetFiles "PrintTreeHandleActions"
    _handlerCode .= text

    treeJSON <- H.liftAff $ getTreeViaAJAX "./data/flare-2.json"

    case treeJSON of
      (E.Left err) -> pure unit
      (E.Right (tree :: TreeJson_)) -> do
        textRep     <- H.liftAff $ Tree.getPrintTree =<< makeModel TidyTree Radial tree
        H.modify_ (\st -> st { tree = textRep } )
        pure unit
    pure unit
