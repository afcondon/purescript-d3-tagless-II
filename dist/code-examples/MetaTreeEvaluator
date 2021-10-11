-- | evaluate a tree first using the "metatree" interpreter, then draw the RESULTING (syntax) tree using D3 interpreter
drawMetaTree :: TreeJson_ -> Aff Unit
drawMetaTree json =
  MetaTree.drawTree =<< makeModel TidyTree Vertical =<< Tree.getMetaTreeJSON =<< makeModel TidyTree Radial json
