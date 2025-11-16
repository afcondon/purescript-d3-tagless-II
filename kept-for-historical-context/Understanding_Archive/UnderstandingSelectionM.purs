module PSD3.Understanding.UnderstandingSelectionM where

import Prelude

import CodeSnippet (codeSnippet, triggerPrismHighlighting)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.DocsHeader as DocsHeader
import PSD3.Website.Types (Section(..))
import Type.Proxy (Proxy(..))

type State = Unit

data Action = Initialize

type Slots =
  ( docsHeader :: forall q. H.Slot q Void Unit
  )

_docsHeader = Proxy :: Proxy "docsHeader"

component :: forall q i o. H.Component q i o Aff
component = H.mkComponent
  { initialState: \_ -> unit
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: State -> H.ComponentHTML Action Slots Aff
render _ =
  HH.div
    [ HP.classes [ HH.ClassName "explanation-page" ] ]
    [ -- Docs Header
      HH.slot_ _docsHeader unit DocsHeader.component
        { currentSection: Just UnderstandingSection }

    -- Main content
    , HH.div
        [ HP.classes [ HH.ClassName "explanation-content" ] ]
        [ -- Page title
          HH.h1
            [ HP.classes [ HH.ClassName "explanation-title" ] ]
            [ HH.text "The SelectionM Monad" ]

        -- Grammar
        , HH.h2_ [ HH.text "Grammar" ]
        , HH.p_
            [ HH.text "D3.js is, naturally, a JavaScript idiomatic library, making use of function chaining as a readable and easily-learnt API." ]
        , HH.p_
            [ HH.text "PS<$>D3 formalizes this slightly while retaining the general shape of the pattern and core concepts such as Selection, Attribute." ]
        , HH.p_ [ HH.text "The basic grammar of PS<$>D3 is just this:" ]
        , HH.ul_
            [ HH.li_ [ HH.text "we can ", HH.em_ [ HH.text "attach" ], HH.text " our visualisation into the DOM, typically hooking into something like <div id=\"viz\">" ]
            , HH.li_ [ HH.text "we can build up whatever additional structure we want inside that, using ", HH.em_ [ HH.text "appendTo" ], HH.text " to for example put an <svg> with some <g>'s inside the <div id=\"viz\">" ]
            , HH.li_ [ HH.text "we can apply ", HH.em_ [ HH.text "attributes" ], HH.text " to the elements we've created: styles, ids, css classes and such" ]
            , HH.li_ [ HH.text "we can apply ", HH.em_ [ HH.text "behaviors" ], HH.text " to the elements we've created: things like zoom, drag and click event-handlers" ]
            , HH.li_ [ HH.text "we can ", HH.em_ [ HH.text "join" ], HH.text " some array of data at this point, such that when we next ", HH.em_ [ HH.text "add" ], HH.text " something, say a <circle>, we will put ", HH.em_ [ HH.text "n" ], HH.text " <circle>s in, not just one." ]
            ]
        , HH.p_ [ HH.text "In D3 the thing that is being acted on in this chain of functions is The Selection..." ]

        -- The SelectionM Monad
        , HH.h2_ [ HH.text "The SelectionM Monad" ]
        , HH.p_
            [ HH.text "Where functions would be chained in JavaScript, the natural counterpart in PureScript is a Monad, specifically the SelectionM Monad which embodies the core functions above and a few others." ]
        , HH.p_ [ HH.text "It looks like this:" ]
        -- SNIPPET: selectionMClass src/lib/PSD3/Capabilities/Selection.purs 85-258
        , codeSnippet "selectionMClass" "haskell"
        ]
    ]

handleAction :: forall o. Action -> H.HalogenM State Action Slots o Aff Unit
handleAction = case _ of
  Initialize -> triggerPrismHighlighting
