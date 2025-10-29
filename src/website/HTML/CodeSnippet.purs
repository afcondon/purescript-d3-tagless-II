module CodeSnippet where

import Prelude

import CodeSnippets as CS
import Data.Array ((:))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Html.Renderer.Halogen as RH
import PSD3.Shared.Utilities (highlightString_)

-- | Render a code snippet with Prism.js syntax highlighting
-- |
-- | Usage:
-- |   codeSnippet "mySnippet" "purescript"
-- |
-- | The snippet will be extracted from your source files by adding this comment:
-- |   SNIPPET: mySnippet src/path/to/file.purs 10-25
codeSnippet :: forall w i. String -> String -> HH.HTML w i
codeSnippet snippetName language =
  HH.pre
    [ HP.class_ $ HH.ClassName $ "language-" <> language ]
    [ HH.code_
        [ RH.render_ $ highlightString_ $ getSnippetContent snippetName ]
    ]

-- | Render a code snippet with additional CSS classes
codeSnippetWithClasses :: forall w i. String -> String -> Array String -> HH.HTML w i
codeSnippetWithClasses snippetName language classes =
  HH.pre
    [ HP.classes $ HH.ClassName <$> (("language-" <> language) : classes) ]
    [ HH.code_
        [ RH.render_ $ highlightString_ $ getSnippetContent snippetName ]
    ]

-- | Render a code snippet with metadata (source file and line numbers)
codeSnippetWithMeta :: forall w i. String -> String -> HH.HTML w i
codeSnippetWithMeta snippetName language =
  let info = CS.getSnippetInfo snippetName
  in HH.div
      [ HP.classes [ HH.ClassName "code-snippet-container" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "code-snippet-meta" ] ]
          [ HH.text $ info.source <> " (lines " <> info.lines <> ")" ]
      , HH.pre
          [ HP.class_ $ HH.ClassName $ "language-" <> language ]
          [ HH.code_
              [ RH.render_ $ highlightString_ info.content ]
          ]
      ]

-- | Get just the content of a snippet (for direct use)
getSnippetContent :: String -> String
getSnippetContent = CS.getSnippet
