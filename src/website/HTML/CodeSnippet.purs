module CodeSnippet where

import Prelude

import CodeSnippets as CS
import Data.Array ((:))
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import PSD3.Shared.Utilities (highlightAllLineNumbers)

-- | Render a code snippet with Prism.js syntax highlighting, filename, and line numbers
-- |
-- | Usage:
-- |   codeSnippet "mySnippet" "purescript"
-- |
-- | The snippet will be extracted from your source files by adding this comment:
-- |   SNIPPET: mySnippet src/path/to/file.purs 10-25
-- |
-- | This shows the source filename and line numbers matching the original file.
-- | Note: Call triggerPrismHighlighting in your component's Initialize action
codeSnippet :: forall w i. String -> String -> HH.HTML w i
codeSnippet snippetName language =
  let info = CS.getSnippetInfo snippetName
      startLine = getStartLine info.lines
  in HH.div
      [ HP.classes [ HH.ClassName "code-snippet-container" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "code-snippet-meta" ] ]
          [ HH.text $ info.source <> " (lines " <> info.lines <> ")" ]
      , HH.pre
          [ HP.classes [ HH.ClassName $ "language-" <> language, HH.ClassName "line-numbers" ]
          , HP.attr (HH.AttrName "data-start") (show startLine)
          ]
          [ HH.code_
              [ HH.text info.content ]
          ]
      ]

-- | Trigger Prism highlighting after code snippets are rendered
-- | Call this in your component's Initialize action
triggerPrismHighlighting :: forall m. MonadEffect m => m Unit
triggerPrismHighlighting = H.liftEffect highlightAllLineNumbers

-- | Extract the starting line number from a line range string like "10-25"
getStartLine :: String -> Int
getStartLine lineRange =
  case split (Pattern "-") lineRange of
    [start, _] -> fromMaybe 1 $ fromString start
    _ -> 1

-- | Render a code snippet without metadata (simple version)
codeSnippetSimple :: forall w i. String -> String -> HH.HTML w i
codeSnippetSimple snippetName language =
  HH.pre
    [ HP.class_ $ HH.ClassName $ "language-" <> language ]
    [ HH.code_
        [ HH.text $ getSnippetContent snippetName ]
    ]

-- | Render a code snippet with additional CSS classes
codeSnippetWithClasses :: forall w i. String -> String -> Array String -> HH.HTML w i
codeSnippetWithClasses snippetName language classes =
  let info = CS.getSnippetInfo snippetName
      startLine = getStartLine info.lines
  in HH.div
      [ HP.classes [ HH.ClassName "code-snippet-container" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "code-snippet-meta" ] ]
          [ HH.text $ info.source <> " (lines " <> info.lines <> ")" ]
      , HH.pre
          [ HP.classes $ HH.ClassName <$> (("language-" <> language) : ("line-numbers" : classes))
          , HP.attr (HH.AttrName "data-start") (show startLine)
          ]
          [ HH.code_
              [ HH.text info.content ]
          ]
      ]

-- | Render a code snippet with metadata (alias for codeSnippet)
codeSnippetWithMeta :: forall w i. String -> String -> HH.HTML w i
codeSnippetWithMeta = codeSnippet

-- | Get just the content of a snippet (for direct use)
getSnippetContent :: String -> String
getSnippetContent = CS.getSnippet
