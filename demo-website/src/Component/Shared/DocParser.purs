module PSD3.Shared.DocParser where

import Prelude

import Data.Array as Array
import Data.String as String
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | Extract documentation header and remaining code from PureScript source
parseDocumentation :: String -> { docLines :: Array String, codeLines :: Array String }
parseDocumentation source =
  let
    lines = String.split (String.Pattern "\n") source
    { init: docLines, rest: codeLines } = splitAtModuleDeclaration lines
  in
    { docLines: extractDocComments docLines
    , codeLines
    }

-- | Split lines at module declaration
-- | Look for lines that START with "module " (ignoring leading whitespace)
splitAtModuleDeclaration :: Array String -> { init :: Array String, rest :: Array String }
splitAtModuleDeclaration lines =
  case Array.findIndex isModuleDeclaration lines of
    Nothing -> { init: [], rest: lines }
    Just idx ->
      { init: Array.take idx lines
      , rest: Array.drop idx lines
      }
  where
    isModuleDeclaration line =
      let trimmed = String.trim line
      in String.take 7 trimmed == "module "

-- | Extract and clean doc comment lines (remove '-- |' prefix)
extractDocComments :: Array String -> Array String
extractDocComments lines =
  lines
    # Array.filter (String.contains (String.Pattern "-- |"))
    # map stripDocPrefix

-- | Remove '-- |' prefix and leading space
stripDocPrefix :: String -> String
stripDocPrefix line =
  line
    # String.replaceAll (String.Pattern "-- |") (String.Replacement "")
    # String.trim

-- | Convert markdown lines to HTML elements
markdownToHtml :: forall w i. Array String -> Array (HH.HTML w i)
markdownToHtml lines =
  processLines lines []

-- | Process lines into HTML elements, handling block-level structures
processLines :: forall w i. Array String -> Array (HH.HTML w i) -> Array (HH.HTML w i)
processLines lines acc = case Array.uncons lines of
  Nothing -> Array.reverse acc
  Just { head, tail } ->
    if String.null (String.trim head) then
      -- Empty line - add spacing
      processLines tail acc
    else if String.contains (String.Pattern "```") head then
      -- Code block
      let { code, remaining } = extractCodeBlock tail
      in processLines remaining (Array.cons (renderCodeBlock code) acc)
    else if isHeading head then
      -- Heading
      processLines tail (Array.cons (renderHeading head) acc)
    else if isBulletPoint head then
      -- Start of list
      let { listItems, remaining } = extractList lines
      in processLines remaining (Array.cons (renderList listItems) acc)
    else if isNumberedPoint head then
      -- Start of numbered list
      let { listItems, remaining } = extractNumberedList lines
      in processLines remaining (Array.cons (renderOrderedList listItems) acc)
    else
      -- Regular paragraph
      processLines tail (Array.cons (renderParagraph head) acc)

-- | Check if line is a heading
isHeading :: String -> Boolean
isHeading line = String.take 2 line == "##"

-- | Check if line starts with bullet point
isBulletPoint :: String -> Boolean
isBulletPoint line =
  let trimmed = String.trim line
  in String.take 2 trimmed == "- " || String.take 2 trimmed == "* "

-- | Check if line starts with number
isNumberedPoint :: String -> Boolean
isNumberedPoint line =
  let trimmed = String.trim line
  in case String.indexOf (String.Pattern ". ") trimmed of
    Just idx -> idx <= 2 && idx > 0
    Nothing -> false

-- | Extract code block until closing ```
extractCodeBlock :: Array String -> { code :: Array String, remaining :: Array String }
extractCodeBlock lines =
  case Array.findIndex (String.contains (String.Pattern "```")) lines of
    Nothing -> { code: lines, remaining: [] }
    Just idx ->
      { code: Array.take idx lines
      , remaining: Array.drop (idx + 1) lines
      }

-- | Extract consecutive list items
extractList :: Array String -> { listItems :: Array String, remaining :: Array String }
extractList lines =
  let
    { init, rest } = Array.span isBulletPoint lines
  in
    { listItems: map (String.replaceAll (String.Pattern "- ") (String.Replacement "")
                   >>> String.replaceAll (String.Pattern "* ") (String.Replacement "")
                   >>> String.trim) init
    , remaining: rest
    }

-- | Extract consecutive numbered list items
extractNumberedList :: Array String -> { listItems :: Array String, remaining :: Array String }
extractNumberedList lines =
  let
    { init, rest } = Array.span isNumberedPoint lines
    cleanItem line =
      case String.indexOf (String.Pattern ". ") line of
        Just idx -> String.drop (idx + 2) line # String.trim
        Nothing -> line
  in
    { listItems: map cleanItem init
    , remaining: rest
    }

-- | Render heading
renderHeading :: forall w i. String -> HH.HTML w i
renderHeading line =
  let
    level = countHashes line
    text = String.drop level line # String.trim
  in
    case level of
      2 -> HH.h2_ [ HH.text text ]
      3 -> HH.h3_ [ HH.text text ]
      4 -> HH.h4_ [ HH.text text ]
      _ -> HH.h2_ [ HH.text text ]

-- | Count leading # characters
countHashes :: String -> Int
countHashes line =
  if String.take 1 line == "#" then
    1 + countHashes (String.drop 1 line)
  else
    0

-- | Render paragraph with inline formatting
renderParagraph :: forall w i. String -> HH.HTML w i
renderParagraph text =
  HH.p_ (parseInline text)

-- | Render unordered list
renderList :: forall w i. Array String -> HH.HTML w i
renderList items =
  HH.ul_ (map (\item -> HH.li_ (parseInline item)) items)

-- | Render ordered list
renderOrderedList :: forall w i. Array String -> HH.HTML w i
renderOrderedList items =
  HH.ol_ (map (\item -> HH.li_ (parseInline item)) items)

-- | Render code block
renderCodeBlock :: forall w i. Array String -> HH.HTML w i
renderCodeBlock codeLines =
  HH.pre_
    [ HH.code
        [ HP.classes [ HH.ClassName "language-purescript" ] ]
        [ HH.text (String.joinWith "\n" codeLines) ]
    ]

-- | Parse inline formatting (bold, code, etc.)
parseInline :: forall w i. String -> Array (HH.HTML w i)
parseInline text =
  parseInlineSegments text []

-- | Parse text into segments with inline formatting
parseInlineSegments :: forall w i. String -> Array (HH.HTML w i) -> Array (HH.HTML w i)
parseInlineSegments text acc =
  if String.null text then
    Array.reverse acc
  else
    case findNextInlineFormat text of
      Just { format, before, content, after } ->
        let
          beforeElem = if String.null before then [] else [HH.text before]
          formattedElem = case format of
            "**" -> HH.strong_ [ HH.text content ]
            "`" -> HH.code_ [ HH.text content ]
            _ -> HH.text content
        in
          parseInlineSegments after (Array.cons formattedElem (acc <> beforeElem))
      Nothing ->
        Array.reverse (Array.cons (HH.text text) acc)

-- | Find next inline format marker
findNextInlineFormat :: String -> Maybe { format :: String, before :: String, content :: String, after :: String }
findNextInlineFormat text =
  case findBold text of
    Just result -> Just result
    Nothing -> findCode text

-- | Find bold text (**text**)
findBold :: String -> Maybe { format :: String, before :: String, content :: String, after :: String }
findBold text =
  case String.indexOf (String.Pattern "**") text of
    Nothing -> Nothing
    Just startIdx ->
      let
        afterStart = String.drop (startIdx + 2) text
      in
        case String.indexOf (String.Pattern "**") afterStart of
          Nothing -> Nothing
          Just endIdx ->
            Just
              { format: "**"
              , before: String.take startIdx text
              , content: String.take endIdx afterStart
              , after: String.drop (endIdx + 2) afterStart
              }

-- | Find inline code (`code`)
findCode :: String -> Maybe { format :: String, before :: String, content :: String, after :: String }
findCode text =
  case String.indexOf (String.Pattern "`") text of
    Nothing -> Nothing
    Just startIdx ->
      let
        afterStart = String.drop (startIdx + 1) text
      in
        case String.indexOf (String.Pattern "`") afterStart of
          Nothing -> Nothing
          Just endIdx ->
            Just
              { format: "`"
              , before: String.take startIdx text
              , content: String.take endIdx afterStart
              , after: String.drop (endIdx + 1) afterStart
              }
