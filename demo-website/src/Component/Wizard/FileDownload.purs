module PSD3.Wizard.FileDownload where

import Prelude

import Data.Array (foldl, uncons)
import Data.Foldable (for_)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import PSD3.Wizard.Generator (GeneratedFile)

-- | JSZip wrapper for creating zip files in the browser
foreign import data JSZip :: Type

-- | Create a new JSZip instance
foreign import createZip :: Effect JSZip

-- | Add a file to the zip
foreign import addFile :: JSZip -> String -> String -> Effect Unit

-- | Generate zip file and trigger browser download
foreign import downloadZipImpl :: JSZip -> String -> EffectFnAff Unit

-- | Copy text to clipboard
foreign import copyToClipboard :: String -> Effect Boolean

-- | Download all generated files as a zip
downloadAsZip :: String -> Array GeneratedFile -> Aff Unit
downloadAsZip projectName files = do
  zip <- liftEffect createZip
  -- Add all files to the zip
  liftEffect $ for_ files \file ->
    addFile zip file.filename file.content
  -- Generate and download
  fromEffectFnAff $ downloadZipImpl zip (projectName <> ".zip")

-- | Copy all files to clipboard as formatted text
copyFilesToClipboard :: Array GeneratedFile -> Effect Boolean
copyFilesToClipboard files =
  let
    formatted = formatFilesForClipboard files
  in
    copyToClipboard formatted

-- | Format files as readable text for clipboard
formatFilesForClipboard :: Array GeneratedFile -> String
formatFilesForClipboard files =
  let
    repeatString :: String -> Int -> String
    repeatString str n = foldl (<>) "" (replicateArray n str)

    separator = "\n" <> repeatString "=" 80 <> "\n"
    formatFile file =
      "File: " <> file.filename <> "\n" <>
      repeatString "-" 80 <> "\n" <>
      file.content
    fileTexts = map formatFile files
  in
    intercalateArray separator fileTexts
  where
    replicateArray :: forall a. Int -> a -> Array a
    replicateArray n x = if n <= 0 then [] else [x] <> replicateArray (n - 1) x

    intercalateArray :: String -> Array String -> String
    intercalateArray sep arr = maybe "" (\{ head: first, tail: rest } ->
      if rest == [] then first else first <> sep <> intercalateArray sep rest
    ) (uncons arr)
