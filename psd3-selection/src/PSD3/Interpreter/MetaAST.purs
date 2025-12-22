module PSD3.Interpreter.MetaAST
  ( TreeAST(..)
  , toAST
  , prettyPrintAST
  , showElemType
  ) where

-- | Meta/AST interpreter for TreeAPI
-- | Produces a PureScript data structure representation of the tree
-- | This demonstrates that the tree itself IS data that can be manipulated

import Prelude

import Data.Array (length, intercalate)
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.AST (Tree(..))

-- | AST representation of a tree (simplified for display)
data TreeAST
  = NodeAST
      { name :: Maybe String
      , elemType :: String
      , attrCount :: Int
      , children :: Array TreeAST
      }
  | JoinAST
      { name :: String
      , key :: String
      , dataCount :: Int
      , hasTemplate :: Boolean
      }
  | NestedJoinAST
      { name :: String
      , key :: String
      , dataCount :: Int
      , hasTemplate :: Boolean
      }
  | UpdateJoinAST
      { name :: String
      , key :: String
      , dataCount :: Int
      , hasEnter :: Boolean
      , hasUpdate :: Boolean
      , hasExit :: Boolean
      , hasTemplate :: Boolean
      }
  | UpdateNestedJoinAST
      { name :: String
      , key :: String
      , dataCount :: Int
      , hasEnter :: Boolean
      , hasUpdate :: Boolean
      , hasExit :: Boolean
      , hasTemplate :: Boolean
      }

derive instance Generic TreeAST _

-- | Convert TreeAPI Tree to AST representation
toAST :: forall datum. Tree datum -> TreeAST
toAST tree = case tree of
  Node {name, elemType, attrs, children} ->
    NodeAST
      { name
      , elemType: showElemType elemType
      , attrCount: length attrs
      , children: map toAST children
      }

  Join {name, key, joinData} ->
    JoinAST
      { name
      , key
      , dataCount: length joinData
      , hasTemplate: true
      }

  NestedJoin {name, key, joinData} ->
    NestedJoinAST
      { name
      , key
      , dataCount: length joinData
      , hasTemplate: true
      }

  UpdateJoin {name, key, joinData, behaviors} ->
    UpdateJoinAST
      { name
      , key
      , dataCount: length joinData
      , hasEnter: case behaviors.enter of
          Just _ -> true
          Nothing -> false
      , hasUpdate: case behaviors.update of
          Just _ -> true
          Nothing -> false
      , hasExit: case behaviors.exit of
          Just _ -> true
          Nothing -> false
      , hasTemplate: true
      }

  UpdateNestedJoin {name, key, joinData, behaviors} ->
    UpdateNestedJoinAST
      { name
      , key
      , dataCount: length joinData
      , hasEnter: case behaviors.enter of
          Just _ -> true
          Nothing -> false
      , hasUpdate: case behaviors.update of
          Just _ -> true
          Nothing -> false
      , hasExit: case behaviors.exit of
          Just _ -> true
          Nothing -> false
      , hasTemplate: true
      }

-- | Pretty-print the AST
prettyPrintAST :: TreeAST -> String
prettyPrintAST ast = prettyPrint ast 0
  where
    prettyPrint :: TreeAST -> Int -> String
    prettyPrint tree level = case tree of
      NodeAST {name, elemType, attrCount, children} ->
        let nameStr = case name of
              Just n -> "\"" <> n <> "\""
              Nothing -> "anonymous"
            childrenStr = if length children > 0
              then "\n" <> indent (level + 1) <> ", children: " <> show (length children) <> " nodes\n"
                   <> intercalate "\n" (map (\c -> prettyPrint c (level + 2)) children)
              else ""
        in indent level <> "NodeAST\n"
           <> indent (level + 1) <> "{ name: " <> nameStr <> "\n"
           <> indent (level + 1) <> ", elemType: " <> elemType <> "\n"
           <> indent (level + 1) <> ", attrCount: " <> show attrCount
           <> childrenStr <> "\n"
           <> indent level <> "}"

      JoinAST {name, key, dataCount, hasTemplate} ->
        indent level <> "JoinAST\n"
        <> indent (level + 1) <> "{ name: \"" <> name <> "\"\n"
        <> indent (level + 1) <> ", key: \"" <> key <> "\"\n"
        <> indent (level + 1) <> ", dataCount: " <> show dataCount <> "\n"
        <> indent (level + 1) <> ", hasTemplate: " <> show hasTemplate <> "\n"
        <> indent level <> "}"

      NestedJoinAST {name, key, dataCount, hasTemplate} ->
        indent level <> "NestedJoinAST\n"
        <> indent (level + 1) <> "{ name: \"" <> name <> "\"\n"
        <> indent (level + 1) <> ", key: \"" <> key <> "\"\n"
        <> indent (level + 1) <> ", dataCount: " <> show dataCount <> "\n"
        <> indent (level + 1) <> ", hasTemplate: " <> show hasTemplate <> "\n"
        <> indent level <> "}"

      UpdateJoinAST {name, key, dataCount, hasEnter, hasUpdate, hasExit, hasTemplate} ->
        indent level <> "UpdateJoinAST\n"
        <> indent (level + 1) <> "{ name: \"" <> name <> "\"\n"
        <> indent (level + 1) <> ", key: \"" <> key <> "\"\n"
        <> indent (level + 1) <> ", dataCount: " <> show dataCount <> "\n"
        <> indent (level + 1) <> ", hasEnter: " <> show hasEnter <> "\n"
        <> indent (level + 1) <> ", hasUpdate: " <> show hasUpdate <> "\n"
        <> indent (level + 1) <> ", hasExit: " <> show hasExit <> "\n"
        <> indent (level + 1) <> ", hasTemplate: " <> show hasTemplate <> "\n"
        <> indent level <> "}"

      UpdateNestedJoinAST {name, key, dataCount, hasEnter, hasUpdate, hasExit, hasTemplate} ->
        indent level <> "UpdateNestedJoinAST\n"
        <> indent (level + 1) <> "{ name: \"" <> name <> "\"\n"
        <> indent (level + 1) <> ", key: \"" <> key <> "\"\n"
        <> indent (level + 1) <> ", dataCount: " <> show dataCount <> "\n"
        <> indent (level + 1) <> ", hasEnter: " <> show hasEnter <> "\n"
        <> indent (level + 1) <> ", hasUpdate: " <> show hasUpdate <> "\n"
        <> indent (level + 1) <> ", hasExit: " <> show hasExit <> "\n"
        <> indent (level + 1) <> ", hasTemplate: " <> show hasTemplate <> "\n"
        <> indent level <> "}"

    indent :: Int -> String
    indent n = power "  " n

    power :: String -> Int -> String
    power _ 0 = ""
    power s n = s <> power s (n - 1)

-- | Show element type as string
showElemType :: ElementType -> String
showElemType SVG = "SVG"
showElemType Group = "Group"
showElemType Circle = "Circle"
showElemType Rect = "Rect"
showElemType Path = "Path"
showElemType Line = "Line"
showElemType Text = "Text"
showElemType Div = "Div"
showElemType Span = "Span"
showElemType Table = "Table"
showElemType Tbody = "Tbody"
showElemType Thead = "Thead"
showElemType Tr = "Tr"
showElemType Td = "Td"
showElemType Th = "Th"
showElemType Defs = "Defs"
showElemType LinearGradient = "LinearGradient"
showElemType Stop = "Stop"
showElemType PatternFill = "PatternFill"
