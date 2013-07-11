{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Types where

import Prelude hiding (length, lines, break, span, catch)
import Data.Text hiding (concat, map, zip, reverse, null)
import Data.Aeson

data IndexedWord  = IndexedWord Int Text deriving (Show, Eq)
data Node = Node Text Int Int Int deriving (Show, Eq)
data Link = Link Int Int deriving (Show, Eq)
data Whole = Whole [Node] [Link] EditorState deriving (Show, Eq)
data EditorState = EditorState Int Int Int Int Int FilePath deriving (Show, Eq)

editingFile :: EditorState -> FilePath
editingFile (EditorState _ _ _ _ _ path) = path

increment :: Int -> IndexedWord -> IndexedWord
increment j (IndexedWord i s) = IndexedWord (i + j) s

loc :: IndexedWord -> Int
loc (IndexedWord i s) = length s + i

instance ToJSON Whole where
    toJSON (Whole ns ls editor_state) = object [
        "nodes" .= toJSON ns,
        "links" .= toJSON ls,
        "editor_state" .= toJSON editor_state
        ]

instance ToJSON EditorState where
    toJSON (EditorState top_line bottom_line win_width col line fname) = object [
        "top_line" .= top_line,
        "bottom_line" .= bottom_line,
        "win_width" .= win_width,
        "col" .= col,
        "line" .= line,
        "fname" .= fname
        ]

instance ToJSON Node where
    toJSON (Node w c l grep_count) = object [
                                            "word" .= w,
                                            "col" .= c,
                                            "line" .= l,
                                            "length" .= length w,
                                            "count" .= grep_count
                                            ]

instance ToJSON Link where
    toJSON (Link s t) = object ["source" .= toJSON s, "target" .= toJSON t]

