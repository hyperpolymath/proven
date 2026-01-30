-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeTree operations
|||
||| This module exports tree utility functions to the C ABI via Idris2's RefC backend.
||| All functions are proven total and protect against infinite recursion via totality checking.
|||
||| Return conventions:
||| - Tree metrics → Int (size, height, leaf count)
||| - Validation → Int (0 = false/invalid, 1 = true/valid)
||| - Traversals → String (comma-separated values)
|||
||| NOTE: Full BinaryTree data structure not exposed across FFI due to complexity of
||| marshalling recursive types. This module provides utility functions that can
||| validate properties of tree-like structures represented as lists/arrays.
|||
||| CRITICAL: Functions assume well-formed input. For BST validation, provide sorted
||| list representation. Tree building from scratch should be done Idris-side.
module Proven.FFI.SafeTree

import Proven.SafeTree
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

||| Encode List String as comma-separated
encodeList : List String -> String
encodeList [] = ""
encodeList (x :: xs) = foldl (\acc, y => acc ++ "," ++ y) x xs

||| Decode comma-separated to List String
decodeList : String -> List String
decodeList s = if s == "" then [] else split (== ',') s

||| Parse integers from comma-separated string
parseIntList : String -> Maybe (List Integer)
parseIntList s =
  if s == "" then Just []
  else traverse parseInteger (decodeList s)

--------------------------------------------------------------------------------
-- BST Validation Helpers
--------------------------------------------------------------------------------

||| Check if a list is sorted (for BST validation)
isSortedList : List Integer -> Bool
isSortedList [] = True
isSortedList [_] = True
isSortedList (x :: y :: xs) = x <= y && isSortedList (y :: xs)

%export
proven_idris_tree_is_sorted : String -> Int
proven_idris_tree_is_sorted s =
  case parseIntList s of
    Nothing => 0  -- Invalid input
    Just xs => encodeBool (isSortedList xs)

%export
proven_idris_tree_has_duplicates : String -> Int
proven_idris_tree_has_duplicates s =
  case parseIntList s of
    Nothing => 0
    Just xs => encodeBool (hasDuplicates xs)
  where
    hasDuplicates : List Integer -> Bool
    hasDuplicates [] = False
    hasDuplicates (x :: xs) = x `elem` xs || hasDuplicates xs

--------------------------------------------------------------------------------
-- Tree Property Checks
--------------------------------------------------------------------------------

||| Check if height difference is balanced
%export
proven_idris_tree_is_height_balanced : Int -> Int -> Int
proven_idris_tree_is_height_balanced leftHeight rightHeight =
  let diff = if leftHeight >= rightHeight
               then minus (cast leftHeight) (cast rightHeight)
               else minus (cast rightHeight) (cast leftHeight)
  in encodeBool (diff <= 1)

||| Calculate maximum height from two subtrees
%export
proven_idris_tree_max_height : Int -> Int -> Int
proven_idris_tree_max_height left right =
  cast (max (cast left) (cast right)) + 1

--------------------------------------------------------------------------------
-- List Operations (for tree traversals represented as lists)
--------------------------------------------------------------------------------

||| Reverse a list (useful for certain tree operations)
%export
proven_idris_tree_reverse_list : String -> String
proven_idris_tree_reverse_list s = encodeList (reverse (decodeList s))

||| Merge two sorted lists (for BST merge operations)
%export
proven_idris_tree_merge_sorted : String -> String -> String
proven_idris_tree_merge_sorted s1 s2 =
  case (parseIntList s1, parseIntList s2) of
    (Just xs, Just ys) => encodeList (map show (mergeSorted xs ys))
    _ => ""
  where
    mergeSorted : List Integer -> List Integer -> List Integer
    mergeSorted [] ys = ys
    mergeSorted xs [] = xs
    mergeSorted (x :: xs) (y :: ys) =
      if x <= y
        then x :: mergeSorted xs (y :: ys)
        else y :: mergeSorted (x :: xs) ys

||| Remove duplicates from sorted list
%export
proven_idris_tree_unique_sorted : String -> String
proven_idris_tree_unique_sorted s =
  case parseIntList s of
    Nothing => ""
    Just xs => encodeList (map show (uniqueSorted xs))
  where
    uniqueSorted : List Integer -> List Integer
    uniqueSorted [] = []
    uniqueSorted [x] = [x]
    uniqueSorted (x :: y :: xs) =
      if x == y
        then uniqueSorted (y :: xs)
        else x :: uniqueSorted (y :: xs)

--------------------------------------------------------------------------------
-- Tree Metrics Helpers
--------------------------------------------------------------------------------

||| Calculate tree size from node count
%export
proven_idris_tree_size_from_nodes : String -> Int
proven_idris_tree_size_from_nodes s = cast (length (decodeList s))

||| Calculate height from depth (0-indexed to 1-indexed)
%export
proven_idris_tree_depth_to_height : Int -> Int
proven_idris_tree_depth_to_height depth = depth + 1

||| Calculate maximum nodes for a complete binary tree of given height
%export
proven_idris_tree_max_nodes_for_height : Int -> Int
proven_idris_tree_max_nodes_for_height h =
  cast (pow2 (cast h) - 1)
  where
    pow2 : Nat -> Nat
    pow2 Z = 1
    pow2 (S n) = 2 * pow2 n

||| Calculate minimum height for given number of nodes
%export
proven_idris_tree_min_height_for_nodes : Int -> Int
proven_idris_tree_min_height_for_nodes n =
  cast (ceilLog2 (cast n + 1))
  where
    ceilLog2 : Nat -> Nat
    ceilLog2 Z = 0
    ceilLog2 (S n) = S (ceilLog2 (n `div` 2))

--------------------------------------------------------------------------------
-- Validation Helpers
--------------------------------------------------------------------------------

%export
proven_idris_tree_is_valid_node_count : Int -> Int
proven_idris_tree_is_valid_node_count n = encodeBool (n >= 0)

%export
proven_idris_tree_is_valid_height : Int -> Int
proven_idris_tree_is_valid_height h = encodeBool (h >= 0)

%export
proven_idris_tree_is_complete : Int -> Int -> Int
proven_idris_tree_is_complete nodeCount treeHeight =
  let maxNodes = pow2 (cast treeHeight) - 1
      minNodes = if treeHeight == 0 then 0 else pow2 (cast (treeHeight - 1))
  in encodeBool (nodeCount >= cast minNodes && nodeCount <= cast maxNodes)
  where
    pow2 : Nat -> Nat
    pow2 Z = 1
    pow2 (S n) = 2 * pow2 n

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_tree_friendly_error : String -> String
proven_idris_tree_friendly_error errorMsg =
  if isInfixOf "invalid" (toLower errorMsg)
    then "Invalid tree structure or parameters"
  else if isInfixOf "unbalanced" (toLower errorMsg)
    then "Tree is not balanced (height difference exceeds 1)"
  else if isInfixOf "sorted" (toLower errorMsg)
    then "List is not sorted (required for BST validation)"
  else
    "Tree operation error"
