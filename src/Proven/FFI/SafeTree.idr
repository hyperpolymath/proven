-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeTree operations (61/75)
module Proven.FFI.SafeTree

import Proven.SafeTree
import Proven.Core

%default total

encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

export
proven_idris_tree_is_empty : Int -> Int
proven_idris_tree_is_empty nodeCount = encodeBool (nodeCount == 0)

export
proven_idris_tree_height_diff : Int -> Int -> Int
proven_idris_tree_height_diff leftHeight rightHeight =
  if leftHeight >= rightHeight
    then leftHeight - rightHeight
    else rightHeight - leftHeight

export
proven_idris_tree_is_balanced : Int -> Int
proven_idris_tree_is_balanced heightDiff = encodeBool (heightDiff <= 1)

export
proven_idris_tree_max_height : Int -> Int -> Int
proven_idris_tree_max_height leftHeight rightHeight =
  if leftHeight >= rightHeight then leftHeight else rightHeight
