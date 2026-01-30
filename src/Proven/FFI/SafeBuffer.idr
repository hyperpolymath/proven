-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeBuffer operations
|||
||| This module exports safe buffer operations to the C ABI via Idris2's RefC backend.
||| All functions are proven total and protect against buffer overflows/underflows.
|||
||| Return conventions:
||| - Buffer operations → (status: Int, result: String)
|||   - status = 0: Success, result contains buffer content or value
|||   - status = 1: Error (overflow, underflow, out of bounds)
||| - Buffer queries → Int (length, capacity, remaining)
||| - Validation → Int (0 = false, 1 = true)
|||
||| CRITICAL: All buffer operations are bounds-checked. Writing beyond capacity fails.
|||
||| NOTE: Buffers serialized as comma-separated strings for FFI compatibility.
||| Capacity and length tracked separately.
module Proven.FFI.SafeBuffer

import Proven.SafeBuffer
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

||| Encode buffer content as comma-separated string
encodeContent : List String -> String
encodeContent [] = ""
encodeContent (x :: xs) = foldl (\acc, y => acc ++ "," ++ y) x xs

||| Decode comma-separated string to list
decodeContent : String -> List String
decodeContent s = if s == "" then [] else split (== ',') s

||| Encode Maybe buffer as (status, content)
encodeMaybeBuffer : Maybe (DynBuffer String) -> (Int, String, Int)
encodeMaybeBuffer Nothing = (1, "", 0)
encodeMaybeBuffer (Just buf) = (0, encodeContent buf.content, cast buf.capacity)

||| Encode Maybe String as (status, value)
encodeMaybeString : Maybe String -> (Int, String)
encodeMaybeString Nothing = (1, "")
encodeMaybeString (Just s) = (0, s)

--------------------------------------------------------------------------------
-- Buffer Creation
--------------------------------------------------------------------------------

%export
proven_idris_buffer_create : Int -> (Int, String, Int)
proven_idris_buffer_create capacity =
  if capacity >= 0
    then (0, "", capacity)  -- Empty buffer
    else (1, "", 0)         -- Invalid capacity

%export
proven_idris_buffer_from_list : String -> Int -> (Int, String, Int)
proven_idris_buffer_from_list content capacity =
  let items = decodeContent content
      len = length items
  in if cast len <= capacity
       then (0, content, capacity)
       else (1, "", 0)  -- Content exceeds capacity

--------------------------------------------------------------------------------
-- Buffer Queries
--------------------------------------------------------------------------------

%export
proven_idris_buffer_length : String -> Int
proven_idris_buffer_length content =
  cast (length (decodeContent content))

%export
proven_idris_buffer_is_empty : String -> Int
proven_idris_buffer_is_empty content =
  encodeBool (content == "")

%export
proven_idris_buffer_is_full : String -> Int -> Int
proven_idris_buffer_is_full content capacity =
  let buf = MkDynBuffer (cast capacity) (decodeContent content)
  in encodeBool (isFull buf)

%export
proven_idris_buffer_remaining : String -> Int -> Int
proven_idris_buffer_remaining content capacity =
  let buf = MkDynBuffer (cast capacity) (decodeContent content)
  in cast (remaining buf)

--------------------------------------------------------------------------------
-- Buffer Write Operations
--------------------------------------------------------------------------------

%export
proven_idris_buffer_write : String -> String -> Int -> (Int, String, Int)
proven_idris_buffer_write content item capacity =
  let buf = MkDynBuffer (cast capacity) (decodeContent content)
  in encodeMaybeBuffer (write item buf)

%export
proven_idris_buffer_write_many : String -> String -> Int -> (Int, String, Int)
proven_idris_buffer_write_many content items capacity =
  let buf = MkDynBuffer (cast capacity) (decodeContent content)
      itemList = decodeContent items
  in encodeMaybeBuffer (writeMany itemList buf)

--------------------------------------------------------------------------------
-- Buffer Read Operations
--------------------------------------------------------------------------------

%export
proven_idris_buffer_read_at : String -> Int -> (Int, String)
proven_idris_buffer_read_at content idx =
  let buf = MkDynBuffer 0 (decodeContent content)  -- Capacity doesn't matter for reads
  in encodeMaybeString (readAt (cast idx) buf)

%export
proven_idris_buffer_peek_first : String -> (Int, String)
proven_idris_buffer_peek_first content =
  case decodeContent content of
    [] => (1, "")
    (x :: _) => (0, x)

%export
proven_idris_buffer_peek_last : String -> (Int, String)
proven_idris_buffer_peek_last content =
  case reverse (decodeContent content) of
    [] => (1, "")
    (x :: _) => (0, x)

--------------------------------------------------------------------------------
-- Buffer Manipulation
--------------------------------------------------------------------------------

%export
proven_idris_buffer_clear : String -> Int -> (Int, String, Int)
proven_idris_buffer_clear _ capacity = (0, "", capacity)

%export
proven_idris_buffer_take : String -> Int -> String
proven_idris_buffer_take content n =
  let buf = MkDynBuffer 0 (decodeContent content)
  in encodeContent (takeBuf (cast n) buf)

%export
proven_idris_buffer_drop : String -> Int -> Int -> (Int, String, Int)
proven_idris_buffer_drop content n capacity =
  let buf = MkDynBuffer (cast capacity) (decodeContent content)
      dropped = dropBuf (cast n) buf
  in (0, encodeContent dropped.content, cast dropped.capacity)

%export
proven_idris_buffer_slice : String -> Int -> Int -> String
proven_idris_buffer_slice content start len =
  let buf = MkDynBuffer 0 (decodeContent content)
  in encodeContent (slice (cast start) (cast len) buf)

--------------------------------------------------------------------------------
-- Buffer Copying
--------------------------------------------------------------------------------

%export
proven_idris_buffer_copy_to : String -> String -> Int -> (Int, String, Int)
proven_idris_buffer_copy_to srcContent destContent destCapacity =
  let src = MkDynBuffer 0 (decodeContent srcContent)
      dest = MkDynBuffer (cast destCapacity) (decodeContent destContent)
  in encodeMaybeBuffer (copyTo src dest)

%export
proven_idris_buffer_append : String -> String -> Int -> (Int, String, Int)
proven_idris_buffer_append content1 content2 capacity =
  let buf = MkDynBuffer (cast capacity) (decodeContent content1)
      items = decodeContent content2
  in encodeMaybeBuffer (writeMany items buf)

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

%export
proven_idris_buffer_is_valid_index : String -> Int -> Int
proven_idris_buffer_is_valid_index content idx =
  let len = length (decodeContent content)
  in encodeBool (idx >= 0 && cast idx < len)

%export
proven_idris_buffer_is_valid_capacity : Int -> Int
proven_idris_buffer_is_valid_capacity capacity =
  encodeBool (capacity >= 0)

%export
proven_idris_buffer_would_overflow : String -> Int -> Int -> Int
proven_idris_buffer_would_overflow content itemCount capacity =
  let currentLen = length (decodeContent content)
  in encodeBool (currentLen + itemCount > cast capacity)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_buffer_friendly_error : String -> String
proven_idris_buffer_friendly_error errorMsg =
  if isInfixOf "overflow" (toLower errorMsg) || isInfixOf "full" (toLower errorMsg)
    then "Buffer overflow (exceeds capacity)"
  else if isInfixOf "underflow" (toLower errorMsg) || isInfixOf "empty" (toLower errorMsg)
    then "Buffer underflow (no elements to read)"
  else if isInfixOf "out of bounds" (toLower errorMsg) || isInfixOf "index" (toLower errorMsg)
    then "Index out of bounds"
  else if isInfixOf "capacity" (toLower errorMsg)
    then "Invalid buffer capacity"
  else
    "Buffer operation error"
