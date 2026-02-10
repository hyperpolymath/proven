-- SPDX-License-Identifier: Palimpsest-MPL-1.0
||| SafeBuffer - Safe buffer operations with bounds checking
|||
||| This module provides safe buffer operations that prevent
||| buffer overflows and underflows through bounds checking.
module Proven.SafeBuffer
import Data.String
import Data.List

import public Proven.Core
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Buffer Types
--------------------------------------------------------------------------------

||| A fixed-size buffer with compile-time size
public export
data Buffer : (size : Nat) -> Type -> Type where
  MkBuffer : Vect size a -> Buffer size a

||| A buffer with runtime-checked bounds
public export
record DynBuffer a where
  constructor MkDynBuffer
  capacity : Nat
  content : List a

--------------------------------------------------------------------------------
-- Fixed-Size Buffer Operations
--------------------------------------------------------------------------------

||| Create an empty buffer filled with a default value
public export
empty : (size : Nat) -> (defaultValue : a) -> Buffer size a
empty size defaultValue = MkBuffer (replicate size defaultValue)

||| Get element at index (type-safe, no runtime check needed)
public export
index : (idx : Fin size) -> Buffer size a -> a
index idx (MkBuffer v) = index idx v

||| Set element at index
public export
setAt : (idx : Fin size) -> a -> Buffer size a -> Buffer size a
setAt idx val (MkBuffer v) = MkBuffer (replaceAt idx val v)

||| Get buffer size (constant, known at compile time)
public export
bufferSize : Buffer size a -> Nat
bufferSize {size} _ = size

||| Convert buffer to list
public export
toList : Buffer size a -> List a
toList (MkBuffer v) = toList v

--------------------------------------------------------------------------------
-- Dynamic Buffer Operations
--------------------------------------------------------------------------------

||| Create an empty dynamic buffer
public export
emptyDyn : (capacity : Nat) -> DynBuffer a
emptyDyn cap = MkDynBuffer cap []

||| Get current length
public export
dynLength : DynBuffer a -> Nat
dynLength buf = length buf.content

||| Check if buffer is empty
public export
isEmpty : DynBuffer a -> Bool
isEmpty buf = isNil buf.content

||| Check if buffer is full
public export
isFull : DynBuffer a -> Bool
isFull buf = length buf.content >= buf.capacity

||| Remaining capacity
public export
remaining : DynBuffer a -> Nat
remaining buf = minus buf.capacity (length buf.content)

||| Safe write to dynamic buffer
public export
write : a -> DynBuffer a -> Maybe (DynBuffer a)
write x buf =
  if isFull buf
    then Nothing
    else Just (MkDynBuffer buf.capacity (buf.content ++ [x]))

||| Safe write multiple elements
public export
writeMany : List a -> DynBuffer a -> Maybe (DynBuffer a)
writeMany xs buf =
  if length xs > remaining buf
    then Nothing
    else Just (MkDynBuffer buf.capacity (buf.content ++ xs))

||| Safe read at index
public export
readAt : (idx : Nat) -> DynBuffer a -> Maybe a
readAt idx buf = index' idx buf.content

||| Clear the buffer
public export
clear : DynBuffer a -> DynBuffer a
clear buf = MkDynBuffer buf.capacity []

||| Take first n elements
public export
takeBuf : (n : Nat) -> DynBuffer a -> List a
takeBuf n buf = take n buf.content

||| Drop first n elements
public export
dropBuf : (n : Nat) -> DynBuffer a -> DynBuffer a
dropBuf n buf = MkDynBuffer buf.capacity (drop n buf.content)

--------------------------------------------------------------------------------
-- Buffer Copying
--------------------------------------------------------------------------------

||| Copy contents from one dynamic buffer to another
public export
copyTo : DynBuffer a -> DynBuffer a -> Maybe (DynBuffer a)
copyTo src dest =
  if length src.content > remaining dest
    then Nothing
    else Just (MkDynBuffer dest.capacity (dest.content ++ src.content))

||| Slice a portion of the buffer
public export
slice : (start : Nat) -> (len : Nat) -> DynBuffer a -> List a
slice start len buf = take len (drop start buf.content)
