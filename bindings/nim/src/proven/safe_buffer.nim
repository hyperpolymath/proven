# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe bounded buffer operations.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

type
  BoundedBuffer* = object
    ## A bounded byte buffer backed by libproven's verified implementation.
    ## Uses an opaque pointer to the C-side ProvenBoundedBuffer.
    handle: ptr ProvenBoundedBuffer

  BufferError* = object of CatchableError
    ## Error raised when buffer operations fail.

proc `=destroy`*(buf: BoundedBuffer) =
  ## Destructor: automatically frees the underlying C buffer.
  if buf.handle != nil:
    provenBufferFree(buf.handle)

proc `=copy`*(dest: var BoundedBuffer, src: BoundedBuffer) {.error:
  "BoundedBuffer cannot be copied; it owns an opaque C resource".}

proc `=sink`*(dest: var BoundedBuffer, src: BoundedBuffer) =
  ## Move semantics for BoundedBuffer.
  `=destroy`(dest)
  dest.handle = src.handle

proc newBoundedBuffer*(capacity: int): BoundedBuffer =
  ## Create a new bounded buffer with the given capacity in bytes.
  ## Maximum capacity is 100MB as enforced by libproven.
  let res = provenBufferCreate(csize_t(capacity))
  if res.status != PROVEN_OK or res.buffer == nil:
    raise newException(BufferError,
      "Failed to create bounded buffer (status=" & $res.status & ")")
  BoundedBuffer(handle: res.buffer)

proc append*(buf: BoundedBuffer, data: openArray[byte]): bool =
  ## Append bytes to the buffer.  Returns true on success, false if
  ## appending would exceed the buffer capacity.
  if buf.handle == nil or data.len == 0:
    return buf.handle != nil
  let status = provenBufferAppend(buf.handle, unsafeAddr data[0], csize_t(data.len))
  status == PROVEN_OK

proc append*(buf: BoundedBuffer, data: string): bool =
  ## Append a string's bytes to the buffer.  Returns true on success,
  ## false if appending would exceed the buffer capacity.
  if buf.handle == nil:
    return false
  if data.len == 0:
    return true
  let status = provenBufferAppend(buf.handle, unsafeAddr data[0], csize_t(data.len))
  status == PROVEN_OK

proc getData*(buf: BoundedBuffer): Option[seq[byte]] =
  ## Get a copy of the buffer contents as a byte sequence.
  ## Returns None if the buffer handle is invalid.
  if buf.handle == nil:
    return none(seq[byte])
  var outPtr: pointer
  var outLen: csize_t
  let status = provenBufferGet(buf.handle, addr outPtr, addr outLen)
  if status != PROVEN_OK:
    return none(seq[byte])
  if outLen == 0:
    return some(newSeq[byte](0))
  var resultBytes = newSeq[byte](int(outLen))
  copyMem(addr resultBytes[0], outPtr, int(outLen))
  some(resultBytes)

proc getDataAsString*(buf: BoundedBuffer): Option[string] =
  ## Get a copy of the buffer contents as a string.
  ## Returns None if the buffer handle is invalid.
  if buf.handle == nil:
    return none(string)
  var outPtr: pointer
  var outLen: csize_t
  let status = provenBufferGet(buf.handle, addr outPtr, addr outLen)
  if status != PROVEN_OK:
    return none(string)
  if outLen == 0:
    return some("")
  var resultStr = newString(int(outLen))
  copyMem(addr resultStr[0], outPtr, int(outLen))
  some(resultStr)

proc len*(buf: BoundedBuffer): int =
  ## Get current data length in the buffer.
  if buf.handle != nil:
    int(buf.handle[].length)
  else:
    0

proc capacity*(buf: BoundedBuffer): int =
  ## Get the total capacity of the buffer.
  if buf.handle != nil:
    int(buf.handle[].capacity)
  else:
    0

proc remaining*(buf: BoundedBuffer): int =
  ## Get remaining capacity in bytes.
  if buf.handle != nil:
    int(buf.handle[].capacity) - int(buf.handle[].length)
  else:
    0

proc isEmpty*(buf: BoundedBuffer): bool =
  ## Check if buffer is empty.
  buf.len == 0

proc isFull*(buf: BoundedBuffer): bool =
  ## Check if buffer is full.
  buf.remaining == 0

proc isValid*(buf: BoundedBuffer): bool =
  ## Check if the buffer handle is valid (non-nil).
  buf.handle != nil
