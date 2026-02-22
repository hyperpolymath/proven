# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe Bloom filter operations.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

type
  BloomFilter* = object
    ## Bloom filter backed by libproven's verified implementation.
    ## Uses an opaque pointer to the C-side ProvenBloomFilter.
    handle: ptr ProvenBloomFilter

  BloomFilterError* = object of CatchableError
    ## Error raised when Bloom filter creation fails.

proc `=destroy`*(bf: BloomFilter) =
  ## Destructor: automatically frees the underlying C Bloom filter.
  if bf.handle != nil:
    provenBloomFree(bf.handle)

proc `=copy`*(dest: var BloomFilter, src: BloomFilter) {.error:
  "BloomFilter cannot be copied; it owns an opaque C resource".}

proc `=sink`*(dest: var BloomFilter, src: BloomFilter) =
  ## Move semantics for BloomFilter.
  `=destroy`(dest)
  dest.handle = src.handle

proc newBloomFilter*(expectedItems: int,
                     falsePositiveRate: float64 = 0.01): BloomFilter =
  ## Create a new Bloom filter sized for the expected number of items and
  ## desired false-positive rate.  Delegates to proven_bloom_create.
  let handle = provenBloomCreate(csize_t(expectedItems), falsePositiveRate)
  if handle == nil:
    raise newException(BloomFilterError,
      "Failed to create Bloom filter (allocation or invalid parameters)")
  BloomFilter(handle: handle)

proc insert*(bf: BloomFilter, item: string) =
  ## Insert a string item into the filter.
  if bf.handle != nil and item.len > 0:
    provenBloomAdd(bf.handle, unsafeAddr item[0], csize_t(item.len))
  elif bf.handle != nil:
    # Empty string: pass a valid but zero-length pointer
    var dummy: uint8
    provenBloomAdd(bf.handle, addr dummy, csize_t(0))

proc insert*[T](bf: BloomFilter, item: T) =
  ## Insert any item into the filter by converting it to a string first.
  bf.insert($item)

proc contains*(bf: BloomFilter, item: string): bool =
  ## Check if an item might be in the filter.
  ## Returns false if definitely not present, true if possibly present
  ## (probabilistic data structure -- false positives are possible).
  if bf.handle == nil:
    return false
  if item.len > 0:
    provenBloomContains(bf.handle, unsafeAddr item[0], csize_t(item.len))
  else:
    var dummy: uint8
    provenBloomContains(bf.handle, addr dummy, csize_t(0))

proc contains*[T](bf: BloomFilter, item: T): bool =
  ## Check if any item might be in the filter (converts to string).
  bf.contains($item)

proc getHashCount*(bf: BloomFilter): uint32 =
  ## Get the number of hash functions used by the filter.
  if bf.handle != nil:
    bf.handle[].hash_count
  else:
    0

proc getBitCount*(bf: BloomFilter): int =
  ## Get the number of bits in the filter.
  if bf.handle != nil:
    int(bf.handle[].bit_count)
  else:
    0

proc isValid*(bf: BloomFilter): bool =
  ## Check if the Bloom filter handle is valid (non-nil).
  bf.handle != nil
