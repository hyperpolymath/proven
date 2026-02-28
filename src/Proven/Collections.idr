-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| Proven.Collections - Convenience re-export module for verified data structure safety
|||
||| Groups all data structure Safe* modules into a single import for applications
||| that need formally verified containers. Covers ordered sets, min/max heaps,
||| bounded queues, directed/undirected graphs, balanced trees, probabilistic
||| Bloom filters, LRU caches, bounded buffers, compact bitsets, union-find
||| with path compression, dense matrices, and multi-dimensional tensors.
|||
||| Usage:
|||   import Proven.Collections
|||
||| This single import provides access to all verified data structure types,
||| constructors, operations, and invariant proofs without needing 12 separate
||| imports.
module Proven.Collections

import public Proven.SafeSet
import public Proven.SafeHeap
import public Proven.SafeQueue
import public Proven.SafeGraph
import public Proven.SafeTree
import public Proven.SafeBloom
import public Proven.SafeLRU
import public Proven.SafeBuffer
import public Proven.SafeBitset
import public Proven.SafeUnionFind
import public Proven.SafeMatrix
import public Proven.SafeTensor
