-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for the proven-cache key/value caching server.
||| Re-exports core types from Cache.Types and defines server constants.
module Cache

import public Cache.Types

%default total

---------------------------------------------------------------------------
-- Server Constants
---------------------------------------------------------------------------

||| Default listening port for cache client connections.
public export
cachePort : Nat
cachePort = 6379

||| Maximum key length in bytes.
public export
maxKeyLength : Nat
maxKeyLength = 512

||| Maximum value size in bytes (512 MiB).
public export
maxValueSize : Nat
maxValueSize = 536870912

||| Default TTL for new keys, in seconds. Zero means no expiration.
public export
defaultTTL : Nat
defaultTTL = 0
