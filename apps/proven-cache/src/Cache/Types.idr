-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-cache key/value caching server.
||| Defines closed sum types for cache commands, eviction policies,
||| value data types, error codes, and replication modes.
module Cache.Types

%default total

---------------------------------------------------------------------------
-- Command: Operations the cache server accepts from clients.
---------------------------------------------------------------------------

||| Enumerates the commands that clients can issue to the cache server.
||| Modelled after common key/value store command vocabularies.
public export
data Command
  = Get     -- ^ Retrieve the value for a key
  | Set     -- ^ Store a key/value pair (optionally with TTL)
  | Delete  -- ^ Remove a key and its value
  | Exists  -- ^ Check whether a key exists
  | Expire  -- ^ Set or update the TTL on an existing key
  | TTL     -- ^ Query the remaining TTL of a key
  | Keys    -- ^ List keys matching a glob pattern
  | Flush   -- ^ Remove all keys from the cache
  | Incr    -- ^ Atomically increment an integer value
  | Decr    -- ^ Atomically decrement an integer value
  | Append  -- ^ Append bytes to a string value
  | Prepend -- ^ Prepend bytes to a string value
  | CAS     -- ^ Compare-and-swap: update only if the value has not changed

||| Display a human-readable label for each command.
public export
Show Command where
  show Get     = "Get"
  show Set     = "Set"
  show Delete  = "Delete"
  show Exists  = "Exists"
  show Expire  = "Expire"
  show TTL     = "TTL"
  show Keys    = "Keys"
  show Flush   = "Flush"
  show Incr    = "Incr"
  show Decr    = "Decr"
  show Append  = "Append"
  show Prepend = "Prepend"
  show CAS     = "CAS"

---------------------------------------------------------------------------
-- EvictionPolicy: Strategy for reclaiming memory when the cache is full.
---------------------------------------------------------------------------

||| Determines which entries are evicted when the cache reaches its
||| configured memory limit.
public export
data EvictionPolicy
  = LRU        -- ^ Evict least recently used entries first
  | LFU        -- ^ Evict least frequently used entries first
  | Random     -- ^ Evict entries at random
  | EvictTTL   -- ^ Evict entries closest to expiration first
  | NoEviction -- ^ Reject writes when memory is full (no eviction)

||| Display a human-readable label for each eviction policy.
public export
Show EvictionPolicy where
  show LRU        = "LRU"
  show LFU        = "LFU"
  show Random     = "Random"
  show EvictTTL   = "TTL"
  show NoEviction = "NoEviction"

---------------------------------------------------------------------------
-- DataType: The type tag for values stored in the cache.
---------------------------------------------------------------------------

||| Classifies the runtime type of a cached value for type-safe
||| operations (e.g., Incr only works on IntVal).
public export
data DataType
  = StringVal -- ^ UTF-8 string or raw byte sequence
  | IntVal    -- ^ 64-bit signed integer
  | ListVal   -- ^ Ordered list of values
  | SetVal    -- ^ Unordered set of unique values
  | HashVal   -- ^ Key/value map (nested hash)

||| Display a human-readable label for each data type.
public export
Show DataType where
  show StringVal = "String"
  show IntVal    = "Int"
  show ListVal   = "List"
  show SetVal    = "Set"
  show HashVal   = "Hash"

---------------------------------------------------------------------------
-- ErrorCode: Error responses from the cache server.
---------------------------------------------------------------------------

||| Error codes returned by the cache server when a command cannot
||| be fulfilled.
public export
data ErrorCode
  = NotFound      -- ^ The requested key does not exist
  | TypeMismatch  -- ^ Operation incompatible with the value's data type
  | OutOfMemory   -- ^ Cache memory limit reached and eviction policy is NoEviction
  | KeyTooLong    -- ^ Key exceeds the maximum allowed length
  | ValueTooLarge -- ^ Value exceeds the maximum allowed size
  | CASConflict   -- ^ Compare-and-swap failed: value was modified concurrently

||| Display a human-readable label for each error code.
public export
Show ErrorCode where
  show NotFound      = "NotFound"
  show TypeMismatch  = "TypeMismatch"
  show OutOfMemory   = "OutOfMemory"
  show KeyTooLong    = "KeyTooLong"
  show ValueTooLarge = "ValueTooLarge"
  show CASConflict   = "CASConflict"

---------------------------------------------------------------------------
-- ReplicationMode: The replication role of this cache instance.
---------------------------------------------------------------------------

||| Describes the replication topology role of the cache server instance.
public export
data ReplicationMode
  = RNone    -- ^ Standalone instance, no replication
  | Primary  -- ^ Primary (leader) that accepts writes and replicates to replicas
  | Replica  -- ^ Read-only replica that receives updates from the primary
  | Sentinel -- ^ Sentinel node that monitors primaries and orchestrates failover

||| Display a human-readable label for each replication mode.
public export
Show ReplicationMode where
  show RNone    = "None"
  show Primary  = "Primary"
  show Replica  = "Replica"
  show Sentinel = "Sentinel"
