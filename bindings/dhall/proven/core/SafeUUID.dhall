-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

{-
SafeUUID - Safe UUID handling

Provides UUID types and validation markers.
UUIDs are typed by version for correct usage patterns.

Part of the Proven safety primitives library.
-}

let Prelude = https://prelude.dhall-lang.org/v23.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

-- UUID version types
let UUIDVersion = < V1 | V3 | V4 | V5 | V6 | V7 | V8 | Nil >

-- UUID type (36 character string)
let UUID = {
    uuid : Text,
    version : UUIDVersion
}

-- UUID result
let UUIDResult = { value : UUID, ok : Bool }

-- Create success result
let ok
    : UUID -> UUIDResult
    = \(u : UUID) -> { value = u, ok = True }

-- Create error result
let err
    : UUIDResult
    = { value = { uuid = "", version = UUIDVersion.Nil }, ok = False }

-- Create UUID v4 marker (random)
let mkUUIDv4
    : Text -> UUID
    = \(s : Text) ->
        { uuid = s, version = UUIDVersion.V4 }

-- Create UUID v7 marker (time-sorted)
let mkUUIDv7
    : Text -> UUID
    = \(s : Text) ->
        { uuid = s, version = UUIDVersion.V7 }

-- Create UUID v1 marker (time-based, MAC address)
let mkUUIDv1
    : Text -> UUID
    = \(s : Text) ->
        { uuid = s, version = UUIDVersion.V1 }

-- Create UUID v3 marker (MD5 namespace)
let mkUUIDv3
    : Text -> UUID
    = \(s : Text) ->
        { uuid = s, version = UUIDVersion.V3 }

-- Create UUID v5 marker (SHA-1 namespace)
let mkUUIDv5
    : Text -> UUID
    = \(s : Text) ->
        { uuid = s, version = UUIDVersion.V5 }

-- Nil UUID (all zeros)
let nilUUID
    : UUID
    = { uuid = "00000000-0000-0000-0000-000000000000", version = UUIDVersion.Nil }

-- Check if UUID is nil
let isNil
    : UUID -> Bool
    = \(u : UUID) ->
        u.uuid == "00000000-0000-0000-0000-000000000000"

-- Standard namespace UUIDs (RFC 4122)
let Namespaces = {
    -- DNS namespace
    dns = { uuid = "6ba7b810-9dad-11d1-80b4-00c04fd430c8", version = UUIDVersion.V1 },
    -- URL namespace
    url = { uuid = "6ba7b811-9dad-11d1-80b4-00c04fd430c8", version = UUIDVersion.V1 },
    -- OID namespace
    oid = { uuid = "6ba7b812-9dad-11d1-80b4-00c04fd430c8", version = UUIDVersion.V1 },
    -- X.500 DN namespace
    x500 = { uuid = "6ba7b814-9dad-11d1-80b4-00c04fd430c8", version = UUIDVersion.V1 }
}

-- UUID without hyphens (32 chars)
let CompactUUID = { uuid : Text }

-- Create compact UUID
let mkCompactUUID
    : UUID -> CompactUUID
    = \(u : UUID) ->
        -- In Dhall we cannot actually remove hyphens
        { uuid = u.uuid }

-- Typed UUID (for entity IDs)
let TypedUUID = \(T : Type) -> { uuid : UUID, _phantom : {} }

-- Create typed UUID
let mkTypedUUID
    : forall (T : Type) -> UUID -> TypedUUID T
    = \(T : Type) -> \(u : UUID) ->
        { uuid = u, _phantom = {=} }

-- Get raw UUID from typed
let untypeUUID
    : forall (T : Type) -> TypedUUID T -> UUID
    = \(T : Type) -> \(t : TypedUUID T) ->
        t.uuid

-- UUID reference (for foreign keys)
let UUIDRef = \(T : Type) -> { ref : UUID }

-- Create UUID reference
let mkUUIDRef
    : forall (T : Type) -> UUID -> UUIDRef T
    = \(T : Type) -> \(u : UUID) ->
        { ref = u }

-- UUID with timestamp (for V1/V6/V7)
let TimestampedUUID = {
    uuid : UUID,
    timestamp : Optional Natural
}

-- Create timestamped UUID
let mkTimestampedUUID
    : UUID -> Optional Natural -> TimestampedUUID
    = \(u : UUID) -> \(ts : Optional Natural) ->
        { uuid = u, timestamp = ts }

-- UUID generation config
let UUIDConfig = {
    version : UUIDVersion,
    namespace : Optional UUID,
    name : Optional Text
}

-- Config for random UUIDs (v4)
let randomUUIDConfig
    : UUIDConfig
    = {
        version = UUIDVersion.V4,
        namespace = None UUID,
        name = None Text
    }

-- Config for time-sorted UUIDs (v7)
let timeSortedUUIDConfig
    : UUIDConfig
    = {
        version = UUIDVersion.V7,
        namespace = None UUID,
        name = None Text
    }

-- Config for named UUIDs (v5)
let namedUUIDConfig
    : UUID -> Text -> UUIDConfig
    = \(ns : UUID) -> \(name : Text) -> {
        version = UUIDVersion.V5,
        namespace = Some ns,
        name = Some name
    }

-- ULID type (Universally Unique Lexicographically Sortable Identifier)
let ULID = { ulid : Text }

-- Create ULID marker
let mkULID
    : Text -> ULID
    = \(s : Text) ->
        { ulid = s }

-- KSUID type (K-Sortable Unique Identifier)
let KSUID = { ksuid : Text }

-- Create KSUID marker
let mkKSUID
    : Text -> KSUID
    = \(s : Text) ->
        { ksuid = s }

-- Snowflake ID (64-bit time-sorted)
let SnowflakeID = { id : Natural }

-- Create Snowflake ID marker
let mkSnowflakeID
    : Natural -> SnowflakeID
    = \(n : Natural) ->
        { id = n }

in {
    -- Types
    UUIDVersion,
    UUID,
    UUIDResult,
    CompactUUID,
    TypedUUID,
    UUIDRef,
    TimestampedUUID,
    UUIDConfig,
    ULID,
    KSUID,
    SnowflakeID,

    -- Constructors
    ok,
    err,
    mkUUIDv1,
    mkUUIDv3,
    mkUUIDv4,
    mkUUIDv5,
    mkUUIDv7,
    mkCompactUUID,
    mkTypedUUID,
    mkUUIDRef,
    mkTimestampedUUID,
    mkULID,
    mkKSUID,
    mkSnowflakeID,

    -- Utilities
    untypeUUID,
    isNil,

    -- Constants
    nilUUID,
    Namespaces,

    -- Configs
    randomUUIDConfig,
    timeSortedUUIDConfig,
    namedUUIDConfig
}
