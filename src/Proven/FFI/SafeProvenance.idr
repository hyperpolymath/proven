-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeProvenance operations
|||
||| This module exports provenance tracking and audit trail utilities to the C ABI
||| via Idris2's RefC backend. All functions are proven total and ensure tamper detection.
|||
||| Return conventions:
||| - Change type → Int (0=Create, 1=Update, 2=Delete, 3=Derive, 4=Transform, 5=Annotate, 6=Link)
||| - Hash → Int (simplified hash value)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
|||
||| CRITICAL: Hash chains provide tamper detection. Verify chain integrity before trusting data.
|||
||| NOTE: Full provenance structures not exposed. This module provides helpers for
||| building and validating provenance chains in Zig.
module Proven.FFI.SafeProvenance

import Proven.SafeProvenance
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

||| Encode ChangeType as Int
encodeChangeType : ChangeType -> Int
encodeChangeType Create = 0
encodeChangeType Update = 1
encodeChangeType Delete = 2
encodeChangeType Derive = 3
encodeChangeType Transform = 4
encodeChangeType Annotate = 5
encodeChangeType Link = 6

||| Decode Int to ChangeType
decodeChangeType : Int -> Maybe ChangeType
decodeChangeType 0 = Just Create
decodeChangeType 1 = Just Update
decodeChangeType 2 = Just Delete
decodeChangeType 3 = Just Derive
decodeChangeType 4 = Just Transform
decodeChangeType 5 = Just Annotate
decodeChangeType 6 = Just Link
decodeChangeType _ = Nothing

--------------------------------------------------------------------------------
-- Change Type Operations
--------------------------------------------------------------------------------

%export
proven_idris_prov_encode_change_type : String -> Int
proven_idris_prov_encode_change_type s =
  case toLower s of
    "create" => 0
    "update" => 1
    "delete" => 2
    "derive" => 3
    "transform" => 4
    "annotate" => 5
    "link" => 6
    _ => (-1)

%export
proven_idris_prov_decode_change_type : Int -> String
proven_idris_prov_decode_change_type typeCode =
  case decodeChangeType typeCode of
    Nothing => "unknown"
    Just Create => "create"
    Just Update => "update"
    Just Delete => "delete"
    Just Derive => "derive"
    Just Transform => "transform"
    Just Annotate => "annotate"
    Just Link => "link"

%export
proven_idris_prov_is_valid_change_type : Int -> Int
proven_idris_prov_is_valid_change_type typeCode =
  encodeBool (isJust (decodeChangeType typeCode))

%export
proven_idris_prov_is_mutation : Int -> Int
proven_idris_prov_is_mutation typeCode =
  case decodeChangeType typeCode of
    Just Create => 1
    Just Update => 1
    Just Delete => 1
    _ => 0

%export
proven_idris_prov_is_derivation : Int -> Int
proven_idris_prov_is_derivation typeCode =
  case decodeChangeType typeCode of
    Just Derive => 1
    Just Transform => 1
    _ => 0

--------------------------------------------------------------------------------
-- Hash Operations
--------------------------------------------------------------------------------

%export
proven_idris_prov_hash_event : Int -> Int -> Int -> Int
proven_idris_prov_hash_event eventId timestamp prevHash =
  cast (cast eventId + cast timestamp + cast prevHash * 31)

%export
proven_idris_prov_hash_audit_entry : Int -> Int -> Int -> Int
proven_idris_prov_hash_audit_entry auditId timestamp prevHash =
  cast (cast auditId + cast timestamp + cast prevHash * 37)

%export
proven_idris_prov_verify_hash : Int -> Int -> Int -> Int -> Int
proven_idris_prov_verify_hash eventId timestamp prevHash expectedHash =
  let computed = cast (cast eventId + cast timestamp + cast prevHash * 31)
  in encodeBool (computed == cast expectedHash)

--------------------------------------------------------------------------------
-- Chain Validation
--------------------------------------------------------------------------------

%export
proven_idris_prov_is_genesis_hash : Int -> Int
proven_idris_prov_is_genesis_hash hash =
  encodeBool (hash == 0)

%export
proven_idris_prov_hashes_match : Int -> Int -> Int
proven_idris_prov_hashes_match hash1 hash2 =
  encodeBool (hash1 == hash2)

--------------------------------------------------------------------------------
-- Entity Identifier Validation
--------------------------------------------------------------------------------

%export
proven_idris_prov_is_valid_entity_id : String -> Int
proven_idris_prov_is_valid_entity_id entityId =
  encodeBool (not (null entityId))

%export
proven_idris_prov_is_valid_actor_id : String -> Int
proven_idris_prov_is_valid_actor_id actorId =
  encodeBool (not (null actorId))

%export
proven_idris_prov_entity_ids_equal : String -> String -> Int
proven_idris_prov_entity_ids_equal e1 e2 =
  encodeBool (e1 == e2)

--------------------------------------------------------------------------------
-- Lineage Operations
--------------------------------------------------------------------------------

||| Parse source list from comma-separated string
parseSources : String -> List String
parseSources s = if s == "" then [] else split (== ',') s

%export
proven_idris_prov_source_count : String -> Int
proven_idris_prov_source_count sources =
  cast (length (parseSources sources))

%export
proven_idris_prov_has_source : String -> String -> Int
proven_idris_prov_has_source sources targetSource =
  let sourceList = parseSources sources
  in encodeBool (elem targetSource sourceList)

%export
proven_idris_prov_has_any_source : String -> Int
proven_idris_prov_has_any_source sources =
  encodeBool (not (null sources))

%export
proven_idris_prov_add_source : String -> String -> String
proven_idris_prov_add_source sources newSource =
  if null sources
    then newSource
    else sources ++ "," ++ newSource

--------------------------------------------------------------------------------
-- Transform Operations
--------------------------------------------------------------------------------

||| Parse transform list from comma-separated string
parseTransforms : String -> List String
parseTransforms s = if s == "" then [] else split (== ',') s

%export
proven_idris_prov_transform_count : String -> Int
proven_idris_prov_transform_count transforms =
  cast (length (parseTransforms transforms))

%export
proven_idris_prov_has_transform : String -> String -> Int
proven_idris_prov_has_transform transforms targetTransform =
  let transformList = parseTransforms transforms
  in encodeBool (elem targetTransform transformList)

%export
proven_idris_prov_add_transform : String -> String -> String
proven_idris_prov_add_transform transforms newTransform =
  if null transforms
    then newTransform
    else transforms ++ "," ++ newTransform

--------------------------------------------------------------------------------
-- Audit Trail Operations
--------------------------------------------------------------------------------

%export
proven_idris_prov_is_sealed : Int -> Int
proven_idris_prov_is_sealed sealed = encodeBool (sealed /= 0)

%export
proven_idris_prov_can_add_entry : Int -> Int
proven_idris_prov_can_add_entry sealed = encodeBool (sealed == 0)

--------------------------------------------------------------------------------
-- Timestamp Operations
--------------------------------------------------------------------------------

%export
proven_idris_prov_is_valid_timestamp : Int -> Int
proven_idris_prov_is_valid_timestamp timestamp =
  encodeBool (timestamp >= 0)

%export
proven_idris_prov_timestamps_ordered : Int -> Int -> Int
proven_idris_prov_timestamps_ordered earlier later =
  encodeBool (earlier <= later)

%export
proven_idris_prov_timestamp_diff : Int -> Int -> Int
proven_idris_prov_timestamp_diff later earlier =
  if later >= earlier
    then cast (cast later - cast earlier)
    else 0

--------------------------------------------------------------------------------
-- Version Tracking
--------------------------------------------------------------------------------

%export
proven_idris_prov_is_initial_version : Int -> Int
proven_idris_prov_is_initial_version version =
  encodeBool (version == 0)

%export
proven_idris_prov_next_version : Int -> Int
proven_idris_prov_next_version current = current + 1

%export
proven_idris_prov_versions_consecutive : Int -> Int -> Int
proven_idris_prov_versions_consecutive prev next =
  encodeBool (next == prev + 1)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_prov_friendly_error : String -> String
proven_idris_prov_friendly_error errorMsg =
  if isInfixOf "tamper" (toLower errorMsg) || isInfixOf "hash" (toLower errorMsg)
    then "Provenance chain integrity violation (possible tampering)"
  else if isInfixOf "sealed" (toLower errorMsg)
    then "Audit trail is sealed (cannot add more entries)"
  else if isInfixOf "timestamp" (toLower errorMsg)
    then "Invalid timestamp (must be monotonically increasing)"
  else if isInfixOf "version" (toLower errorMsg)
    then "Invalid version number"
  else if isInfixOf "entity" (toLower errorMsg)
    then "Invalid entity identifier"
  else
    "Provenance tracking error"
