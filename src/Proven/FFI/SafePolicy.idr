-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafePolicy operations
|||
||| This module exports policy enforcement utilities to the C ABI via Idris2's RefC backend.
||| All functions are proven total and enforce access control policies.
|||
||| Return conventions:
||| - Zone class → Int (0=Mutable, 1=Immutable, 2=Hybrid, 3=Restricted)
||| - Policy action → Int (0=Allow, 1=Deny, 2=Audit, 3=Transform, 4=Delegate)
||| - Condition evaluation → Int (0 = false, 1 = true)
|||
||| CRITICAL: Policies are evaluated in priority order. Higher priority rules matched first.
|||
||| NOTE: Full Policy/Condition data structures not exposed due to complexity.
||| This module provides encoding/decoding helpers and simple evaluators.
module Proven.FFI.SafePolicy

import Proven.SafePolicy
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

||| Encode ZoneClass as Int
encodeZoneClass : ZoneClass -> Int
encodeZoneClass Mutable = 0
encodeZoneClass Immutable = 1
encodeZoneClass Hybrid = 2
encodeZoneClass Restricted = 3

||| Decode Int to ZoneClass
decodeZoneClass : Int -> Maybe ZoneClass
decodeZoneClass 0 = Just Mutable
decodeZoneClass 1 = Just Immutable
decodeZoneClass 2 = Just Hybrid
decodeZoneClass 3 = Just Restricted
decodeZoneClass _ = Nothing

||| Encode PolicyAction as Int
encodePolicyAction : PolicyAction -> Int
encodePolicyAction Allow = 0
encodePolicyAction Deny = 1
encodePolicyAction Audit = 2
encodePolicyAction Transform = 3
encodePolicyAction Delegate = 4

||| Decode Int to PolicyAction
decodePolicyAction : Int -> Maybe PolicyAction
decodePolicyAction 0 = Just Allow
decodePolicyAction 1 = Just Deny
decodePolicyAction 2 = Just Audit
decodePolicyAction 3 = Just Transform
decodePolicyAction 4 = Just Delegate
decodePolicyAction _ = Nothing

--------------------------------------------------------------------------------
-- Zone Class Operations
--------------------------------------------------------------------------------

%export
proven_idris_policy_encode_zone_class : String -> Int
proven_idris_policy_encode_zone_class s =
  case toLower s of
    "mutable" => 0
    "immutable" => 1
    "hybrid" => 2
    "restricted" => 3
    _ => (-1)

%export
proven_idris_policy_decode_zone_class : Int -> String
proven_idris_policy_decode_zone_class classCode =
  case decodeZoneClass classCode of
    Nothing => "unknown"
    Just Mutable => "mutable"
    Just Immutable => "immutable"
    Just Hybrid => "hybrid"
    Just Restricted => "restricted"

%export
proven_idris_policy_is_valid_zone_class : Int -> Int
proven_idris_policy_is_valid_zone_class classCode =
  encodeBool (isJust (decodeZoneClass classCode))

--------------------------------------------------------------------------------
-- Policy Action Operations
--------------------------------------------------------------------------------

%export
proven_idris_policy_encode_action : String -> Int
proven_idris_policy_encode_action s =
  case toLower s of
    "allow" => 0
    "deny" => 1
    "audit" => 2
    "transform" => 3
    "delegate" => 4
    _ => (-1)

%export
proven_idris_policy_decode_action : Int -> String
proven_idris_policy_decode_action actionCode =
  case decodePolicyAction actionCode of
    Nothing => "unknown"
    Just Allow => "allow"
    Just Deny => "deny"
    Just Audit => "audit"
    Just Transform => "transform"
    Just Delegate => "delegate"

%export
proven_idris_policy_is_valid_action : Int -> Int
proven_idris_policy_is_valid_action actionCode =
  encodeBool (isJust (decodePolicyAction actionCode))

%export
proven_idris_policy_is_allow : Int -> Int
proven_idris_policy_is_allow actionCode =
  case decodePolicyAction actionCode of
    Just Allow => 1
    _ => 0

%export
proven_idris_policy_is_deny : Int -> Int
proven_idris_policy_is_deny actionCode =
  case decodePolicyAction actionCode of
    Just Deny => 1
    _ => 0

--------------------------------------------------------------------------------
-- Simple Condition Evaluation
--------------------------------------------------------------------------------

||| Parse tag list from comma-separated string
parseTags : String -> List String
parseTags s = if s == "" then [] else split (== ',') s

%export
proven_idris_policy_eval_always : Int
proven_idris_policy_eval_always = 1

%export
proven_idris_policy_eval_never : Int
proven_idris_policy_eval_never = 0

%export
proven_idris_policy_eval_in_zone : String -> String -> Int
proven_idris_policy_eval_in_zone targetZone currentZone =
  encodeBool (targetZone == currentZone)

%export
proven_idris_policy_eval_has_tag : String -> String -> Int
proven_idris_policy_eval_has_tag tag tagList =
  let tags = parseTags tagList
  in encodeBool (elem tag tags)

%export
proven_idris_policy_eval_and : Int -> Int -> Int
proven_idris_policy_eval_and c1 c2 =
  encodeBool (c1 /= 0 && c2 /= 0)

%export
proven_idris_policy_eval_or : Int -> Int -> Int
proven_idris_policy_eval_or c1 c2 =
  encodeBool (c1 /= 0 || c2 /= 0)

%export
proven_idris_policy_eval_not : Int -> Int
proven_idris_policy_eval_not c =
  encodeBool (c == 0)

--------------------------------------------------------------------------------
-- Rule Priority
--------------------------------------------------------------------------------

%export
proven_idris_policy_compare_priority : Int -> Int -> Int
proven_idris_policy_compare_priority p1 p2 =
  if p1 > p2 then 1
  else if p1 < p2 then (-1)
  else 0

%export
proven_idris_policy_is_higher_priority : Int -> Int -> Int
proven_idris_policy_is_higher_priority p1 p2 =
  encodeBool (p1 > p2)

--------------------------------------------------------------------------------
-- Conflict Detection Helpers
--------------------------------------------------------------------------------

%export
proven_idris_policy_actions_conflict : Int -> Int -> Int
proven_idris_policy_actions_conflict action1 action2 =
  encodeBool (action1 /= action2)

%export
proven_idris_policy_same_priority : Int -> Int -> Int
proven_idris_policy_same_priority p1 p2 =
  encodeBool (p1 == p2)

%export
proven_idris_policy_would_conflict : Int -> Int -> Int -> Int -> Int
proven_idris_policy_would_conflict action1 prio1 action2 prio2 =
  encodeBool (action1 /= action2 && prio1 == prio2)

--------------------------------------------------------------------------------
-- Zone Hierarchy Helpers
--------------------------------------------------------------------------------

%export
proven_idris_policy_is_valid_zone_id : String -> Int
proven_idris_policy_is_valid_zone_id zoneId =
  encodeBool (not (null zoneId))

%export
proven_idris_policy_zone_ids_equal : String -> String -> Int
proven_idris_policy_zone_ids_equal z1 z2 =
  encodeBool (z1 == z2)

--------------------------------------------------------------------------------
-- Tag List Operations
--------------------------------------------------------------------------------

%export
proven_idris_policy_tag_count : String -> Int
proven_idris_policy_tag_count tagList =
  cast (length (parseTags tagList))

%export
proven_idris_policy_has_any_tag : String -> String -> Int
proven_idris_policy_has_any_tag requiredTags actualTags =
  let required = parseTags requiredTags
      actual = parseTags actualTags
  in encodeBool (any (\t => elem t actual) required)

%export
proven_idris_policy_has_all_tags : String -> String -> Int
proven_idris_policy_has_all_tags requiredTags actualTags =
  let required = parseTags requiredTags
      actual = parseTags actualTags
  in encodeBool (all (\t => elem t actual) required)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_policy_friendly_error : String -> String
proven_idris_policy_friendly_error errorMsg =
  if isInfixOf "deny" (toLower errorMsg) || isInfixOf "denied" (toLower errorMsg)
    then "Access denied by policy"
  else if isInfixOf "conflict" (toLower errorMsg)
    then "Policy rules conflict (same priority, different actions)"
  else if isInfixOf "zone" (toLower errorMsg)
    then "Invalid zone identifier or classification"
  else if isInfixOf "priority" (toLower errorMsg)
    then "Invalid rule priority"
  else
    "Policy evaluation error"
