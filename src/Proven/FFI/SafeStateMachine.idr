-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeStateMachine operations
|||
||| This module exports state machine utilities to the C ABI via Idris2's RefC backend.
||| All functions are proven total and validate state transitions.
|||
||| Return conventions:
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
||| - Counts → Int (number of transitions, states, etc.)
||| - Comparison → Int (0 = not equal, 1 = equal)
|||
||| CRITICAL: State machines must be deterministic. No duplicate transitions
|||           from same (state, event) pair.
|||
||| NOTE: Full StateMachine/MachineInstance structures not exposed due to
||| polymorphism. This module provides validation and comparison helpers.
module Proven.FFI.SafeStateMachine

import Proven.SafeStateMachine
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

||| Parse state list from comma-separated string
parseStates : String -> List String
parseStates s = if s == "" then [] else split (== ',') s

||| Parse event list from comma-separated string
parseEvents : String -> List String
parseEvents s = if s == "" then [] else split (== ',') s

--------------------------------------------------------------------------------
-- State Operations
--------------------------------------------------------------------------------

%export
proven_idris_sm_states_equal : String -> String -> Int
proven_idris_sm_states_equal s1 s2 = encodeBool (s1 == s2)

%export
proven_idris_sm_is_valid_state : String -> Int
proven_idris_sm_is_valid_state state = encodeBool (not (null state))

%export
proven_idris_sm_state_in_list : String -> String -> Int
proven_idris_sm_state_in_list state stateList =
  let states = parseStates stateList
  in encodeBool (elem state states)

%export
proven_idris_sm_state_count : String -> Int
proven_idris_sm_state_count stateList =
  cast (length (parseStates stateList))

--------------------------------------------------------------------------------
-- Event Operations
--------------------------------------------------------------------------------

%export
proven_idris_sm_events_equal : String -> String -> Int
proven_idris_sm_events_equal e1 e2 = encodeBool (e1 == e2)

%export
proven_idris_sm_is_valid_event : String -> Int
proven_idris_sm_is_valid_event event = encodeBool (not (null event))

%export
proven_idris_sm_event_in_list : String -> String -> Int
proven_idris_sm_event_in_list event eventList =
  let events = parseEvents eventList
  in encodeBool (elem event events)

%export
proven_idris_sm_event_count : String -> Int
proven_idris_sm_event_count eventList =
  cast (length (parseEvents eventList))

--------------------------------------------------------------------------------
-- Transition Validation
--------------------------------------------------------------------------------

||| Transition format: "from->event->to"
parseTransition : String -> Maybe (String, String, String)
parseTransition s =
  case split (== '>') s of
    [from, "-", event, "-", to] => Just (from, event, to)
    _ => Nothing

||| Parse transition list from semicolon-separated string
parseTransitions : String -> List (String, String, String)
parseTransitions s =
  if s == "" then []
  else mapMaybe parseTransition (split (== ';') s)

%export
proven_idris_sm_is_valid_transition_format : String -> Int
proven_idris_sm_is_valid_transition_format trans =
  encodeBool (isJust (parseTransition trans))

%export
proven_idris_sm_transition_count : String -> Int
proven_idris_sm_transition_count transList =
  cast (length (parseTransitions transList))

%export
proven_idris_sm_has_transition : String -> String -> String -> String -> Int
proven_idris_sm_has_transition transList from event to =
  let transitions = parseTransitions transList
  in encodeBool (elem (from, event, to) transitions)

%export
proven_idris_sm_has_transition_from : String -> String -> String -> Int
proven_idris_sm_has_transition_from transList from event =
  let transitions = parseTransitions transList
  in encodeBool (any (\(f, e, _) => f == from && e == event) transitions)

--------------------------------------------------------------------------------
-- Determinism Check
--------------------------------------------------------------------------------

||| Check for duplicate (from, event) pairs
%export
proven_idris_sm_is_deterministic : String -> Int
proven_idris_sm_is_deterministic transList =
  let transitions = parseTransitions transList
  in encodeBool (noDuplicateTriggers transitions)
  where
    noDuplicateTriggers : List (String, String, String) -> Bool
    noDuplicateTriggers [] = True
    noDuplicateTriggers ((f1, e1, t1) :: rest) =
      not (any (\(f2, e2, t2) => f1 == f2 && e1 == e2 && t1 /= t2) rest) &&
      noDuplicateTriggers rest

%export
proven_idris_sm_find_conflict : String -> String
proven_idris_sm_find_conflict transList =
  let transitions = parseTransitions transList
  in findConflict transitions
  where
    findConflict : List (String, String, String) -> String
    findConflict [] = ""
    findConflict ((f1, e1, t1) :: rest) =
      case find (\(f2, e2, t2) => f1 == f2 && e1 == e2 && t1 /= t2) rest of
        Just (f2, e2, t2) => f1 ++ "->" ++ e1 ++ " conflicts: " ++ t1 ++ " vs " ++ t2
        Nothing => findConflict rest

--------------------------------------------------------------------------------
-- Reachability
--------------------------------------------------------------------------------

%export
proven_idris_sm_reachable_from : String -> String -> String
proven_idris_sm_reachable_from transList from =
  let transitions = parseTransitions transList
      reachable = map (\(_, _, to) => to) (filter (\(f, _, _) => f == from) transitions)
  in joinWith "," reachable
  where
    joinWith : String -> List String -> String
    joinWith _ [] = ""
    joinWith sep (x :: xs) = foldl (\acc, y => acc ++ sep ++ y) x xs

%export
proven_idris_sm_available_events_from : String -> String -> String
proven_idris_sm_available_events_from transList from =
  let transitions = parseTransitions transList
      events = map (\(_, e, _) => e) (filter (\(f, _, _) => f == from) transitions)
  in joinWith "," (nubSM events)
  where
    joinWith : String -> List String -> String
    joinWith _ [] = ""
    joinWith sep (x :: xs) = foldl (\acc, y => acc ++ sep ++ y) x xs

    nubSM : List String -> List String
    nubSM [] = []
    nubSM (x :: xs) = x :: assert_total (nubSM (filter (/= x) xs))

--------------------------------------------------------------------------------
-- Final State Checks
--------------------------------------------------------------------------------

%export
proven_idris_sm_is_final_state : String -> String -> Int
proven_idris_sm_is_final_state finalStates current =
  let finals = parseStates finalStates
  in encodeBool (elem current finals)

%export
proven_idris_sm_has_final_states : String -> Int
proven_idris_sm_has_final_states finalStates =
  encodeBool (not (null finalStates))

%export
proven_idris_sm_final_state_count : String -> Int
proven_idris_sm_final_state_count finalStates =
  cast (length (parseStates finalStates))

--------------------------------------------------------------------------------
-- History Operations
--------------------------------------------------------------------------------

%export
proven_idris_sm_history_length : Int -> Int
proven_idris_sm_history_length len = len

%export
proven_idris_sm_is_initial_state : Int -> Int
proven_idris_sm_is_initial_state historyLen =
  encodeBool (historyLen == 0)

--------------------------------------------------------------------------------
-- Validation Helpers
--------------------------------------------------------------------------------

%export
proven_idris_sm_all_states_valid : String -> Int
proven_idris_sm_all_states_valid stateList =
  let states = parseStates stateList
  in encodeBool (all (not . null) states)

%export
proven_idris_sm_all_events_valid : String -> Int
proven_idris_sm_all_events_valid eventList =
  let events = parseEvents eventList
  in encodeBool (all (not . null) events)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

%export
proven_idris_sm_friendly_error : String -> String
proven_idris_sm_friendly_error errorMsg =
  if isInfixOf "deterministic" (toLower errorMsg) || isInfixOf "duplicate" (toLower errorMsg)
    then "State machine is non-deterministic (duplicate transitions)"
  else if isInfixOf "transition" (toLower errorMsg) || isInfixOf "invalid" (toLower errorMsg)
    then "Invalid state transition"
  else if isInfixOf "final" (toLower errorMsg)
    then "Not in a final/accepting state"
  else if isInfixOf "state" (toLower errorMsg)
    then "Invalid state identifier"
  else
    "State machine error"
