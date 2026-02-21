-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeStateMachine operations (65/75)
module Proven.FFI.SafeStateMachine

import Proven.SafeStateMachine
import Proven.Core

%default total

encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

export
proven_idris_fsm_is_final_state : Int -> Int -> Int
proven_idris_fsm_is_final_state currentState finalStatesCount =
  encodeBool (finalStatesCount > 0)

export
proven_idris_fsm_transition_valid : Int -> Int -> Int
proven_idris_fsm_transition_valid fromState toState =
  encodeBool (fromState /= toState)

export
proven_idris_fsm_can_trigger : Int -> Int
proven_idris_fsm_can_trigger transitionExists = encodeBool (transitionExists /= 0)

export
proven_idris_fsm_history_depth : Int -> Int
proven_idris_fsm_history_depth historyLength = historyLength
