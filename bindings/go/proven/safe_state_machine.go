// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeStateMachine provides type-safe state transitions via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"

// StateMachineHandle is a handle to a state machine allocated in the FFI layer.
type StateMachineHandle struct {
	ptr *C.StateMachine
}

// StateMachineCreate creates a new state machine with the given number of states
// and an initial state.
func StateMachineCreate(stateCount, initialState uint32) (*StateMachineHandle, error) {
	ptr := C.proven_state_machine_create(C.uint32_t(stateCount), C.uint32_t(initialState))
	if ptr == nil {
		return nil, newError(StatusErrAllocFailed)
	}
	return &StateMachineHandle{ptr: ptr}, nil
}

// AllowTransition allows a transition from one state to another.
// Returns true if the transition was successfully registered.
func (sm *StateMachineHandle) AllowTransition(from, to uint32) bool {
	return bool(C.proven_state_machine_allow(sm.ptr, C.uint32_t(from), C.uint32_t(to)))
}

// Transition attempts to transition to a new state.
// Returns true if the transition was allowed and executed.
func (sm *StateMachineHandle) Transition(to uint32) bool {
	return bool(C.proven_state_machine_transition(sm.ptr, C.uint32_t(to)))
}

// CurrentState returns the current state of the machine.
func (sm *StateMachineHandle) CurrentState() uint32 {
	return uint32(C.proven_state_machine_state(sm.ptr))
}

// Free releases the state machine's memory.
func (sm *StateMachineHandle) Free() {
	if sm.ptr != nil {
		C.proven_state_machine_free(sm.ptr)
		sm.ptr = nil
	}
}
