// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeStateMachine - Typed wrapper for finite state machines.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * The JS FFI layer provides StateMachine and StateMachineBuilder classes
 * backed by native state machine implementations.
 */

/** Opaque handle to a JavaScript StateMachine instance. */
type stateMachine

/** Opaque handle to a JavaScript StateMachineBuilder instance. */
type stateMachineBuilder

/** Transition result from the state machine. */
type transitionResult = {
  success: bool,
  previousState: string,
  currentState: string,
  error: option<string>,
}

/** JavaScript bindings to the SafeStateMachine FFI wrapper. */
module SafeStateMachineJs = {
  @module("../../javascript/src/safe_state_machine.js")
  @new
  external createBuilder: unit => stateMachineBuilder = "StateMachineBuilder"
}

/**
 * Create a new state machine builder.
 *
 * @returns A new StateMachineBuilder handle for fluent configuration.
 */
let createBuilder = SafeStateMachineJs.createBuilder
