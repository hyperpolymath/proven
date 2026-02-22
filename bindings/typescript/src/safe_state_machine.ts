// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeStateMachine - Typed wrapper for state machine transitions.
 *
 * Delegates all computation to the JavaScript FFI binding, which calls
 * libproven (Idris 2 + Zig) via Deno.dlopen. No logic is reimplemented here.
 *
 * @module
 */

import {
  StateMachine as JsStateMachine,
  StateMachineBuilder as JsStateMachineBuilder,
  SafeStateMachine as JsSafeStateMachine,
} from '../../javascript/src/safe_state_machine.js';

/** Result type returned by state machine operations. */
export type Result<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly error: string };

/**
 * Typed wrapper around the FFI-backed StateMachine.
 *
 * Every method calls through to the JavaScript FFI wrapper, which in turn
 * calls the formally verified Idris 2 code via the Zig FFI bridge.
 * States are identified by integer IDs (0-based).
 */
export class StateMachine {
  /** The underlying JS FFI state machine instance. */
  readonly #inner: InstanceType<typeof JsStateMachine>;

  /** @internal -- use StateMachine.create() or StateMachineBuilder.build(). */
  constructor(inner: InstanceType<typeof JsStateMachine>) {
    this.#inner = inner;
  }

  /**
   * Create a state machine via FFI.
   * Delegates to proven_state_machine_create.
   *
   * @param stateCount - Total number of states.
   * @param initialState - Initial state ID.
   * @returns Result with the StateMachine instance, or error.
   */
  static create(stateCount: number, initialState: number): Result<StateMachine> {
    const result = JsSafeStateMachine.create(
      stateCount,
      initialState,
    ) as Result<InstanceType<typeof JsStateMachine>>;
    if (!result.ok) return result;
    return { ok: true, value: new StateMachine(result.value) };
  }

  /**
   * Allow a transition from one state to another.
   * Delegates to proven_state_machine_allow via FFI.
   *
   * @param from - Source state ID.
   * @param to - Target state ID.
   * @returns True if the transition was registered.
   */
  allowTransition(from: number, to: number): boolean {
    return this.#inner.allowTransition(from, to);
  }

  /**
   * Try to transition to a new state.
   * Delegates to proven_state_machine_transition via FFI.
   *
   * @param to - Target state ID.
   * @returns True if the transition was valid and executed.
   */
  transition(to: number): boolean {
    return this.#inner.transition(to);
  }

  /**
   * Get the current state ID.
   * Delegates to proven_state_machine_state via FFI.
   *
   * @returns The current state ID.
   */
  getState(): number {
    return this.#inner.getState();
  }

  /**
   * Close and free the native state machine.
   * Delegates to proven_state_machine_free via FFI.
   */
  close(): void {
    this.#inner.close();
  }
}

/**
 * Builder for constructing a state machine with named states.
 * Maps string state names to integer IDs internally.
 *
 * Delegates to the JavaScript FFI StateMachineBuilder, which handles
 * name-to-ID mapping and calls the FFI layer.
 */
export class StateMachineBuilder {
  /** The underlying JS FFI builder instance. */
  readonly #inner: InstanceType<typeof JsStateMachineBuilder>;

  constructor() {
    this.#inner = new JsStateMachineBuilder();
  }

  /**
   * Set the initial state.
   * Delegates to the JS FFI builder.
   *
   * @param name - State name.
   * @returns This builder for chaining.
   */
  initialState(name: string): this {
    this.#inner.initialState(name);
    return this;
  }

  /**
   * Add a transition.
   * Delegates to the JS FFI builder.
   *
   * @param from - Source state name.
   * @param event - Event name (for documentation; not used in FFI).
   * @param to - Target state name.
   * @returns This builder for chaining.
   */
  addTransition(from: string, event: string, to: string): this {
    this.#inner.addTransition(from, event, to);
    return this;
  }

  /**
   * Build the state machine.
   * Delegates to the JS FFI builder, which constructs the native
   * state machine via proven_state_machine_create.
   *
   * @returns Result with the StateMachine instance, or error.
   */
  build(): Result<StateMachine> {
    const result = this.#inner.build() as Result<InstanceType<typeof JsStateMachine>>;
    if (!result.ok) return result;
    return { ok: true, value: new StateMachine(result.value) };
  }
}

/**
 * SafeStateMachine namespace.
 * All operations delegate to the JavaScript FFI binding.
 */
export class SafeStateMachine {
  /**
   * Create a state machine.
   * Delegates to proven_state_machine_create via FFI.
   *
   * @param stateCount - Total number of states.
   * @param initialState - Initial state ID.
   * @returns Result with the StateMachine instance, or error.
   */
  static create(stateCount: number, initialState: number): Result<StateMachine> {
    return StateMachine.create(stateCount, initialState);
  }
}
