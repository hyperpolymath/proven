// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/**
 * SafeStateMachine - Type-safe state transitions.
 *
 * Thin FFI wrapper: all computation delegates to libproven (Idris 2 + Zig).
 * @module
 */

import { getLib } from './ffi.js';
import { ok, err } from './result.js';

/**
 * State machine backed by the libproven FFI.
 * States are identified by integer IDs (0-based).
 */
export class StateMachine {
  /** @type {Deno.PointerObject|null} */
  #ptr = null;

  /**
   * Create a state machine.
   *
   * @param {number} stateCount - Total number of states.
   * @param {number} initialState - Initial state ID.
   * @returns {{ ok: true, value: StateMachine } | { ok: false, error: string }}
   */
  static create(stateCount, initialState) {
    const symbols = getLib();
    const ptr = symbols.proven_state_machine_create(stateCount >>> 0, initialState >>> 0);
    if (ptr === null) return err('Failed to create state machine');
    const sm = new StateMachine();
    sm.#ptr = ptr;
    return ok(sm);
  }

  /**
   * Allow a transition from one state to another.
   *
   * @param {number} from - Source state ID.
   * @param {number} to - Target state ID.
   * @returns {boolean} True if the transition was registered.
   */
  allowTransition(from, to) {
    if (this.#ptr === null) return false;
    const symbols = getLib();
    return symbols.proven_state_machine_allow(this.#ptr, from >>> 0, to >>> 0);
  }

  /**
   * Try to transition to a new state.
   *
   * @param {number} to - Target state ID.
   * @returns {boolean} True if the transition was valid and executed.
   */
  transition(to) {
    if (this.#ptr === null) return false;
    const symbols = getLib();
    return symbols.proven_state_machine_transition(this.#ptr, to >>> 0);
  }

  /**
   * Get the current state ID.
   *
   * @returns {number}
   */
  getState() {
    if (this.#ptr === null) return 0;
    const symbols = getLib();
    return symbols.proven_state_machine_state(this.#ptr);
  }

  /**
   * Close and free the native state machine.
   */
  close() {
    if (this.#ptr !== null) {
      const symbols = getLib();
      symbols.proven_state_machine_free(this.#ptr);
      this.#ptr = null;
    }
  }
}

/**
 * Builder for constructing a state machine with named states.
 * Maps string state names to integer IDs internally.
 */
export class StateMachineBuilder {
  /** @type {Map<string, number>} */
  #states = new Map();
  /** @type {Array<[number, number]>} */
  #transitions = [];
  /** @type {string|null} */
  #initialState = null;
  /** @type {number} */
  #nextId = 0;

  /**
   * Register a named state (called implicitly by addTransition).
   *
   * @param {string} name - State name.
   * @returns {number} The integer ID for this state.
   */
  #getOrAddState(name) {
    if (!this.#states.has(name)) {
      this.#states.set(name, this.#nextId++);
    }
    return this.#states.get(name);
  }

  /**
   * Set the initial state.
   *
   * @param {string} name - State name.
   * @returns {StateMachineBuilder}
   */
  initialState(name) {
    this.#initialState = name;
    this.#getOrAddState(name);
    return this;
  }

  /**
   * Add a transition.
   *
   * @param {string} from - Source state name.
   * @param {string} _event - Event name (for documentation; not used in FFI).
   * @param {string} to - Target state name.
   * @returns {StateMachineBuilder}
   */
  addTransition(from, _event, to) {
    const fromId = this.#getOrAddState(from);
    const toId = this.#getOrAddState(to);
    this.#transitions.push([fromId, toId]);
    return this;
  }

  /**
   * Build the state machine.
   *
   * @returns {{ ok: true, value: StateMachine } | { ok: false, error: string }}
   */
  build() {
    if (this.#initialState === null) return err('No initial state set');
    const initialId = this.#states.get(this.#initialState);
    if (initialId === undefined) return err('Initial state not found');

    const result = StateMachine.create(this.#nextId, initialId);
    if (!result.ok) return result;

    const sm = result.value;
    for (const [from, to] of this.#transitions) {
      sm.allowTransition(from, to);
    }
    return ok(sm);
  }
}

/**
 * SafeStateMachine namespace.
 */
export class SafeStateMachine {
  /**
   * Create a state machine.
   *
   * @param {number} stateCount
   * @param {number} initialState
   * @returns {{ ok: true, value: StateMachine } | { ok: false, error: string }}
   */
  static create(stateCount, initialState) {
    return StateMachine.create(stateCount, initialState);
  }
}
