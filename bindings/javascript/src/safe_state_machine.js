// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeStateMachine - Type-safe finite state machines.
 *
 * Provides deterministic state transitions with validation.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Transition definition.
 *
 * @typedef {Object} Transition
 * @property {string} from - Source state
 * @property {string} to - Target state
 * @property {string} event - Triggering event
 * @property {((context: any) => boolean)} [guard] - Optional guard function
 * @property {((context: any) => void)} [action] - Optional action function
 */

/**
 * State machine configuration.
 *
 * @typedef {Object} StateMachineConfig
 * @property {string[]} states - Valid states
 * @property {string} initial - Initial state
 * @property {Transition[]} transitions - Valid transitions
 * @property {Object.<string, (context: any) => void>} [onEnter] - State entry callbacks
 * @property {Object.<string, (context: any) => void>} [onExit] - State exit callbacks
 */

/**
 * Finite state machine.
 *
 * @template C Context type
 */
export class StateMachine {
  /** @type {Set<string>} */
  #states;
  /** @type {string} */
  #current;
  /** @type {string} */
  #initial;
  /** @type {Map<string, Map<string, Transition>>} */
  #transitions;
  /** @type {Map<string, (context: C) => void>} */
  #onEnter;
  /** @type {Map<string, (context: C) => void>} */
  #onExit;
  /** @type {C} */
  #context;
  /** @type {string[]} */
  #history;
  /** @type {number} */
  #maxHistory;

  /**
   * Create a state machine.
   *
   * @param {StateMachineConfig} config - Configuration
   * @param {C} [context] - Initial context
   * @param {number} [maxHistory=100] - Maximum history length
   */
  constructor(config, context = /** @type {C} */ ({}), maxHistory = 100) {
    const { states, initial, transitions, onEnter = {}, onExit = {} } = config;

    if (!states || states.length === 0) {
      throw new Error('States cannot be empty');
    }
    if (!states.includes(initial)) {
      throw new Error('Initial state must be in states list');
    }

    this.#states = new Set(states);
    this.#initial = initial;
    this.#current = initial;
    this.#context = context;
    this.#history = [initial];
    this.#maxHistory = maxHistory;

    // Build transition map: state -> event -> transition
    this.#transitions = new Map();
    for (const state of states) {
      this.#transitions.set(state, new Map());
    }

    for (const transition of transitions) {
      if (!this.#states.has(transition.from)) {
        throw new Error(`Invalid source state: ${transition.from}`);
      }
      if (!this.#states.has(transition.to)) {
        throw new Error(`Invalid target state: ${transition.to}`);
      }

      const stateTransitions = this.#transitions.get(transition.from);
      if (stateTransitions.has(transition.event)) {
        throw new Error(`Duplicate transition: ${transition.from} + ${transition.event}`);
      }
      stateTransitions.set(transition.event, transition);
    }

    // Store callbacks
    this.#onEnter = new Map(Object.entries(onEnter));
    this.#onExit = new Map(Object.entries(onExit));

    // Call initial state onEnter
    const enterCallback = this.#onEnter.get(initial);
    if (enterCallback) {
      enterCallback(this.#context);
    }
  }

  /**
   * Get current state.
   *
   * @returns {string}
   */
  get current() {
    return this.#current;
  }

  /**
   * Get context.
   *
   * @returns {C}
   */
  get context() {
    return this.#context;
  }

  /**
   * Get all valid states.
   *
   * @returns {string[]}
   */
  get states() {
    return Array.from(this.#states);
  }

  /**
   * Get state history.
   *
   * @returns {string[]}
   */
  get history() {
    return [...this.#history];
  }

  /**
   * Check if in a specific state.
   *
   * @param {string} state - State to check
   * @returns {boolean}
   */
  isIn(state) {
    return this.#current === state;
  }

  /**
   * Check if an event can be triggered.
   *
   * @param {string} event - Event to check
   * @returns {boolean}
   */
  canTrigger(event) {
    const stateTransitions = this.#transitions.get(this.#current);
    if (!stateTransitions) {
      return false;
    }

    const transition = stateTransitions.get(event);
    if (!transition) {
      return false;
    }

    // Check guard if present
    if (transition.guard && !transition.guard(this.#context)) {
      return false;
    }

    return true;
  }

  /**
   * Trigger an event to transition.
   *
   * @param {string} event - Event to trigger
   * @returns {{ ok: true, value: string } | { ok: false, error: string }}
   */
  trigger(event) {
    const stateTransitions = this.#transitions.get(this.#current);
    if (!stateTransitions) {
      return err(`Invalid current state: ${this.#current}`);
    }

    const transition = stateTransitions.get(event);
    if (!transition) {
      return err(`No transition for event '${event}' from state '${this.#current}'`);
    }

    // Check guard
    if (transition.guard && !transition.guard(this.#context)) {
      return err(`Guard prevented transition on '${event}'`);
    }

    const fromState = this.#current;
    const toState = transition.to;

    // Call exit callback
    const exitCallback = this.#onExit.get(fromState);
    if (exitCallback) {
      exitCallback(this.#context);
    }

    // Call transition action
    if (transition.action) {
      transition.action(this.#context);
    }

    // Update state
    this.#current = toState;

    // Add to history
    this.#history.push(toState);
    if (this.#history.length > this.#maxHistory) {
      this.#history.shift();
    }

    // Call enter callback
    const enterCallback = this.#onEnter.get(toState);
    if (enterCallback) {
      enterCallback(this.#context);
    }

    return ok(toState);
  }

  /**
   * Get available events from current state.
   *
   * @returns {string[]}
   */
  availableEvents() {
    const stateTransitions = this.#transitions.get(this.#current);
    if (!stateTransitions) {
      return [];
    }

    const events = [];
    for (const [event, transition] of stateTransitions) {
      // Include event if no guard or guard passes
      if (!transition.guard || transition.guard(this.#context)) {
        events.push(event);
      }
    }

    return events;
  }

  /**
   * Get all transitions from current state.
   *
   * @returns {Array<{event: string, to: string}>}
   */
  availableTransitions() {
    const stateTransitions = this.#transitions.get(this.#current);
    if (!stateTransitions) {
      return [];
    }

    const transitions = [];
    for (const [event, transition] of stateTransitions) {
      if (!transition.guard || transition.guard(this.#context)) {
        transitions.push({ event, to: transition.to });
      }
    }

    return transitions;
  }

  /**
   * Update context.
   *
   * @param {Partial<C>} updates - Context updates
   */
  updateContext(updates) {
    Object.assign(this.#context, updates);
  }

  /**
   * Reset to initial state.
   *
   * @param {C} [newContext] - Optional new context
   */
  reset(newContext) {
    // Call exit on current state
    const exitCallback = this.#onExit.get(this.#current);
    if (exitCallback) {
      exitCallback(this.#context);
    }

    // Reset state
    this.#current = this.#initial;
    this.#history = [this.#initial];

    if (newContext !== undefined) {
      this.#context = newContext;
    }

    // Call enter on initial state
    const enterCallback = this.#onEnter.get(this.#initial);
    if (enterCallback) {
      enterCallback(this.#context);
    }
  }

  /**
   * Export state for persistence.
   *
   * @returns {Object}
   */
  export() {
    return {
      current: this.#current,
      context: this.#context,
      history: this.#history,
    };
  }

  /**
   * Import state from persistence.
   *
   * @param {Object} data - Exported data
   * @returns {{ ok: true, value: void } | { ok: false, error: string }}
   */
  import(data) {
    if (!this.#states.has(data.current)) {
      return err(`Invalid state: ${data.current}`);
    }

    this.#current = data.current;
    this.#context = data.context;
    this.#history = data.history || [data.current];

    return ok(undefined);
  }
}

/**
 * State machine builder for fluent API.
 *
 * @template C Context type
 */
export class StateMachineBuilder {
  /** @type {string[]} */
  #states;
  /** @type {string | undefined} */
  #initial;
  /** @type {Transition[]} */
  #transitions;
  /** @type {Object.<string, (context: C) => void>} */
  #onEnter;
  /** @type {Object.<string, (context: C) => void>} */
  #onExit;

  constructor() {
    this.#states = [];
    this.#initial = undefined;
    this.#transitions = [];
    this.#onEnter = {};
    this.#onExit = {};
  }

  /**
   * Add a state.
   *
   * @param {string} state - State name
   * @returns {StateMachineBuilder<C>}
   */
  addState(state) {
    if (!this.#states.includes(state)) {
      this.#states.push(state);
    }
    return this;
  }

  /**
   * Add multiple states.
   *
   * @param {string[]} states - State names
   * @returns {StateMachineBuilder<C>}
   */
  addStates(states) {
    for (const state of states) {
      this.addState(state);
    }
    return this;
  }

  /**
   * Set initial state.
   *
   * @param {string} state - Initial state
   * @returns {StateMachineBuilder<C>}
   */
  setInitial(state) {
    this.#initial = state;
    return this;
  }

  /**
   * Add a transition.
   *
   * @param {string} from - Source state
   * @param {string} event - Triggering event
   * @param {string} to - Target state
   * @param {Object} [options] - Options
   * @param {(context: C) => boolean} [options.guard] - Guard function
   * @param {(context: C) => void} [options.action] - Action function
   * @returns {StateMachineBuilder<C>}
   */
  addTransition(from, event, to, options = {}) {
    this.#transitions.push({
      from,
      to,
      event,
      guard: options.guard,
      action: options.action,
    });
    return this;
  }

  /**
   * Add state entry callback.
   *
   * @param {string} state - State name
   * @param {(context: C) => void} callback - Callback function
   * @returns {StateMachineBuilder<C>}
   */
  onEnter(state, callback) {
    this.#onEnter[state] = callback;
    return this;
  }

  /**
   * Add state exit callback.
   *
   * @param {string} state - State name
   * @param {(context: C) => void} callback - Callback function
   * @returns {StateMachineBuilder<C>}
   */
  onExit(state, callback) {
    this.#onExit[state] = callback;
    return this;
  }

  /**
   * Build the state machine.
   *
   * @param {C} [context] - Initial context
   * @returns {{ ok: true, value: StateMachine<C> } | { ok: false, error: string }}
   */
  build(context) {
    if (this.#states.length === 0) {
      return err('No states defined');
    }
    if (!this.#initial) {
      return err('No initial state set');
    }

    try {
      const machine = new StateMachine(
        {
          states: this.#states,
          initial: this.#initial,
          transitions: this.#transitions,
          onEnter: this.#onEnter,
          onExit: this.#onExit,
        },
        context,
      );
      return ok(machine);
    } catch (error) {
      return err(error instanceof Error ? error.message : String(error));
    }
  }
}

/**
 * Safe state machine utilities.
 */
export class SafeStateMachine {
  /**
   * Create a state machine builder.
   *
   * @template C
   * @returns {StateMachineBuilder<C>}
   */
  static builder() {
    return new StateMachineBuilder();
  }

  /**
   * Create a state machine from config.
   *
   * @template C
   * @param {StateMachineConfig} config - Configuration
   * @param {C} [context] - Initial context
   * @returns {{ ok: true, value: StateMachine<C> } | { ok: false, error: string }}
   */
  static create(config, context) {
    try {
      return ok(new StateMachine(config, context));
    } catch (error) {
      return err(error instanceof Error ? error.message : String(error));
    }
  }

  /**
   * Create a simple traffic light state machine (example).
   *
   * @returns {StateMachine<{}>}
   */
  static trafficLight() {
    return new StateMachine({
      states: ['red', 'yellow', 'green'],
      initial: 'red',
      transitions: [
        { from: 'red', event: 'timer', to: 'green' },
        { from: 'green', event: 'timer', to: 'yellow' },
        { from: 'yellow', event: 'timer', to: 'red' },
      ],
    });
  }
}
