// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * Guard function type for transitions.
 */
export type Guard<C> = (context: C) => boolean;

/**
 * Action function type for transitions.
 */
export type Action<C> = (context: C) => void;

/**
 * Transition definition.
 */
export interface Transition<S, E, C> {
  from: S;
  event: E;
  to: S;
  guard?: Guard<C>;
  action?: Action<C>;
}

/**
 * State machine configuration.
 */
export interface StateMachineConfig<S, E, C> {
  initial: S;
  context: C;
  transitions: Transition<S, E, C>[];
  onEnter?: Map<S, Action<C>>;
  onExit?: Map<S, Action<C>>;
}

/**
 * Transition result.
 */
export interface TransitionResult<S> {
  success: boolean;
  previousState: S;
  currentState: S;
  error?: string;
}

/**
 * StateMachine provides a type-safe finite state machine implementation.
 */
export class StateMachine<S extends string, E extends string, C = object> {
  private currentState: S;
  private context: C;
  private readonly transitions: Map<string, Transition<S, E, C>[]>;
  private readonly onEnter: Map<S, Action<C>>;
  private readonly onExit: Map<S, Action<C>>;
  private history: { state: S; event?: E; timestamp: number }[] = [];
  private readonly maxHistorySize: number;

  constructor(config: StateMachineConfig<S, E, C>, maxHistorySize: number = 100) {
    this.currentState = config.initial;
    this.context = config.context;
    this.onEnter = config.onEnter ?? new Map();
    this.onExit = config.onExit ?? new Map();
    this.maxHistorySize = maxHistorySize;

    // Index transitions by (from, event)
    this.transitions = new Map();
    for (const transition of config.transitions) {
      const key = `${transition.from}:${transition.event}`;
      const existing = this.transitions.get(key) ?? [];
      existing.push(transition);
      this.transitions.set(key, existing);
    }

    // Record initial state
    this.history.push({ state: config.initial, timestamp: Date.now() });
  }

  /**
   * Get current state.
   */
  getState(): S {
    return this.currentState;
  }

  /**
   * Get context.
   */
  getContext(): C {
    return this.context;
  }

  /**
   * Update context.
   */
  setContext(context: Partial<C>): void {
    this.context = { ...this.context, ...context };
  }

  /**
   * Check if an event can be processed in the current state.
   */
  canHandle(event: E): boolean {
    const key = `${this.currentState}:${event}`;
    const transitions = this.transitions.get(key);
    if (!transitions) return false;

    return transitions.some((t) => !t.guard || t.guard(this.context));
  }

  /**
   * Get available events from current state.
   */
  availableEvents(): E[] {
    const events = new Set<E>();
    for (const [key, transitions] of this.transitions) {
      const [from] = key.split(':');
      if (from === this.currentState) {
        for (const t of transitions) {
          if (!t.guard || t.guard(this.context)) {
            events.add(t.event);
          }
        }
      }
    }
    return Array.from(events);
  }

  /**
   * Send an event to the state machine.
   */
  send(event: E): TransitionResult<S> {
    const previousState = this.currentState;
    const key = `${this.currentState}:${event}`;
    const transitions = this.transitions.get(key);

    if (!transitions || transitions.length === 0) {
      return {
        success: false,
        previousState,
        currentState: this.currentState,
        error: `No transition from '${this.currentState}' for event '${event}'`,
      };
    }

    // Find first transition whose guard passes
    const transition = transitions.find((t) => !t.guard || t.guard(this.context));

    if (!transition) {
      return {
        success: false,
        previousState,
        currentState: this.currentState,
        error: `Guard condition failed for transition from '${this.currentState}' on '${event}'`,
      };
    }

    // Execute exit action
    const exitAction = this.onExit.get(this.currentState);
    if (exitAction) {
      exitAction(this.context);
    }

    // Execute transition action
    if (transition.action) {
      transition.action(this.context);
    }

    // Update state
    this.currentState = transition.to;

    // Execute enter action
    const enterAction = this.onEnter.get(this.currentState);
    if (enterAction) {
      enterAction(this.context);
    }

    // Record history
    this.history.push({ state: this.currentState, event, timestamp: Date.now() });
    if (this.history.length > this.maxHistorySize) {
      this.history.shift();
    }

    return {
      success: true,
      previousState,
      currentState: this.currentState,
    };
  }

  /**
   * Check if in a specific state.
   */
  is(state: S): boolean {
    return this.currentState === state;
  }

  /**
   * Check if in any of the given states.
   */
  isAny(states: S[]): boolean {
    return states.includes(this.currentState);
  }

  /**
   * Get state history.
   */
  getHistory(): { state: S; event?: E; timestamp: number }[] {
    return [...this.history];
  }

  /**
   * Reset to initial state.
   */
  reset(initialState: S, context?: C): void {
    this.currentState = initialState;
    if (context !== undefined) {
      this.context = context;
    }
    this.history = [{ state: initialState, timestamp: Date.now() }];
  }
}

/**
 * StateMachineBuilder provides a fluent API for building state machines.
 */
export class StateMachineBuilder<S extends string, E extends string, C = object> {
  private initial?: S;
  private context?: C;
  private transitions: Transition<S, E, C>[] = [];
  private onEnterCallbacks: Map<S, Action<C>> = new Map();
  private onExitCallbacks: Map<S, Action<C>> = new Map();

  /**
   * Set initial state.
   */
  withInitial(state: S): this {
    this.initial = state;
    return this;
  }

  /**
   * Set initial context.
   */
  withContext(context: C): this {
    this.context = context;
    return this;
  }

  /**
   * Add a transition.
   */
  addTransition(from: S, event: E, to: S, options?: { guard?: Guard<C>; action?: Action<C> }): this {
    this.transitions.push({
      from,
      event,
      to,
      guard: options?.guard,
      action: options?.action,
    });
    return this;
  }

  /**
   * Add an on-enter callback for a state.
   */
  onEnter(state: S, action: Action<C>): this {
    this.onEnterCallbacks.set(state, action);
    return this;
  }

  /**
   * Add an on-exit callback for a state.
   */
  onExit(state: S, action: Action<C>): this {
    this.onExitCallbacks.set(state, action);
    return this;
  }

  /**
   * Build the state machine.
   */
  build(): Result<StateMachine<S, E, C>> {
    if (!this.initial) {
      return { ok: false, error: 'Initial state is required' };
    }
    if (this.context === undefined) {
      return { ok: false, error: 'Context is required' };
    }
    if (this.transitions.length === 0) {
      return { ok: false, error: 'At least one transition is required' };
    }

    return {
      ok: true,
      value: new StateMachine({
        initial: this.initial,
        context: this.context,
        transitions: this.transitions,
        onEnter: this.onEnterCallbacks,
        onExit: this.onExitCallbacks,
      }),
    };
  }
}

/**
 * Simple traffic light state machine example.
 */
export type TrafficLightState = 'red' | 'yellow' | 'green';
export type TrafficLightEvent = 'timer' | 'emergency';

export function createTrafficLight(): StateMachine<TrafficLightState, TrafficLightEvent, object> {
  return new StateMachineBuilder<TrafficLightState, TrafficLightEvent, object>()
    .withInitial('red')
    .withContext({})
    .addTransition('red', 'timer', 'green')
    .addTransition('green', 'timer', 'yellow')
    .addTransition('yellow', 'timer', 'red')
    .addTransition('red', 'emergency', 'red')
    .addTransition('green', 'emergency', 'red')
    .addTransition('yellow', 'emergency', 'red')
    .build().value!;
}

export const SafeStateMachine = {
  StateMachine,
  StateMachineBuilder,
  createTrafficLight,
};
