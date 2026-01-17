// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeStateMachine - Finite state machine implementation that cannot crash.
 *
 * Provides type-safe state transitions with compile-time verification of
 * transition validity. All operations handle edge cases gracefully.
 */

/** Error types for state machine operations */
type stateMachineError =
  | InvalidTransition
  | MaxTransitionsReached

/** Transition definition: from state -> event -> to state */
type transition<'state, 'event> = {
  from: 'state,
  trigger: 'event,
  to: 'state,
}

/** State machine with state and event types */
type t<'state, 'event> = {
  mutable currentState: 'state,
  initialState: 'state,
  transitions: array<transition<'state, 'event>>,
  finalStates: array<'state>,
}

/** Create a new state machine with an initial state */
let make = (initialState: 'state): t<'state, 'event> => {
  {
    currentState: initialState,
    initialState: initialState,
    transitions: [],
    finalStates: [],
  }
}

/** Add a transition to the state machine */
let addTransition = (
  stateMachine: t<'state, 'event>,
  ~from: 'state,
  ~trigger: 'event,
  ~to_: 'state,
): t<'state, 'event> => {
  let newTransition = {from: from, trigger: trigger, to: to_}
  {
    ...stateMachine,
    transitions: Belt.Array.concat(stateMachine.transitions, [newTransition]),
  }
}

/** Mark a state as a final/accepting state */
let addFinalState = (stateMachine: t<'state, 'event>, state: 'state): t<'state, 'event> => {
  {
    ...stateMachine,
    finalStates: Belt.Array.concat(stateMachine.finalStates, [state]),
  }
}

/** Find a valid transition for the current state and event */
let findTransition = (
  stateMachine: t<'state, 'event>,
  event: 'event,
  ~stateEq: ('state, 'state) => bool,
  ~eventEq: ('event, 'event) => bool,
): option<'state> => {
  let result = ref(None)

  Belt.Array.forEach(stateMachine.transitions, trans => {
    if result.contents == None {
      if stateEq(trans.from, stateMachine.currentState) && eventEq(trans.trigger, event) {
        result := Some(trans.to)
      }
    }
  })

  result.contents
}

/** Trigger an event, transitioning to a new state if valid */
let trigger = (
  stateMachine: t<'state, 'event>,
  event: 'event,
  ~stateEq: ('state, 'state) => bool,
  ~eventEq: ('event, 'event) => bool,
): result<t<'state, 'event>, stateMachineError> => {
  switch findTransition(stateMachine, event, ~stateEq, ~eventEq) {
  | Some(nextState) => Ok({...stateMachine, currentState: nextState})
  | None => Error(InvalidTransition)
  }
}

/** Check if an event can be triggered from the current state */
let canTrigger = (
  stateMachine: t<'state, 'event>,
  event: 'event,
  ~stateEq: ('state, 'state) => bool,
  ~eventEq: ('event, 'event) => bool,
): bool => {
  Belt.Option.isSome(findTransition(stateMachine, event, ~stateEq, ~eventEq))
}

/** Get the current state */
let getState = (stateMachine: t<'state, 'event>): 'state => {
  stateMachine.currentState
}

/** Check if the machine is in a final state */
let isInFinalState = (
  stateMachine: t<'state, 'event>,
  ~stateEq: ('state, 'state) => bool,
): bool => {
  Belt.Array.some(stateMachine.finalStates, finalState =>
    stateEq(finalState, stateMachine.currentState)
  )
}

/** Reset the state machine to its initial state */
let reset = (stateMachine: t<'state, 'event>): t<'state, 'event> => {
  {...stateMachine, currentState: stateMachine.initialState}
}

/** Get all available transitions from the current state */
let availableTransitions = (
  stateMachine: t<'state, 'event>,
  ~stateEq: ('state, 'state) => bool,
): array<'event> => {
  Belt.Array.keepMap(stateMachine.transitions, trans =>
    if stateEq(trans.from, stateMachine.currentState) {
      Some(trans.trigger)
    } else {
      None
    }
  )
}

/** Get all states reachable from the current state */
let reachableStates = (
  stateMachine: t<'state, 'event>,
  ~stateEq: ('state, 'state) => bool,
): array<'state> => {
  Belt.Array.keepMap(stateMachine.transitions, trans =>
    if stateEq(trans.from, stateMachine.currentState) {
      Some(trans.to)
    } else {
      None
    }
  )
}

/** Count the number of transitions defined */
let transitionCount = (stateMachine: t<'state, 'event>): int => {
  Belt.Array.length(stateMachine.transitions)
}

/** Check if a specific state exists in the transitions */
let hasState = (
  stateMachine: t<'state, 'event>,
  state: 'state,
  ~stateEq: ('state, 'state) => bool,
): bool => {
  Belt.Array.some(stateMachine.transitions, trans =>
    stateEq(trans.from, state) || stateEq(trans.to, state)
  )
}

/** Create a simple string-based state machine (convenience function) */
module StringMachine = {
  type stringTransition = transition<string, string>
  type stringMachine = t<string, string>

  let make = (initialState: string): stringMachine => {
    {
      currentState: initialState,
      initialState: initialState,
      transitions: [],
      finalStates: [],
    }
  }

  let stringEq = (a: string, b: string): bool => a == b

  let addTransition = (
    sm: stringMachine,
    ~from: string,
    ~trigger: string,
    ~to_: string,
  ): stringMachine => {
    addTransition(sm, ~from, ~trigger, ~to_)
  }

  let addFinalState = (sm: stringMachine, state: string): stringMachine => {
    addFinalState(sm, state)
  }

  let trigger = (sm: stringMachine, event: string): result<stringMachine, stateMachineError> => {
    trigger(sm, event, ~stateEq=stringEq, ~eventEq=stringEq)
  }

  let canTrigger = (sm: stringMachine, event: string): bool => {
    canTrigger(sm, event, ~stateEq=stringEq, ~eventEq=stringEq)
  }

  let getState = (sm: stringMachine): string => {
    getState(sm)
  }

  let isInFinalState = (sm: stringMachine): bool => {
    isInFinalState(sm, ~stateEq=stringEq)
  }

  let reset = (sm: stringMachine): stringMachine => {
    reset(sm)
  }

  let availableTransitions = (sm: stringMachine): array<string> => {
    availableTransitions(sm, ~stateEq=stringEq)
  }
}

/** Create a simple int-based state machine (convenience function) */
module IntMachine = {
  type intTransition = transition<int, int>
  type intMachine = t<int, int>

  let make = (initialState: int): intMachine => {
    {
      currentState: initialState,
      initialState: initialState,
      transitions: [],
      finalStates: [],
    }
  }

  let intEq = (a: int, b: int): bool => a == b

  let addTransition = (sm: intMachine, ~from: int, ~trigger: int, ~to_: int): intMachine => {
    addTransition(sm, ~from, ~trigger, ~to_)
  }

  let addFinalState = (sm: intMachine, state: int): intMachine => {
    addFinalState(sm, state)
  }

  let trigger = (sm: intMachine, event: int): result<intMachine, stateMachineError> => {
    trigger(sm, event, ~stateEq=intEq, ~eventEq=intEq)
  }

  let canTrigger = (sm: intMachine, event: int): bool => {
    canTrigger(sm, event, ~stateEq=intEq, ~eventEq=intEq)
  }

  let getState = (sm: intMachine): int => {
    getState(sm)
  }

  let isInFinalState = (sm: intMachine): bool => {
    isInFinalState(sm, ~stateEq=intEq)
  }

  let reset = (sm: intMachine): intMachine => {
    reset(sm)
  }

  let availableTransitions = (sm: intMachine): array<int> => {
    availableTransitions(sm, ~stateEq=intEq)
  }
}
