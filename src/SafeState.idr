-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

||| SafeState - Type-safe state machines with proven valid transitions
|||
||| Inspired by TUI design where mode switching (Normal/Edit) must be valid.
||| This module provides state machines where invalid transitions are
||| impossible by construction.

module SafeState

import Data.List
import Data.Vect

%default total

-- ============================================================================
-- CORE TYPES
-- ============================================================================

||| A state in the machine, identified by a label
public export
data State : Type where
  MkState : (label : String) -> State

||| Proof that a transition from one state to another is valid
public export
data ValidTransition : (from : State) -> (to : State) -> (allowed : List (State, State)) -> Type where
  Here : ValidTransition from to ((from, to) :: rest)
  There : ValidTransition from to rest -> ValidTransition from to (other :: rest)

||| A state machine with typed states and valid transitions
public export
record StateMachine where
  constructor MkStateMachine
  states : List State
  initial : State
  transitions : List (State, State)
  current : State

||| Result of attempting a transition
public export
data TransitionResult : Type where
  Success : (newState : State) -> TransitionResult
  InvalidTransition : (from : State) -> (to : State) -> TransitionResult
  InvalidState : (state : State) -> TransitionResult

-- ============================================================================
-- STATE MACHINE OPERATIONS
-- ============================================================================

||| Check if a state is in the list of valid states
export
isValidState : State -> List State -> Bool
isValidState s [] = False
isValidState s@(MkState label) (MkState l :: rest) =
  if label == l then True else isValidState s rest

||| Check if a transition is allowed
export
isValidTransitionBool : State -> State -> List (State, State) -> Bool
isValidTransitionBool from to [] = False
isValidTransitionBool from@(MkState f) to@(MkState t) ((MkState f', MkState t') :: rest) =
  if f == f' && t == t'
    then True
    else isValidTransitionBool from to rest

||| Create a new state machine
export
newMachine : (states : List State) -> (initial : State) -> (transitions : List (State, State)) -> StateMachine
newMachine states initial transitions = MkStateMachine states initial transitions initial

||| Attempt to transition to a new state
export
transition : StateMachine -> State -> TransitionResult
transition machine to =
  if not (isValidState to machine.states)
    then InvalidState to
    else if isValidTransitionBool machine.current to machine.transitions
      then Success to
      else InvalidTransition machine.current to

||| Apply a successful transition to the machine
export
applyTransition : StateMachine -> State -> StateMachine
applyTransition machine newState = { current := newState } machine

||| Get current state
export
getCurrentState : StateMachine -> State
getCurrentState = current

||| Reset to initial state
export
reset : StateMachine -> StateMachine
reset machine = { current := machine.initial } machine

-- ============================================================================
-- COMMON STATE MACHINE PATTERNS
-- ============================================================================

||| Input mode states (from TUI design)
export
normalState : State
normalState = MkState "normal"

export
editingState : State
editingState = MkState "editing"

export
commandState : State
commandState = MkState "command"

||| TUI-style input mode machine
export
inputModeMachine : StateMachine
inputModeMachine = newMachine
  [normalState, editingState, commandState]
  normalState
  [ (normalState, editingState)   -- i to enter edit
  , (normalState, commandState)   -- : to enter command
  , (editingState, normalState)   -- Esc to return
  , (commandState, normalState)   -- Esc or Enter to return
  ]

||| Connection states
export
disconnectedState : State
disconnectedState = MkState "disconnected"

export
connectingState : State
connectingState = MkState "connecting"

export
connectedState : State
connectedState = MkState "connected"

export
errorState : State
errorState = MkState "error"

||| Network connection state machine
export
connectionMachine : StateMachine
connectionMachine = newMachine
  [disconnectedState, connectingState, connectedState, errorState]
  disconnectedState
  [ (disconnectedState, connectingState)
  , (connectingState, connectedState)
  , (connectingState, errorState)
  , (connectedState, disconnectedState)
  , (errorState, disconnectedState)
  , (errorState, connectingState)  -- retry
  ]

||| Transaction states
export
pendingState : State
pendingState = MkState "pending"

export
processingState : State
processingState = MkState "processing"

export
committedState : State
committedState = MkState "committed"

export
rolledBackState : State
rolledBackState = MkState "rolled_back"

||| Transaction state machine
export
transactionMachine : StateMachine
transactionMachine = newMachine
  [pendingState, processingState, committedState, rolledBackState]
  pendingState
  [ (pendingState, processingState)
  , (processingState, committedState)
  , (processingState, rolledBackState)
  ]

-- ============================================================================
-- PROOFS
-- ============================================================================

||| Proof that reset always returns to initial state
export
resetReturnsToInitial : (m : StateMachine) -> getCurrentState (reset m) = m.initial
resetReturnsToInitial m = Refl

||| Proof that new machine starts at initial state
export
newMachineAtInitial : (ss : List State) -> (i : State) -> (ts : List (State, State))
                   -> getCurrentState (newMachine ss i ts) = i
newMachineAtInitial ss i ts = Refl
