-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeStateMachine - Safe finite state machine implementation
|||
||| This module provides type-safe state machines with
||| validated transitions and event handling.
module Proven.SafeStateMachine
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- State Machine Types
--------------------------------------------------------------------------------

||| A transition from one state to another
public export
record Transition state event where
  constructor MkTransition
  fromState : state
  trigger : event
  toState : state

||| A state machine definition
public export
record StateMachine state event where
  constructor MkMachine
  initialState : state
  transitions : List (Transition state event)
  finalStates : List state

||| Runtime state machine instance
public export
record MachineInstance state event where
  constructor MkInstance
  machine : StateMachine state event
  currentState : state
  history : List (state, event)  -- State history with triggering events

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

||| Create a new state machine
public export
newMachine : (initial : state) -> StateMachine state event
newMachine init = MkMachine init [] []

||| Add a transition to the machine
public export
addTransition : Eq state => Eq event =>
                state -> event -> state -> 
                StateMachine state event -> StateMachine state event
addTransition from evt to machine =
  MkMachine machine.initialState 
            (MkTransition from evt to :: machine.transitions)
            machine.finalStates

||| Mark states as final/accepting
public export
setFinalStates : List state -> StateMachine state event -> StateMachine state event
setFinalStates finals machine =
  MkMachine machine.initialState machine.transitions finals

||| Create a running instance of a state machine
public export
start : StateMachine state event -> MachineInstance state event
start machine = MkInstance machine machine.initialState []

--------------------------------------------------------------------------------
-- Transitions
--------------------------------------------------------------------------------

||| Find valid transition for current state and event
findTransition : Eq state => Eq event =>
                 state -> event -> List (Transition state event) -> Maybe state
findTransition _ _ [] = Nothing
findTransition curr evt (t :: ts) =
  if t.fromState == curr && t.trigger == evt
    then Just t.toState
    else findTransition curr evt ts

||| Attempt to trigger an event
public export
trigger : Eq state => Eq event =>
          event -> MachineInstance state event -> Maybe (MachineInstance state event)
trigger evt inst =
  case findTransition inst.currentState evt inst.machine.transitions of
    Nothing => Nothing
    Just next => Just (MkInstance inst.machine next ((inst.currentState, evt) :: inst.history))

||| Force transition to a state (unsafe - bypasses validation)
public export
forceState : state -> MachineInstance state event -> MachineInstance state event
forceState newState inst = MkInstance inst.machine newState inst.history

||| Get the current state
public export
getState : MachineInstance state event -> state
getState = currentState

||| Check if in a final state
public export
isInFinalState : Eq state => MachineInstance state event -> Bool
isInFinalState inst = any (== inst.currentState) inst.machine.finalStates

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

||| Get all possible events from current state
public export
availableEvents : Eq state => MachineInstance state event -> List event
availableEvents inst =
  map trigger (filter (\t => t.fromState == inst.currentState) inst.machine.transitions)

||| Check if an event can be triggered
public export
canTrigger : Eq state => Eq event => event -> MachineInstance state event -> Bool
canTrigger evt inst =
  case findTransition inst.currentState evt inst.machine.transitions of
    Nothing => False
    Just _ => True

||| Get all states reachable from current state
public export
reachableStates : Eq state => MachineInstance state event -> List state
reachableStates inst =
  map toState (filter (\t => t.fromState == inst.currentState) inst.machine.transitions)

||| Get the transition history
public export
getHistory : MachineInstance state event -> List (state, event)
getHistory = history

||| Reset the machine to initial state
public export
reset : MachineInstance state event -> MachineInstance state event
reset inst = MkInstance inst.machine inst.machine.initialState []

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

||| Check if machine is deterministic (no duplicate transitions)
public export
isDeterministic : Eq state => Eq event => StateMachine state event -> Bool
isDeterministic machine = noDuplicates machine.transitions
  where
    conflicts : Transition state event -> Transition state event -> Bool
    conflicts a b = a.fromState == b.fromState && a.trigger == b.trigger && a.toState /= b.toState
    
    noDuplicates : List (Transition state event) -> Bool
    noDuplicates [] = True
    noDuplicates (t :: ts) = not (any (conflicts t) ts) && noDuplicates ts

||| Remove duplicates (local, uses assert_total)
nubSM : Eq a => List a -> List a
nubSM [] = []
nubSM (x :: xs) = x :: assert_total (nubSM (filter (/= x) xs))

||| Get all states mentioned in the machine
public export
allStates : Eq state => StateMachine state event -> List state
allStates machine =
  nubSM (machine.initialState :: machine.finalStates ++
         concatMap (\t => [t.fromState, t.toState]) machine.transitions)

||| Count total number of transitions
public export
transitionCount : StateMachine state event -> Nat
transitionCount machine = length machine.transitions

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show state => Show (MachineInstance state event) where
  show inst = "Machine(state=" ++ show inst.currentState ++ 
              ", history=" ++ show (length inst.history) ++ " steps)"

