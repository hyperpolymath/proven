// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"sync"
)

// State represents a state in the state machine.
type State string

// Event represents an event that can trigger transitions.
type Event string

// TransitionGuard is a function that returns true if a transition should be allowed.
type TransitionGuard func() bool

// TransitionAction is a function called when a transition occurs.
type TransitionAction func(from, to State, event Event)

// Transition defines a state transition.
type Transition struct {
	From   State
	To     State
	Event  Event
	Guard  TransitionGuard
	Action TransitionAction
}

// StateMachine is a finite state machine implementation.
type StateMachine struct {
	currentState  State
	transitions   map[State]map[Event]Transition
	onEnter       map[State]func(State)
	onExit        map[State]func(State)
	history       []StateHistoryEntry
	maxHistory    int
	mu            sync.RWMutex
}

// StateHistoryEntry records a state transition.
type StateHistoryEntry struct {
	From  State
	To    State
	Event Event
}

// NewStateMachine creates a new state machine with an initial state.
func NewStateMachine(initialState State) *StateMachine {
	return &StateMachine{
		currentState: initialState,
		transitions:  make(map[State]map[Event]Transition),
		onEnter:      make(map[State]func(State)),
		onExit:       make(map[State]func(State)),
		history:      make([]StateHistoryEntry, 0),
		maxHistory:   100,
	}
}

// AddTransition adds a transition to the state machine.
func (sm *StateMachine) AddTransition(from State, event Event, to State) {
	sm.AddTransitionWithGuardAndAction(from, event, to, nil, nil)
}

// AddTransitionWithGuard adds a transition with a guard.
func (sm *StateMachine) AddTransitionWithGuard(from State, event Event, to State, guard TransitionGuard) {
	sm.AddTransitionWithGuardAndAction(from, event, to, guard, nil)
}

// AddTransitionWithAction adds a transition with an action.
func (sm *StateMachine) AddTransitionWithAction(from State, event Event, to State, action TransitionAction) {
	sm.AddTransitionWithGuardAndAction(from, event, to, nil, action)
}

// AddTransitionWithGuardAndAction adds a transition with both guard and action.
func (sm *StateMachine) AddTransitionWithGuardAndAction(from State, event Event, to State, guard TransitionGuard, action TransitionAction) {
	sm.mu.Lock()
	defer sm.mu.Unlock()

	if sm.transitions[from] == nil {
		sm.transitions[from] = make(map[Event]Transition)
	}

	sm.transitions[from][event] = Transition{
		From:   from,
		To:     to,
		Event:  event,
		Guard:  guard,
		Action: action,
	}
}

// OnEnter sets a callback for entering a state.
func (sm *StateMachine) OnEnter(state State, callback func(State)) {
	sm.mu.Lock()
	defer sm.mu.Unlock()
	sm.onEnter[state] = callback
}

// OnExit sets a callback for exiting a state.
func (sm *StateMachine) OnExit(state State, callback func(State)) {
	sm.mu.Lock()
	defer sm.mu.Unlock()
	sm.onExit[state] = callback
}

// Send sends an event to the state machine.
// Returns true if a transition occurred.
func (sm *StateMachine) Send(event Event) bool {
	sm.mu.Lock()
	defer sm.mu.Unlock()

	stateTransitions, exists := sm.transitions[sm.currentState]
	if !exists {
		return false
	}

	transition, exists := stateTransitions[event]
	if !exists {
		return false
	}

	// Check guard
	if transition.Guard != nil && !transition.Guard() {
		return false
	}

	from := sm.currentState
	to := transition.To

	// Exit callback
	if exitFn, ok := sm.onExit[from]; ok {
		exitFn(to)
	}

	// Transition action
	if transition.Action != nil {
		transition.Action(from, to, event)
	}

	// Update state
	sm.currentState = to

	// Record history
	if len(sm.history) >= sm.maxHistory {
		sm.history = sm.history[1:]
	}
	sm.history = append(sm.history, StateHistoryEntry{
		From:  from,
		To:    to,
		Event: event,
	})

	// Enter callback
	if enterFn, ok := sm.onEnter[to]; ok {
		enterFn(from)
	}

	return true
}

// Can checks if an event can trigger a transition from the current state.
func (sm *StateMachine) Can(event Event) bool {
	sm.mu.RLock()
	defer sm.mu.RUnlock()

	stateTransitions, exists := sm.transitions[sm.currentState]
	if !exists {
		return false
	}

	transition, exists := stateTransitions[event]
	if !exists {
		return false
	}

	if transition.Guard != nil && !transition.Guard() {
		return false
	}

	return true
}

// Current returns the current state.
func (sm *StateMachine) Current() State {
	sm.mu.RLock()
	defer sm.mu.RUnlock()
	return sm.currentState
}

// Is checks if the machine is in a specific state.
func (sm *StateMachine) Is(state State) bool {
	sm.mu.RLock()
	defer sm.mu.RUnlock()
	return sm.currentState == state
}

// AvailableEvents returns events that can be triggered from the current state.
func (sm *StateMachine) AvailableEvents() []Event {
	sm.mu.RLock()
	defer sm.mu.RUnlock()

	events := make([]Event, 0)

	stateTransitions, exists := sm.transitions[sm.currentState]
	if !exists {
		return events
	}

	for event, transition := range stateTransitions {
		if transition.Guard == nil || transition.Guard() {
			events = append(events, event)
		}
	}

	return events
}

// History returns the transition history.
func (sm *StateMachine) History() []StateHistoryEntry {
	sm.mu.RLock()
	defer sm.mu.RUnlock()

	result := make([]StateHistoryEntry, len(sm.history))
	copy(result, sm.history)
	return result
}

// SetMaxHistory sets the maximum history size.
func (sm *StateMachine) SetMaxHistory(max int) {
	sm.mu.Lock()
	defer sm.mu.Unlock()

	if max < 0 {
		max = 0
	}
	sm.maxHistory = max

	if len(sm.history) > max {
		sm.history = sm.history[len(sm.history)-max:]
	}
}

// Reset resets the state machine to a specific state.
func (sm *StateMachine) Reset(state State) {
	sm.mu.Lock()
	defer sm.mu.Unlock()
	sm.currentState = state
	sm.history = make([]StateHistoryEntry, 0)
}

// StateMachineBuilder helps build state machines fluently.
type StateMachineBuilder struct {
	sm *StateMachine
}

// NewStateMachineBuilder creates a builder for a state machine.
func NewStateMachineBuilder(initialState State) *StateMachineBuilder {
	return &StateMachineBuilder{
		sm: NewStateMachine(initialState),
	}
}

// Transition adds a transition.
func (b *StateMachineBuilder) Transition(from State, event Event, to State) *StateMachineBuilder {
	b.sm.AddTransition(from, event, to)
	return b
}

// TransitionWithGuard adds a guarded transition.
func (b *StateMachineBuilder) TransitionWithGuard(from State, event Event, to State, guard TransitionGuard) *StateMachineBuilder {
	b.sm.AddTransitionWithGuard(from, event, to, guard)
	return b
}

// TransitionWithAction adds a transition with an action.
func (b *StateMachineBuilder) TransitionWithAction(from State, event Event, to State, action TransitionAction) *StateMachineBuilder {
	b.sm.AddTransitionWithAction(from, event, to, action)
	return b
}

// OnEnter sets an enter callback.
func (b *StateMachineBuilder) OnEnter(state State, callback func(State)) *StateMachineBuilder {
	b.sm.OnEnter(state, callback)
	return b
}

// OnExit sets an exit callback.
func (b *StateMachineBuilder) OnExit(state State, callback func(State)) *StateMachineBuilder {
	b.sm.OnExit(state, callback)
	return b
}

// Build returns the built state machine.
func (b *StateMachineBuilder) Build() *StateMachine {
	return b.sm
}
