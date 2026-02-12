// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Guard function type for transitions.
public typealias Guard<C> = (C) -> Bool

/// Action function type for transitions.
public typealias Action<C> = (inout C) -> Void

/// Transition definition.
public struct Transition<S: Hashable, E: Hashable, C> {
    public let from: S
    public let event: E
    public let to: S
    public let guard_: Guard<C>?
    public let action: Action<C>?

    public init(from: S, event: E, to: S, guard_: Guard<C>? = nil, action: Action<C>? = nil) {
        self.from = from
        self.event = event
        self.to = to
        self.guard_ = guard_
        self.action = action
    }
}

/// Transition result.
public struct TransitionResult<S: Hashable> {
    public let success: Bool
    public let previousState: S
    public let currentState: S
    public let error: String?
}

/// History entry.
public struct HistoryEntry<S: Hashable, E: Hashable> {
    public let state: S
    public let event: E?
    public let timestamp: Double
}

/// Type-safe finite state machine.
public class StateMachine<S: Hashable, E: Hashable, C> {
    private var currentState: S
    private var context: C
    private var transitions: [String: [Transition<S, E, C>]] = [:]
    private var onEnter: [S: Action<C>] = [:]
    private var onExit: [S: Action<C>] = [:]
    private var history: [HistoryEntry<S, E>] = []
    private let maxHistorySize: Int
    private let lock = NSLock()

    public init(initial: S, context: C, maxHistorySize: Int = 100) {
        self.currentState = initial
        self.context = context
        self.maxHistorySize = maxHistorySize
        self.history.append(HistoryEntry(state: initial, event: nil, timestamp: Date().timeIntervalSince1970 * 1000))
    }

    /// Add a transition.
    public func addTransition(_ transition: Transition<S, E, C>) {
        lock.lock()
        defer { lock.unlock() }

        let key = "\(transition.from):\(transition.event)"
        if transitions[key] == nil {
            transitions[key] = []
        }
        transitions[key]!.append(transition)
    }

    /// Set on-enter callback for a state.
    public func setOnEnter(_ state: S, action: @escaping Action<C>) {
        lock.lock()
        defer { lock.unlock() }
        onEnter[state] = action
    }

    /// Set on-exit callback for a state.
    public func setOnExit(_ state: S, action: @escaping Action<C>) {
        lock.lock()
        defer { lock.unlock() }
        onExit[state] = action
    }

    /// Get current state.
    public func getState() -> S {
        lock.lock()
        defer { lock.unlock() }
        return currentState
    }

    /// Get context.
    public func getContext() -> C {
        lock.lock()
        defer { lock.unlock() }
        return context
    }

    /// Update context.
    public func updateContext(_ update: (inout C) -> Void) {
        lock.lock()
        defer { lock.unlock() }
        update(&context)
    }

    /// Check if an event can be processed in the current state.
    public func canHandle(_ event: E) -> Bool {
        lock.lock()
        defer { lock.unlock() }

        let key = "\(currentState):\(event)"
        guard let available = transitions[key] else { return false }

        return available.contains { t in
            t.guard_ == nil || t.guard_!(context)
        }
    }

    /// Get available events from current state.
    public func availableEvents() -> [E] {
        lock.lock()
        defer { lock.unlock() }

        var events: Set<E> = []
        for (key, trans) in transitions {
            let parts = key.split(separator: ":")
            if parts.count == 2 && String(parts[0]) == String(describing: currentState) {
                for t in trans {
                    if t.guard_ == nil || t.guard_!(context) {
                        events.insert(t.event)
                    }
                }
            }
        }
        return Array(events)
    }

    /// Send an event to the state machine.
    public func send(_ event: E) -> TransitionResult<S> {
        lock.lock()
        defer { lock.unlock() }

        let previousState = currentState
        let key = "\(currentState):\(event)"

        guard let available = transitions[key], !available.isEmpty else {
            return TransitionResult(
                success: false,
                previousState: previousState,
                currentState: currentState,
                error: "No transition from '\(currentState)' for event '\(event)'"
            )
        }

        // Find first transition whose guard passes
        guard let transition = available.first(where: { t in t.guard_ == nil || t.guard_!(context) }) else {
            return TransitionResult(
                success: false,
                previousState: previousState,
                currentState: currentState,
                error: "Guard condition failed for transition from '\(currentState)' on '\(event)'"
            )
        }

        // Execute exit action
        if let exitAction = onExit[currentState] {
            exitAction(&context)
        }

        // Execute transition action
        if let transitionAction = transition.action {
            transitionAction(&context)
        }

        // Update state
        currentState = transition.to

        // Execute enter action
        if let enterAction = onEnter[currentState] {
            enterAction(&context)
        }

        // Record history
        history.append(HistoryEntry(state: currentState, event: event, timestamp: Date().timeIntervalSince1970 * 1000))
        if history.count > maxHistorySize {
            history.removeFirst()
        }

        return TransitionResult(
            success: true,
            previousState: previousState,
            currentState: currentState,
            error: nil
        )
    }

    /// Check if in a specific state.
    public func `is`(_ state: S) -> Bool {
        lock.lock()
        defer { lock.unlock() }
        return currentState == state
    }

    /// Check if in any of the given states.
    public func isAny(_ states: [S]) -> Bool {
        lock.lock()
        defer { lock.unlock() }
        return states.contains(currentState)
    }

    /// Get state history.
    public func getHistory() -> [HistoryEntry<S, E>] {
        lock.lock()
        defer { lock.unlock() }
        return history
    }

    /// Reset to initial state.
    public func reset(to state: S, context: C? = nil) {
        lock.lock()
        defer { lock.unlock() }

        currentState = state
        if let ctx = context {
            self.context = ctx
        }
        history = [HistoryEntry(state: state, event: nil, timestamp: Date().timeIntervalSince1970 * 1000)]
    }
}

/// State machine builder for fluent configuration.
public class StateMachineBuilder<S: Hashable, E: Hashable, C> {
    private var initial: S?
    private var context: C?
    private var transitions: [Transition<S, E, C>] = []
    private var onEnterCallbacks: [S: Action<C>] = [:]
    private var onExitCallbacks: [S: Action<C>] = [:]

    public init() {}

    public func withInitial(_ state: S) -> StateMachineBuilder {
        initial = state
        return self
    }

    public func withContext(_ context: C) -> StateMachineBuilder {
        self.context = context
        return self
    }

    public func addTransition(from: S, event: E, to: S, guard_: Guard<C>? = nil, action: Action<C>? = nil) -> StateMachineBuilder {
        transitions.append(Transition(from: from, event: event, to: to, guard_: guard_, action: action))
        return self
    }

    public func onEnter(_ state: S, action: @escaping Action<C>) -> StateMachineBuilder {
        onEnterCallbacks[state] = action
        return self
    }

    public func onExit(_ state: S, action: @escaping Action<C>) -> StateMachineBuilder {
        onExitCallbacks[state] = action
        return self
    }

    public func build() -> StateMachine<S, E, C>? {
        guard let initial = initial, let context = context else { return nil }
        guard !transitions.isEmpty else { return nil }

        let machine = StateMachine<S, E, C>(initial: initial, context: context)

        for transition in transitions {
            machine.addTransition(transition)
        }

        for (state, action) in onEnterCallbacks {
            machine.setOnEnter(state, action: action)
        }

        for (state, action) in onExitCallbacks {
            machine.setOnExit(state, action: action)
        }

        return machine
    }
}
