// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

/**
 * Type alias for guard function.
 */
typealias Guard<C> = (C) -> Boolean

/**
 * Type alias for action function.
 */
typealias Action<C> = (C) -> Unit

/**
 * Transition definition.
 */
data class Transition<S, E, C>(
    val from: S,
    val event: E,
    val to: S,
    val guard: Guard<C>? = null,
    val action: Action<C>? = null
)

/**
 * Transition result.
 */
data class TransitionResult<S>(
    val success: Boolean,
    val previousState: S,
    val currentState: S,
    val error: String? = null
)

/**
 * History entry.
 */
data class HistoryEntry<S, E>(
    val state: S,
    val event: E?,
    val timestamp: Long
)

/**
 * Type-safe finite state machine.
 */
class StateMachine<S, E, C>(
    initial: S,
    initialContext: C,
    private val maxHistorySize: Int = 100
) {
    private val lock = ReentrantLock()
    private var currentState: S = initial
    private var context: C = initialContext
    private val transitions = mutableMapOf<Pair<S, E>, MutableList<Transition<S, E, C>>>()
    private val onEnter = mutableMapOf<S, Action<C>>()
    private val onExit = mutableMapOf<S, Action<C>>()
    private val history = mutableListOf<HistoryEntry<S, E>>()

    init {
        history.add(HistoryEntry(initial, null, System.currentTimeMillis()))
    }

    /**
     * Add a transition.
     */
    fun addTransition(transition: Transition<S, E, C>) = lock.withLock {
        val key = transition.from to transition.event
        transitions.getOrPut(key) { mutableListOf() }.add(transition)
    }

    /**
     * Add multiple transitions.
     */
    fun addTransitions(vararg transitionList: Transition<S, E, C>) {
        transitionList.forEach { addTransition(it) }
    }

    /**
     * Set on-enter callback for a state.
     */
    fun setOnEnter(state: S, action: Action<C>) = lock.withLock {
        onEnter[state] = action
    }

    /**
     * Set on-exit callback for a state.
     */
    fun setOnExit(state: S, action: Action<C>) = lock.withLock {
        onExit[state] = action
    }

    /**
     * Get current state.
     */
    fun getState(): S = lock.withLock { currentState }

    /**
     * Get context.
     */
    fun getContext(): C = lock.withLock { context }

    /**
     * Update context.
     */
    fun updateContext(update: (C) -> C) = lock.withLock {
        context = update(context)
    }

    /**
     * Check if an event can be processed.
     */
    fun canHandle(event: E): Boolean = lock.withLock {
        val key = currentState to event
        val available = transitions[key] ?: return false
        available.any { it.guard == null || it.guard.invoke(context) }
    }

    /**
     * Get available events from current state.
     */
    fun availableEvents(): List<E> = lock.withLock {
        transitions.entries
            .filter { (key, trans) ->
                key.first == currentState &&
                        trans.any { it.guard == null || it.guard.invoke(context) }
            }
            .map { it.key.second }
    }

    /**
     * Send an event to the state machine.
     */
    fun send(event: E): TransitionResult<S> = lock.withLock {
        val previousState = currentState
        val key = currentState to event

        val available = transitions[key]
        if (available.isNullOrEmpty()) {
            return TransitionResult(
                success = false,
                previousState = previousState,
                currentState = currentState,
                error = "No transition from '$currentState' for event '$event'"
            )
        }

        val transition = available.find { it.guard == null || it.guard.invoke(context) }
        if (transition == null) {
            return TransitionResult(
                success = false,
                previousState = previousState,
                currentState = currentState,
                error = "Guard condition failed for transition from '$currentState' on '$event'"
            )
        }

        // Execute exit action
        onExit[currentState]?.invoke(context)

        // Execute transition action
        transition.action?.invoke(context)

        // Update state
        currentState = transition.to

        // Execute enter action
        onEnter[currentState]?.invoke(context)

        // Record history
        history.add(HistoryEntry(currentState, event, System.currentTimeMillis()))
        if (history.size > maxHistorySize) {
            history.removeAt(0)
        }

        TransitionResult(
            success = true,
            previousState = previousState,
            currentState = currentState
        )
    }

    /**
     * Check if in specific state.
     */
    fun isIn(state: S): Boolean = lock.withLock { currentState == state }

    /**
     * Check if in any of the given states.
     */
    fun isInAny(vararg states: S): Boolean = lock.withLock { currentState in states }

    /**
     * Get state history.
     */
    fun getHistory(): List<HistoryEntry<S, E>> = lock.withLock { history.toList() }

    /**
     * Reset to a state.
     */
    fun reset(state: S, newContext: C? = null) = lock.withLock {
        currentState = state
        if (newContext != null) context = newContext
        history.clear()
        history.add(HistoryEntry(state, null, System.currentTimeMillis()))
    }
}

/**
 * State machine builder for fluent configuration.
 */
class StateMachineBuilder<S, E, C> {
    private var initial: S? = null
    private var initialContext: C? = null
    private val transitions = mutableListOf<Transition<S, E, C>>()
    private val onEnterCallbacks = mutableMapOf<S, Action<C>>()
    private val onExitCallbacks = mutableMapOf<S, Action<C>>()

    fun initial(state: S) = apply { initial = state }
    fun context(ctx: C) = apply { initialContext = ctx }

    fun transition(
        from: S,
        event: E,
        to: S,
        guard: Guard<C>? = null,
        action: Action<C>? = null
    ) = apply {
        transitions.add(Transition(from, event, to, guard, action))
    }

    fun onEnter(state: S, action: Action<C>) = apply {
        onEnterCallbacks[state] = action
    }

    fun onExit(state: S, action: Action<C>) = apply {
        onExitCallbacks[state] = action
    }

    fun build(): StateMachine<S, E, C>? {
        val init = initial ?: return null
        val ctx = initialContext ?: return null
        if (transitions.isEmpty()) return null

        val machine = StateMachine(init, ctx)
        transitions.forEach { machine.addTransition(it) }
        onEnterCallbacks.forEach { (state, action) -> machine.setOnEnter(state, action) }
        onExitCallbacks.forEach { (state, action) -> machine.setOnExit(state, action) }

        return machine
    }
}
