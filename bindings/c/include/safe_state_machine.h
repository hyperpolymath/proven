/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_state_machine.h
 * @brief Type-safe state transitions
 *
 * Provides a state machine with explicit transition rules,
 * ensuring only valid state transitions are allowed.
 */

#ifndef SAFE_STATE_MACHINE_H
#define SAFE_STATE_MACHINE_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * State Machine Types
 * ============================================================================ */

/**
 * @brief State machine structure
 * @note Use proven_state_machine_create() to allocate, proven_state_machine_free() to deallocate
 */
typedef struct ProvenStateMachine {
    uint32_t current_state;     /**< Current state index */
    uint32_t state_count;       /**< Total number of states */
    uint8_t* transitions;       /**< Packed transition matrix (state_count x state_count) */
} ProvenStateMachine;

/* ============================================================================
 * State Machine Operations
 * ============================================================================ */

/**
 * @brief Create a state machine
 * @param state_count Total number of states (max 256)
 * @param initial_state Initial state index
 * @return State machine pointer, or NULL on failure
 *
 * Returns NULL if state_count is 0, exceeds 256, or initial_state >= state_count.
 * All transitions are initially invalid; use proven_state_machine_allow() to enable.
 */
ProvenStateMachine* proven_state_machine_create(uint32_t state_count, uint32_t initial_state);

/**
 * @brief Allow a state transition
 * @param sm State machine
 * @param from Source state index
 * @param to Destination state index
 * @return true if transition was allowed, false on error
 *
 * Returns false if sm is NULL or state indices are out of bounds.
 */
bool proven_state_machine_allow(ProvenStateMachine* sm, uint32_t from, uint32_t to);

/**
 * @brief Attempt to transition to a new state
 * @param sm State machine
 * @param to Destination state index
 * @return true if transition succeeded, false if not allowed
 *
 * Returns false if sm is NULL, to is out of bounds, or transition not allowed.
 */
bool proven_state_machine_transition(ProvenStateMachine* sm, uint32_t to);

/**
 * @brief Get current state
 * @param sm State machine
 * @return Current state index (0 if sm is NULL)
 */
uint32_t proven_state_machine_state(ProvenStateMachine* sm);

/**
 * @brief Free state machine
 * @param sm State machine to free (may be NULL)
 */
void proven_state_machine_free(ProvenStateMachine* sm);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_STATE_MACHINE_H */
