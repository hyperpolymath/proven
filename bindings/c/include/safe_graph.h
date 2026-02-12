/* SPDX-License-Identifier: Apache-2.0 */
/* SPDX-FileCopyrightText: 2025 Hyperpolymath */

/**
 * @file safe_graph.h
 * @brief Graph operations with bounds checking
 *
 * Provides a directed graph implementation using an adjacency
 * matrix with safe edge operations.
 */

#ifndef SAFE_GRAPH_H
#define SAFE_GRAPH_H

#include "proven_types.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Graph Types
 * ============================================================================ */

/**
 * @brief Graph structure (adjacency matrix)
 * @note Use proven_graph_create() to allocate, proven_graph_free() to deallocate
 */
typedef struct ProvenGraph {
    uint8_t* edges;     /**< Packed adjacency matrix (bit per edge) */
    size_t node_count;
} ProvenGraph;

/* ============================================================================
 * Graph Operations
 * ============================================================================ */

/**
 * @brief Create a graph
 * @param node_count Number of nodes (max 10,000)
 * @return Graph pointer, or NULL on failure
 */
ProvenGraph* proven_graph_create(size_t node_count);

/**
 * @brief Add directed edge
 * @param graph Graph
 * @param from Source node index
 * @param to Destination node index
 * @return Status code
 *
 * Returns PROVEN_ERR_NULL_POINTER if graph is NULL.
 * Returns PROVEN_ERR_OUT_OF_BOUNDS if node indices are invalid.
 */
ProvenStatus proven_graph_add_edge(ProvenGraph* graph, size_t from, size_t to);

/**
 * @brief Check if edge exists
 * @param graph Graph
 * @param from Source node index
 * @param to Destination node index
 * @return true if edge exists, false otherwise (or on error)
 */
bool proven_graph_has_edge(ProvenGraph* graph, size_t from, size_t to);

/**
 * @brief Free graph
 * @param graph Graph to free (may be NULL)
 */
void proven_graph_free(ProvenGraph* graph);

#ifdef __cplusplus
}
#endif

#endif /* SAFE_GRAPH_H */
