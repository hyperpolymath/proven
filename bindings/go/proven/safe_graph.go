// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeGraph provides graph operations with bounds checking via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"

// GraphHandle is a handle to a graph allocated in the FFI layer.
type GraphHandle struct {
	ptr *C.Graph
}

// GraphCreate creates a new directed graph with the given number of nodes.
func GraphCreate(nodeCount int) (*GraphHandle, error) {
	ptr := C.proven_graph_create(C.size_t(nodeCount))
	if ptr == nil {
		return nil, newError(StatusErrAllocFailed)
	}
	return &GraphHandle{ptr: ptr}, nil
}

// AddEdge adds a directed edge from one node to another.
func (g *GraphHandle) AddEdge(from, to int) error {
	status := int(C.proven_graph_add_edge(g.ptr, C.size_t(from), C.size_t(to)))
	if status != StatusOK {
		return newError(status)
	}
	return nil
}

// HasEdge checks whether a directed edge exists from one node to another.
func (g *GraphHandle) HasEdge(from, to int) bool {
	return bool(C.proven_graph_has_edge(g.ptr, C.size_t(from), C.size_t(to)))
}

// Free releases the graph's memory.
func (g *GraphHandle) Free() {
	if g.ptr != nil {
		C.proven_graph_free(g.ptr)
		g.ptr = nil
	}
}
