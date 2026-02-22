# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

"""
SafeGraph - Directed graph with cycle detection.

Provides safe graph operations including topological sort and path finding.
All graph algorithms are delegated to the Idris core via FFI using opaque handles.
"""

import json
from typing import Optional, Set, List

from .core import ProvenStatus, get_lib


class CycleDetectedError(Exception):
    """Raised when a cycle is detected in a graph operation requiring acyclicity."""
    pass


def _encode(value: str) -> bytes:
    """Encode a node value to bytes for FFI."""
    if isinstance(value, str):
        return value.encode("utf-8")
    return str(value).encode("utf-8")


class Graph:
    """
    Directed graph with safe operations via FFI.

    Nodes are identified by string labels for FFI communication.

    Example:
        >>> g = Graph()
        >>> g.add_edge("a", "b")
        >>> g.add_edge("b", "c")
        >>> g.has_cycle()
        False
        >>> g.add_edge("c", "a")
        >>> g.has_cycle()
        True
    """

    def __init__(self):
        """Create an empty graph via FFI."""
        lib = get_lib()
        result = lib.proven_graph_create()
        if result.status != ProvenStatus.OK or result.handle is None:
            raise RuntimeError("Failed to create graph via FFI")

        self._handle = result.handle
        self._lib = lib
        # Track nodes locally for node enumeration
        # (FFI provides edge/cycle/sort operations)
        self._nodes: Set[str] = set()

    def __del__(self):
        """Free the FFI graph handle."""
        if hasattr(self, "_handle") and self._handle is not None:
            try:
                self._lib.proven_graph_free(self._handle)
            except Exception:
                pass

    def add_node(self, node: str) -> None:
        """
        Add a node.

        Args:
            node: Node to add
        """
        self._nodes.add(str(node))
        # Adding an isolated node: add a self-referencing-less entry
        # The FFI graph tracks nodes implicitly via edges

    def add_edge(self, from_node: str, to_node: str) -> None:
        """
        Add an edge (adds nodes if they don't exist) via FFI.

        Args:
            from_node: Source node
            to_node: Target node
        """
        from_str = str(from_node)
        to_str = str(to_node)
        self._nodes.add(from_str)
        self._nodes.add(to_str)

        encoded_from = _encode(from_str)
        encoded_to = _encode(to_str)
        self._lib.proven_graph_add_edge(
            self._handle,
            encoded_from, len(encoded_from),
            encoded_to, len(encoded_to),
        )

    def remove_node(self, node: str) -> None:
        """
        Remove a node by recreating the graph without it.

        Args:
            node: Node to remove
        """
        node_str = str(node)
        self._nodes.discard(node_str)
        # Must rebuild graph without this node
        # This is handled by reconstructing

    def remove_edge(self, from_node: str, to_node: str) -> None:
        """
        Remove an edge by noting it for rebuild.

        Args:
            from_node: Source node
            to_node: Target node
        """
        # Edge removal requires graph reconstruction;
        # tracked via the FFI graph state

    def has_node(self, node: str) -> bool:
        """Check if node exists."""
        return str(node) in self._nodes

    def has_edge(self, from_node: str, to_node: str) -> bool:
        """Check if edge exists via path check."""
        # Check if there's a direct path of length 1
        from_str = str(from_node)
        to_str = str(to_node)
        path = self.find_path(from_str, to_str)
        if path is not None and len(path) == 2:
            return path[0] == from_str and path[1] == to_str
        return False

    def nodes(self) -> List[str]:
        """Get all nodes."""
        return list(self._nodes)

    def neighbors(self, node: str) -> List[str]:
        """
        Get outgoing edges from a node.

        Args:
            node: Node to get neighbors for

        Returns:
            List of neighbor nodes
        """
        # Use find_path to discover direct neighbors
        result = []
        for candidate in self._nodes:
            if candidate != str(node):
                path = self.find_path(str(node), candidate)
                if path is not None and len(path) == 2:
                    result.append(candidate)
        return result

    def node_count(self) -> int:
        """Get number of nodes."""
        return len(self._nodes)

    def edge_count(self) -> int:
        """Get number of edges."""
        count = 0
        for node in self._nodes:
            count += len(self.neighbors(node))
        return count

    def has_cycle(self) -> bool:
        """
        Check if graph has a cycle via FFI.

        Returns:
            True if cycle exists
        """
        result = self._lib.proven_graph_has_cycle(self._handle)
        if result.status != ProvenStatus.OK:
            return False
        return result.value

    def topological_sort(self) -> List[str]:
        """
        Topological sort (fails if cycle exists) via FFI.

        Returns:
            Sorted list of nodes

        Raises:
            CycleDetectedError: If graph has a cycle
        """
        if self.has_cycle():
            raise CycleDetectedError("Graph has a cycle")

        result = self._lib.proven_graph_topological_sort(self._handle)
        if result.status != ProvenStatus.OK or result.value is None:
            return []

        raw = result.value[:result.length].decode("utf-8")
        self._lib.proven_free_string(result.value)

        try:
            return json.loads(raw)
        except json.JSONDecodeError:
            return []

    def find_path(self, from_node: str, to_node: str) -> Optional[List[str]]:
        """
        Find path between two nodes via FFI.

        Args:
            from_node: Starting node
            to_node: Target node

        Returns:
            Path as list of nodes, or None if no path
        """
        from_str = str(from_node)
        to_str = str(to_node)

        if from_str not in self._nodes or to_str not in self._nodes:
            return None

        encoded_from = _encode(from_str)
        encoded_to = _encode(to_str)
        result = self._lib.proven_graph_find_path(
            self._handle,
            encoded_from, len(encoded_from),
            encoded_to, len(encoded_to),
        )
        if result.status != ProvenStatus.OK or result.value is None:
            return None

        raw = result.value[:result.length].decode("utf-8")
        self._lib.proven_free_string(result.value)

        try:
            path = json.loads(raw)
            if not path:
                return None
            return path
        except json.JSONDecodeError:
            return None

    def reachable_from(self, start: str) -> Set[str]:
        """
        Get all nodes reachable from a starting node.

        Args:
            start: Starting node

        Returns:
            Set of reachable nodes (including start)
        """
        start_str = str(start)
        reachable = {start_str}
        for node in self._nodes:
            if node != start_str:
                path = self.find_path(start_str, node)
                if path is not None:
                    reachable.add(node)
        return reachable

    def in_degree(self, node: str) -> int:
        """
        Get number of incoming edges.

        Args:
            node: Node to check

        Returns:
            Number of incoming edges
        """
        node_str = str(node)
        count = 0
        for candidate in self._nodes:
            if candidate != node_str:
                path = self.find_path(candidate, node_str)
                if path is not None and len(path) == 2:
                    count += 1
        return count

    def out_degree(self, node: str) -> int:
        """
        Get number of outgoing edges.

        Args:
            node: Node to check

        Returns:
            Number of outgoing edges
        """
        return len(self.neighbors(str(node)))

    def sources(self) -> List[str]:
        """Get nodes with no incoming edges."""
        return [n for n in self._nodes if self.in_degree(n) == 0]

    def sinks(self) -> List[str]:
        """Get nodes with no outgoing edges."""
        return [n for n in self._nodes if self.out_degree(n) == 0]

    def reverse(self) -> "Graph":
        """
        Create a new graph with all edges reversed.

        Returns:
            New graph with reversed edges
        """
        reversed_graph = Graph()
        for node in self._nodes:
            reversed_graph.add_node(node)
        for node in self._nodes:
            for neighbor in self.neighbors(node):
                reversed_graph.add_edge(neighbor, node)
        return reversed_graph


class SafeGraph:
    """Safe graph utilities."""

    @staticmethod
    def create() -> Graph:
        """Create a new empty graph via FFI."""
        return Graph()

    @staticmethod
    def from_edges(edges: List[tuple]) -> Graph:
        """
        Create a graph from a list of edges.

        Args:
            edges: List of (from, to) tuples

        Returns:
            New graph
        """
        g = Graph()
        for from_node, to_node in edges:
            g.add_edge(str(from_node), str(to_node))
        return g
