# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

"""
SafeGraph - Directed graph with cycle detection.

Provides safe graph operations including topological sort and path finding.
"""

from typing import TypeVar, Generic, Optional, Set, Dict, List
from collections import deque

N = TypeVar("N")


class CycleDetectedError(Exception):
    """Raised when a cycle is detected in a graph operation requiring acyclicity."""
    pass


class Graph(Generic[N]):
    """
    Directed graph with safe operations.

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
        """Create an empty graph."""
        self._nodes: Set[N] = set()
        self._edges: Dict[N, Set[N]] = {}

    def add_node(self, node: N) -> None:
        """
        Add a node.

        Args:
            node: Node to add
        """
        self._nodes.add(node)

    def add_edge(self, from_node: N, to_node: N) -> None:
        """
        Add an edge (adds nodes if they don't exist).

        Args:
            from_node: Source node
            to_node: Target node
        """
        self._nodes.add(from_node)
        self._nodes.add(to_node)
        if from_node not in self._edges:
            self._edges[from_node] = set()
        self._edges[from_node].add(to_node)

    def remove_node(self, node: N) -> None:
        """
        Remove a node and all its edges.

        Args:
            node: Node to remove
        """
        self._nodes.discard(node)
        self._edges.pop(node, None)
        for edges in self._edges.values():
            edges.discard(node)

    def remove_edge(self, from_node: N, to_node: N) -> None:
        """
        Remove an edge.

        Args:
            from_node: Source node
            to_node: Target node
        """
        if from_node in self._edges:
            self._edges[from_node].discard(to_node)

    def has_node(self, node: N) -> bool:
        """Check if node exists."""
        return node in self._nodes

    def has_edge(self, from_node: N, to_node: N) -> bool:
        """Check if edge exists."""
        return from_node in self._edges and to_node in self._edges[from_node]

    def nodes(self) -> List[N]:
        """Get all nodes."""
        return list(self._nodes)

    def neighbors(self, node: N) -> List[N]:
        """
        Get outgoing edges from a node.

        Args:
            node: Node to get neighbors for

        Returns:
            List of neighbor nodes
        """
        return list(self._edges.get(node, set()))

    def node_count(self) -> int:
        """Get number of nodes."""
        return len(self._nodes)

    def edge_count(self) -> int:
        """Get number of edges."""
        return sum(len(edges) for edges in self._edges.values())

    def has_cycle(self) -> bool:
        """
        Check if graph has a cycle.

        Returns:
            True if cycle exists
        """
        visited: Set[N] = set()
        rec_stack: Set[N] = set()

        def dfs(node: N) -> bool:
            if node in rec_stack:
                return True
            if node in visited:
                return False

            visited.add(node)
            rec_stack.add(node)

            for neighbor in self._edges.get(node, set()):
                if dfs(neighbor):
                    return True

            rec_stack.remove(node)
            return False

        for node in self._nodes:
            if dfs(node):
                return True
        return False

    def topological_sort(self) -> List[N]:
        """
        Topological sort (fails if cycle exists).

        Returns:
            Sorted list of nodes

        Raises:
            CycleDetectedError: If graph has a cycle
        """
        if self.has_cycle():
            raise CycleDetectedError("Graph has a cycle")

        # Calculate in-degrees
        in_degree: Dict[N, int] = {node: 0 for node in self._nodes}
        for edges in self._edges.values():
            for to_node in edges:
                in_degree[to_node] = in_degree.get(to_node, 0) + 1

        # Start with nodes that have no incoming edges
        queue = deque([node for node, deg in in_degree.items() if deg == 0])
        result = []

        while queue:
            node = queue.popleft()
            result.append(node)

            for neighbor in self._edges.get(node, set()):
                in_degree[neighbor] -= 1
                if in_degree[neighbor] == 0:
                    queue.append(neighbor)

        return result

    def find_path(self, from_node: N, to_node: N) -> Optional[List[N]]:
        """
        Find path between two nodes using BFS.

        Args:
            from_node: Starting node
            to_node: Target node

        Returns:
            Path as list of nodes, or None if no path
        """
        if from_node not in self._nodes or to_node not in self._nodes:
            return None

        visited: Set[N] = set()
        queue = deque([(from_node, [from_node])])

        while queue:
            current, path = queue.popleft()

            if current == to_node:
                return path

            if current in visited:
                continue
            visited.add(current)

            for neighbor in self._edges.get(current, set()):
                if neighbor not in visited:
                    queue.append((neighbor, path + [neighbor]))

        return None

    def reachable_from(self, start: N) -> Set[N]:
        """
        Get all nodes reachable from a starting node.

        Args:
            start: Starting node

        Returns:
            Set of reachable nodes (including start)
        """
        visited: Set[N] = set()
        queue = deque([start])

        while queue:
            node = queue.popleft()
            if node in visited:
                continue
            visited.add(node)

            for neighbor in self._edges.get(node, set()):
                if neighbor not in visited:
                    queue.append(neighbor)

        return visited

    def in_degree(self, node: N) -> int:
        """
        Get number of incoming edges.

        Args:
            node: Node to check

        Returns:
            Number of incoming edges
        """
        count = 0
        for edges in self._edges.values():
            if node in edges:
                count += 1
        return count

    def out_degree(self, node: N) -> int:
        """
        Get number of outgoing edges.

        Args:
            node: Node to check

        Returns:
            Number of outgoing edges
        """
        return len(self._edges.get(node, set()))

    def sources(self) -> List[N]:
        """Get nodes with no incoming edges."""
        in_degrees = {node: 0 for node in self._nodes}
        for edges in self._edges.values():
            for to_node in edges:
                in_degrees[to_node] = in_degrees.get(to_node, 0) + 1
        return [node for node, deg in in_degrees.items() if deg == 0]

    def sinks(self) -> List[N]:
        """Get nodes with no outgoing edges."""
        return [node for node in self._nodes if not self._edges.get(node)]

    def reverse(self) -> "Graph[N]":
        """
        Create a new graph with all edges reversed.

        Returns:
            New graph with reversed edges
        """
        reversed_graph: Graph[N] = Graph()
        for node in self._nodes:
            reversed_graph.add_node(node)
        for from_node, edges in self._edges.items():
            for to_node in edges:
                reversed_graph.add_edge(to_node, from_node)
        return reversed_graph


class SafeGraph:
    """Safe graph utilities."""

    @staticmethod
    def create() -> Graph:
        """Create a new empty graph."""
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
        g: Graph = Graph()
        for from_node, to_node in edges:
            g.add_edge(from_node, to_node)
        return g
