// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//! Safe graph operations with cycle detection.
//!
//! Provides directed graph with safe operations including
//! cycle detection, topological sort, and path finding.

use crate::{Error, Result};
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;

/// Directed graph with safe operations.
#[derive(Debug, Clone)]
pub struct Graph<N: Clone + Eq + Hash> {
    nodes: HashSet<N>,
    edges: HashMap<N, HashSet<N>>,
}

impl<N: Clone + Eq + Hash> Graph<N> {
    /// Create an empty graph.
    pub fn new() -> Self {
        Self {
            nodes: HashSet::new(),
            edges: HashMap::new(),
        }
    }

    /// Add a node.
    pub fn add_node(&mut self, node: N) {
        self.nodes.insert(node);
    }

    /// Add an edge (adds nodes if they don't exist).
    pub fn add_edge(&mut self, from: N, to: N) {
        self.nodes.insert(from.clone());
        self.nodes.insert(to.clone());
        self.edges.entry(from).or_insert_with(HashSet::new).insert(to);
    }

    /// Remove a node and all its edges.
    pub fn remove_node(&mut self, node: &N) {
        self.nodes.remove(node);
        self.edges.remove(node);
        for edges in self.edges.values_mut() {
            edges.remove(node);
        }
    }

    /// Remove an edge.
    pub fn remove_edge(&mut self, from: &N, to: &N) {
        if let Some(edges) = self.edges.get_mut(from) {
            edges.remove(to);
        }
    }

    /// Check if node exists.
    pub fn has_node(&self, node: &N) -> bool {
        self.nodes.contains(node)
    }

    /// Check if edge exists.
    pub fn has_edge(&self, from: &N, to: &N) -> bool {
        self.edges
            .get(from)
            .map(|edges| edges.contains(to))
            .unwrap_or(false)
    }

    /// Get all nodes.
    pub fn nodes(&self) -> impl Iterator<Item = &N> {
        self.nodes.iter()
    }

    /// Get outgoing edges from a node.
    pub fn neighbors(&self, node: &N) -> Vec<&N> {
        self.edges
            .get(node)
            .map(|set| set.iter().collect())
            .unwrap_or_default()
    }

    /// Get node count.
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    /// Get edge count.
    pub fn edge_count(&self) -> usize {
        self.edges.values().map(|e| e.len()).sum()
    }

    /// Detect if graph has a cycle.
    pub fn has_cycle(&self) -> bool {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        for node in &self.nodes {
            if self.has_cycle_util(node, &mut visited, &mut rec_stack) {
                return true;
            }
        }
        false
    }

    fn has_cycle_util(
        &self,
        node: &N,
        visited: &mut HashSet<N>,
        rec_stack: &mut HashSet<N>,
    ) -> bool {
        if rec_stack.contains(node) {
            return true;
        }
        if visited.contains(node) {
            return false;
        }

        visited.insert(node.clone());
        rec_stack.insert(node.clone());

        if let Some(neighbors) = self.edges.get(node) {
            for neighbor in neighbors {
                if self.has_cycle_util(neighbor, visited, rec_stack) {
                    return true;
                }
            }
        }

        rec_stack.remove(node);
        false
    }

    /// Topological sort (fails if cycle exists).
    pub fn topological_sort(&self) -> Result<Vec<N>>
    where
        N: std::fmt::Debug,
    {
        if self.has_cycle() {
            return Err(Error::InvalidInput("Graph has a cycle".into()));
        }

        let mut in_degree: HashMap<N, usize> = HashMap::new();
        for node in &self.nodes {
            in_degree.insert(node.clone(), 0);
        }

        for edges in self.edges.values() {
            for to in edges {
                *in_degree.entry(to.clone()).or_insert(0) += 1;
            }
        }

        let mut queue: VecDeque<N> = in_degree
            .iter()
            .filter(|(_, &deg)| deg == 0)
            .map(|(n, _)| n.clone())
            .collect();

        let mut result = Vec::new();

        while let Some(node) = queue.pop_front() {
            result.push(node.clone());

            if let Some(neighbors) = self.edges.get(&node) {
                for neighbor in neighbors {
                    if let Some(deg) = in_degree.get_mut(neighbor) {
                        *deg -= 1;
                        if *deg == 0 {
                            queue.push_back(neighbor.clone());
                        }
                    }
                }
            }
        }

        Ok(result)
    }

    /// Find path between two nodes using BFS.
    pub fn find_path(&self, from: &N, to: &N) -> Option<Vec<N>> {
        if !self.has_node(from) || !self.has_node(to) {
            return None;
        }

        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();
        let mut parent: HashMap<N, N> = HashMap::new();

        queue.push_back(from.clone());
        visited.insert(from.clone());

        while let Some(current) = queue.pop_front() {
            if &current == to {
                // Reconstruct path
                let mut path = vec![current.clone()];
                let mut node = &current;
                while let Some(p) = parent.get(node) {
                    path.push(p.clone());
                    node = p;
                }
                path.reverse();
                return Some(path);
            }

            if let Some(neighbors) = self.edges.get(&current) {
                for neighbor in neighbors {
                    if !visited.contains(neighbor) {
                        visited.insert(neighbor.clone());
                        parent.insert(neighbor.clone(), current.clone());
                        queue.push_back(neighbor.clone());
                    }
                }
            }
        }

        None
    }

    /// Get all reachable nodes from a starting node.
    pub fn reachable_from(&self, start: &N) -> HashSet<N> {
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();

        queue.push_back(start.clone());

        while let Some(node) = queue.pop_front() {
            if visited.contains(&node) {
                continue;
            }
            visited.insert(node.clone());

            if let Some(neighbors) = self.edges.get(&node) {
                for neighbor in neighbors {
                    if !visited.contains(neighbor) {
                        queue.push_back(neighbor.clone());
                    }
                }
            }
        }

        visited
    }
}

impl<N: Clone + Eq + Hash> Default for Graph<N> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_graph() {
        let mut g: Graph<&str> = Graph::new();
        g.add_edge("a", "b");
        g.add_edge("b", "c");

        assert!(g.has_node(&"a"));
        assert!(g.has_edge(&"a", &"b"));
        assert!(!g.has_edge(&"a", &"c"));
        assert_eq!(g.node_count(), 3);
        assert_eq!(g.edge_count(), 2);
    }

    #[test]
    fn test_cycle_detection() {
        let mut g: Graph<i32> = Graph::new();
        g.add_edge(1, 2);
        g.add_edge(2, 3);
        assert!(!g.has_cycle());

        g.add_edge(3, 1);
        assert!(g.has_cycle());
    }

    #[test]
    fn test_topological_sort() {
        let mut g: Graph<&str> = Graph::new();
        g.add_edge("a", "b");
        g.add_edge("a", "c");
        g.add_edge("b", "d");
        g.add_edge("c", "d");

        let sorted = g.topological_sort().unwrap();
        assert_eq!(sorted[0], "a");
        assert_eq!(sorted[sorted.len() - 1], "d");
    }

    #[test]
    fn test_find_path() {
        let mut g: Graph<i32> = Graph::new();
        g.add_edge(1, 2);
        g.add_edge(2, 3);
        g.add_edge(1, 3);

        let path = g.find_path(&1, &3).unwrap();
        assert_eq!(path[0], 1);
        assert_eq!(path[path.len() - 1], 3);
    }
}
