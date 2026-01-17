# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

require "set"

module Proven
  # Safe graph operations with cycle detection.
  #
  # Provides directed graph with safe operations including
  # cycle detection, topological sort, and path finding.
  module SafeGraph
    # Directed graph with safe operations.
    class Graph
      def initialize
        @nodes = Set.new
        @edges = {}
      end

      # Add a node.
      #
      # @param node [Object]
      def add_node(node)
        @nodes.add(node)
      end

      # Add an edge (adds nodes if they don't exist).
      #
      # @param from [Object]
      # @param to [Object]
      def add_edge(from, to)
        @nodes.add(from)
        @nodes.add(to)
        @edges[from] ||= Set.new
        @edges[from].add(to)
      end

      # Remove a node and all its edges.
      #
      # @param node [Object]
      def remove_node(node)
        @nodes.delete(node)
        @edges.delete(node)
        @edges.each_value { |edges| edges.delete(node) }
      end

      # Remove an edge.
      #
      # @param from [Object]
      # @param to [Object]
      def remove_edge(from, to)
        @edges[from]&.delete(to)
      end

      # Check if node exists.
      #
      # @param node [Object]
      # @return [Boolean]
      def has_node?(node)
        @nodes.include?(node)
      end

      # Check if edge exists.
      #
      # @param from [Object]
      # @param to [Object]
      # @return [Boolean]
      def has_edge?(from, to)
        @edges[from]&.include?(to) || false
      end

      # Get all nodes.
      #
      # @return [Array]
      def nodes
        @nodes.to_a
      end

      # Get outgoing edges from a node.
      #
      # @param node [Object]
      # @return [Array]
      def neighbors(node)
        @edges[node]&.to_a || []
      end

      # Get node count.
      #
      # @return [Integer]
      def node_count
        @nodes.size
      end

      # Get edge count.
      #
      # @return [Integer]
      def edge_count
        @edges.values.sum(&:size)
      end

      # Detect if graph has a cycle.
      #
      # @return [Boolean]
      def has_cycle?
        visited = Set.new
        rec_stack = Set.new

        @nodes.each do |node|
          return true if has_cycle_util(node, visited, rec_stack)
        end
        false
      end

      # Topological sort (fails if cycle exists).
      #
      # @return [Result]
      def topological_sort
        return Result.error(InvalidInputError.new("Graph has a cycle")) if has_cycle?

        in_degree = {}
        @nodes.each { |node| in_degree[node] = 0 }

        @edges.each_value do |targets|
          targets.each do |target|
            in_degree[target] ||= 0
            in_degree[target] += 1
          end
        end

        queue = in_degree.select { |_, deg| deg.zero? }.keys
        result = []

        until queue.empty?
          node = queue.shift
          result.push(node)

          neighbors(node).each do |neighbor|
            in_degree[neighbor] -= 1
            queue.push(neighbor) if in_degree[neighbor].zero?
          end
        end

        Result.ok(result)
      end

      # Find path between two nodes using BFS.
      #
      # @param from [Object]
      # @param to [Object]
      # @return [Array, nil]
      def find_path(from, to)
        return nil unless has_node?(from) && has_node?(to)

        visited = Set.new
        queue = [from]
        parent = {}

        visited.add(from)

        until queue.empty?
          current = queue.shift

          if current == to
            # Reconstruct path
            path = [current]
            node = current
            while parent.key?(node)
              path.unshift(parent[node])
              node = parent[node]
            end
            return path
          end

          neighbors(current).each do |neighbor|
            next if visited.include?(neighbor)

            visited.add(neighbor)
            parent[neighbor] = current
            queue.push(neighbor)
          end
        end

        nil
      end

      # Get all reachable nodes from a starting node.
      #
      # @param start [Object]
      # @return [Set]
      def reachable_from(start)
        visited = Set.new
        queue = [start]

        until queue.empty?
          node = queue.shift
          next if visited.include?(node)

          visited.add(node)
          neighbors(node).each do |neighbor|
            queue.push(neighbor) unless visited.include?(neighbor)
          end
        end

        visited
      end

      # Get reverse graph (all edges reversed).
      #
      # @return [Graph]
      def reverse
        reversed = Graph.new
        @nodes.each { |node| reversed.add_node(node) }
        @edges.each do |from, targets|
          targets.each { |to| reversed.add_edge(to, from) }
        end
        reversed
      end

      # Get strongly connected components using Kosaraju's algorithm.
      #
      # @return [Array<Array>]
      def strongly_connected_components
        visited = Set.new
        stack = []

        # First DFS to fill stack
        @nodes.each do |node|
          fill_order(node, visited, stack) unless visited.include?(node)
        end

        # Get reverse graph
        rev = reverse

        # Second DFS on reverse graph
        visited.clear
        components = []

        until stack.empty?
          node = stack.pop
          next if visited.include?(node)

          component = []
          rev.dfs_collect(node, visited, component)
          components.push(component)
        end

        components
      end

      protected

      def dfs_collect(node, visited, component)
        return if visited.include?(node)

        visited.add(node)
        component.push(node)
        neighbors(node).each { |neighbor| dfs_collect(neighbor, visited, component) }
      end

      private

      def has_cycle_util(node, visited, rec_stack)
        return true if rec_stack.include?(node)
        return false if visited.include?(node)

        visited.add(node)
        rec_stack.add(node)

        neighbors(node).each do |neighbor|
          return true if has_cycle_util(neighbor, visited, rec_stack)
        end

        rec_stack.delete(node)
        false
      end

      def fill_order(node, visited, stack)
        return if visited.include?(node)

        visited.add(node)
        neighbors(node).each { |neighbor| fill_order(neighbor, visited, stack) }
        stack.push(node)
      end
    end

    # Create a new directed graph.
    #
    # @return [Graph]
    def self.graph
      Graph.new
    end
  end
end
