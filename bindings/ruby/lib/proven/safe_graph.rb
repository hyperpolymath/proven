# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Safe graph operations via FFI to libproven.
# ALL computation delegates to Idris 2 compiled code.
# Graphs are opaque pointers managed by libproven.

module Proven
  module SafeGraph
    class << self
      # Create a directed graph with the given node count.
      # Returns an opaque handle or nil on error.
      # Caller MUST call #free when done.
      #
      # @param node_count [Integer]
      # @return [Fiddle::Pointer, nil]
      def create(node_count)
        FFI.invoke_ptr(
          "proven_graph_create",
          [Fiddle::TYPE_SIZE_T],
          [node_count]
        )
      end

      # Add an edge from one node to another.
      # Returns true on success, nil on error.
      #
      # @param graph [Fiddle::Pointer]
      # @param from [Integer] source node index
      # @param to [Integer] target node index
      # @return [Boolean, nil]
      def add_edge(graph, from, to)
        return nil if graph.nil?
        status = FFI.invoke_i32(
          "proven_graph_add_edge",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T, Fiddle::TYPE_SIZE_T],
          [graph, from, to]
        )
        status == FFI::STATUS_OK
      end

      # Check if an edge exists between two nodes.
      #
      # @param graph [Fiddle::Pointer]
      # @param from [Integer]
      # @param to [Integer]
      # @return [Boolean, nil]
      def has_edge?(graph, from, to)
        return nil if graph.nil?
        FFI.invoke_bool(
          "proven_graph_has_edge",
          [Fiddle::TYPE_VOIDP, Fiddle::TYPE_SIZE_T, Fiddle::TYPE_SIZE_T],
          [graph, from, to]
        )
      end

      # Free a graph handle.
      #
      # @param graph [Fiddle::Pointer]
      def free(graph)
        return if graph.nil?
        FFI.invoke_void(
          "proven_graph_free",
          [Fiddle::TYPE_VOIDP],
          [graph]
        )
      end
    end
  end
end
