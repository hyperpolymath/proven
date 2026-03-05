-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-graphdb graph database server.
||| Defines closed sum types for graph elements, query languages,
||| traversal strategies, consistency levels, and error codes.
module Graphdb.Types

%default total

---------------------------------------------------------------------------
-- Element type: kinds of graph elements
---------------------------------------------------------------------------

||| Classification of a graph database element.
public export
data ElementType : Type where
  Node     : ElementType
  Edge     : ElementType
  Property : ElementType
  Label    : ElementType
  Index    : ElementType

export
Show ElementType where
  show Node     = "Node"
  show Edge     = "Edge"
  show Property = "Property"
  show Label    = "Label"
  show Index    = "Index"

---------------------------------------------------------------------------
-- Query language: supported graph query languages
---------------------------------------------------------------------------

||| Query languages supported by the graph database.
public export
data QueryLanguage : Type where
  Cypher  : QueryLanguage
  Gremlin : QueryLanguage
  SPARQL  : QueryLanguage
  GraphQL : QueryLanguage

export
Show QueryLanguage where
  show Cypher  = "Cypher"
  show Gremlin = "Gremlin"
  show SPARQL  = "SPARQL"
  show GraphQL = "GraphQL"

---------------------------------------------------------------------------
-- Traversal strategy: graph traversal algorithms
---------------------------------------------------------------------------

||| Algorithm used for graph traversal operations.
public export
data TraversalStrategy : Type where
  BFS      : TraversalStrategy
  DFS      : TraversalStrategy
  Dijkstra : TraversalStrategy
  AStar    : TraversalStrategy
  Random   : TraversalStrategy

export
Show TraversalStrategy where
  show BFS      = "BFS"
  show DFS      = "DFS"
  show Dijkstra = "Dijkstra"
  show AStar    = "A*"
  show Random   = "Random"

---------------------------------------------------------------------------
-- Consistency: consistency levels for distributed operations
---------------------------------------------------------------------------

||| Consistency level for distributed graph operations.
public export
data Consistency : Type where
  Strong   : Consistency
  Eventual : Consistency
  Session  : Consistency
  Causal   : Consistency

export
Show Consistency where
  show Strong   = "Strong"
  show Eventual = "Eventual"
  show Session  = "Session"
  show Causal   = "Causal"

---------------------------------------------------------------------------
-- Error code: graph database error codes
---------------------------------------------------------------------------

||| Error codes returned by the graph database server.
public export
data ErrorCode : Type where
  SyntaxError         : ErrorCode
  NodeNotFound        : ErrorCode
  EdgeNotFound        : ErrorCode
  ConstraintViolation : ErrorCode
  IndexExists         : ErrorCode
  TransactionConflict : ErrorCode
  OutOfMemory         : ErrorCode

export
Show ErrorCode where
  show SyntaxError         = "SyntaxError"
  show NodeNotFound        = "NodeNotFound"
  show EdgeNotFound        = "EdgeNotFound"
  show ConstraintViolation = "ConstraintViolation"
  show IndexExists         = "IndexExists"
  show TransactionConflict = "TransactionConflict"
  show OutOfMemory         = "OutOfMemory"
