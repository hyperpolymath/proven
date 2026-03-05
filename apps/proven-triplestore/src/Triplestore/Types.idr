-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-triplestore RDF triple store server.
||| Defines closed sum types for statement types, index orderings,
||| storage backends, import formats, and transaction isolation levels.
module Triplestore.Types

%default total

---------------------------------------------------------------------------
-- Statement: RDF statement types
---------------------------------------------------------------------------

||| An RDF statement is either a triple (S,P,O) or a quad (G,S,P,O).
public export
data Statement : Type where
  ||| An RDF triple: subject, predicate, object.
  Triple : Statement
  ||| An RDF quad: graph, subject, predicate, object.
  Quad   : Statement

export
Show Statement where
  show Triple = "Triple"
  show Quad   = "Quad"

---------------------------------------------------------------------------
-- Index order: index orderings for triple/quad lookup
---------------------------------------------------------------------------

||| Index ordering for efficient triple/quad pattern lookups.
public export
data IndexOrder : Type where
  ||| Subject-Predicate-Object.
  SPO  : IndexOrder
  ||| Predicate-Object-Subject.
  POS  : IndexOrder
  ||| Object-Subject-Predicate.
  OSP  : IndexOrder
  ||| Graph-Subject-Predicate-Object.
  GSPO : IndexOrder
  ||| Graph-Predicate-Object-Subject.
  GPOS : IndexOrder
  ||| Graph-Object-Subject-Predicate.
  GOSP : IndexOrder

export
Show IndexOrder where
  show SPO  = "SPO"
  show POS  = "POS"
  show OSP  = "OSP"
  show GSPO = "GSPO"
  show GPOS = "GPOS"
  show GOSP = "GOSP"

---------------------------------------------------------------------------
-- Storage backend: persistent storage engines
---------------------------------------------------------------------------

||| Storage backend for the triple store.
public export
data StorageBackend : Type where
  ||| In-memory store (volatile).
  InMemory   : StorageBackend
  ||| B-tree based persistent store.
  BTree      : StorageBackend
  ||| Log-Structured Merge tree store.
  LSM        : StorageBackend
  ||| Generic persistent store.
  Persistent : StorageBackend

export
Show StorageBackend where
  show InMemory   = "InMemory"
  show BTree      = "BTree"
  show LSM        = "LSM"
  show Persistent = "Persistent"

---------------------------------------------------------------------------
-- Import format: RDF import/bulk-load formats
---------------------------------------------------------------------------

||| RDF serialisation formats supported for bulk import.
public export
data ImportFormat : Type where
  NTriples : ImportFormat
  Turtle   : ImportFormat
  RDFxml   : ImportFormat
  JSONLD   : ImportFormat
  NQuads   : ImportFormat
  Trig     : ImportFormat

export
Show ImportFormat where
  show NTriples = "application/n-triples"
  show Turtle   = "text/turtle"
  show RDFxml   = "application/rdf+xml"
  show JSONLD   = "application/ld+json"
  show NQuads   = "application/n-quads"
  show Trig     = "application/trig"

---------------------------------------------------------------------------
-- Transaction isolation: isolation levels for store transactions
---------------------------------------------------------------------------

||| Transaction isolation level for the triple store.
public export
data TransactionIsolation : Type where
  ||| Read committed: reads see only committed data.
  ReadCommitted : TransactionIsolation
  ||| Serializable: full isolation.
  Serializable  : TransactionIsolation
  ||| Snapshot isolation: consistent reads at a point in time.
  Snapshot      : TransactionIsolation

export
Show TransactionIsolation where
  show ReadCommitted = "ReadCommitted"
  show Serializable  = "Serializable"
  show Snapshot      = "Snapshot"
