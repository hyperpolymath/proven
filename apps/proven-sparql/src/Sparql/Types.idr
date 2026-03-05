-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-sparql endpoint.
||| Defines closed sum types for SPARQL query types, update operations,
||| result formats, and error types per the SPARQL 1.1 specification.
module Sparql.Types

%default total

---------------------------------------------------------------------------
-- Query type: SPARQL 1.1 query forms
---------------------------------------------------------------------------

||| SPARQL 1.1 query forms (W3C Recommendation).
public export
data QueryType : Type where
  ||| SELECT returns variable bindings.
  Select    : QueryType
  ||| CONSTRUCT returns an RDF graph.
  Construct : QueryType
  ||| ASK returns a boolean.
  Ask       : QueryType
  ||| DESCRIBE returns an RDF graph describing a resource.
  Describe  : QueryType

export
Show QueryType where
  show Select    = "SELECT"
  show Construct = "CONSTRUCT"
  show Ask       = "ASK"
  show Describe  = "DESCRIBE"

---------------------------------------------------------------------------
-- Update type: SPARQL 1.1 Update operations
---------------------------------------------------------------------------

||| SPARQL 1.1 Update operations.
public export
data UpdateType : Type where
  Insert : UpdateType
  Delete : UpdateType
  Load   : UpdateType
  Clear  : UpdateType
  Create : UpdateType
  Drop   : UpdateType

export
Show UpdateType where
  show Insert = "INSERT"
  show Delete = "DELETE"
  show Load   = "LOAD"
  show Clear  = "CLEAR"
  show Create = "CREATE"
  show Drop   = "DROP"

---------------------------------------------------------------------------
-- Result format: SPARQL query result serialisation formats
---------------------------------------------------------------------------

||| Serialisation formats for SPARQL query results.
public export
data ResultFormat : Type where
  XML : ResultFormat
  JSON : ResultFormat
  CSV : ResultFormat
  TSV : ResultFormat

export
Show ResultFormat where
  show XML  = "application/sparql-results+xml"
  show JSON = "application/sparql-results+json"
  show CSV  = "text/csv"
  show TSV  = "text/tab-separated-values"

---------------------------------------------------------------------------
-- Error type: SPARQL endpoint error types
---------------------------------------------------------------------------

||| Error types returned by the SPARQL endpoint.
public export
data ErrorType : Type where
  ParseError      : ErrorType
  QueryTimeout    : ErrorType
  ResultsTooLarge : ErrorType
  UnknownGraph    : ErrorType
  AccessDenied    : ErrorType

export
Show ErrorType where
  show ParseError      = "ParseError"
  show QueryTimeout    = "QueryTimeout"
  show ResultsTooLarge = "ResultsTooLarge"
  show UnknownGraph    = "UnknownGraph"
  show AccessDenied    = "AccessDenied"
