-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-semweb semantic web server.
||| Defines closed sum types for RDF serialisation formats, resource types,
||| HTTP methods, content negotiation, and error codes.
module Semweb.Types

%default total

---------------------------------------------------------------------------
-- Format: RDF serialisation formats
---------------------------------------------------------------------------

||| RDF serialisation format.
public export
data Format : Type where
  ||| RDF/XML (W3C Recommendation).
  RDFxml   : Format
  ||| Turtle - Terse RDF Triple Language (W3C Recommendation).
  Turtle   : Format
  ||| N-Triples line-based format (W3C Recommendation).
  NTriples : Format
  ||| N-Quads line-based quad format (W3C Recommendation).
  NQuads   : Format
  ||| JSON-LD (W3C Recommendation).
  JSONLD   : Format
  ||| TriG - Turtle with named graphs (W3C Recommendation).
  Trig     : Format

export
Show Format where
  show RDFxml   = "application/rdf+xml"
  show Turtle   = "text/turtle"
  show NTriples = "application/n-triples"
  show NQuads   = "application/n-quads"
  show JSONLD   = "application/ld+json"
  show Trig     = "application/trig"

---------------------------------------------------------------------------
-- Resource type: kinds of semantic web resources
---------------------------------------------------------------------------

||| Classification of a semantic web resource.
public export
data ResourceType : Type where
  Class      : ResourceType
  Property   : ResourceType
  Individual : ResourceType
  Ontology   : ResourceType
  NamedGraph : ResourceType

export
Show ResourceType where
  show Class      = "Class"
  show Property   = "Property"
  show Individual = "Individual"
  show Ontology   = "Ontology"
  show NamedGraph = "NamedGraph"

---------------------------------------------------------------------------
-- HTTP method: supported HTTP verbs
---------------------------------------------------------------------------

||| HTTP methods supported by the semantic web server.
public export
data HTTPMethod : Type where
  Get    : HTTPMethod
  Post   : HTTPMethod
  Put    : HTTPMethod
  Patch  : HTTPMethod
  Delete : HTTPMethod

export
Show HTTPMethod where
  show Get    = "GET"
  show Post   = "POST"
  show Put    = "PUT"
  show Patch  = "PATCH"
  show Delete = "DELETE"

---------------------------------------------------------------------------
-- Content negotiation: preferred response formats
---------------------------------------------------------------------------

||| Content types available via HTTP content negotiation.
public export
data ContentNegotiation : Type where
  NegRDFxml : ContentNegotiation
  NegTurtle : ContentNegotiation
  NegJSONLD : ContentNegotiation
  NegHTML   : ContentNegotiation

export
Show ContentNegotiation where
  show NegRDFxml = "application/rdf+xml"
  show NegTurtle = "text/turtle"
  show NegJSONLD = "application/ld+json"
  show NegHTML   = "text/html"

---------------------------------------------------------------------------
-- Error code: semantic web server error codes
---------------------------------------------------------------------------

||| Error codes returned by the semantic web server.
public export
data ErrorCode : Type where
  NotFound          : ErrorCode
  InvalidURI        : ErrorCode
  MalformedRDF      : ErrorCode
  UnsupportedFormat : ErrorCode
  ConflictingTriples : ErrorCode

export
Show ErrorCode where
  show NotFound          = "NotFound"
  show InvalidURI        = "InvalidURI"
  show MalformedRDF      = "MalformedRDF"
  show UnsupportedFormat = "UnsupportedFormat"
  show ConflictingTriples = "ConflictingTriples"
