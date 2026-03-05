-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for GraphQL server.
||| All types are closed sum types with Show instances.
module GraphQL.Types

%default total

---------------------------------------------------------------------------
-- Operation Type
---------------------------------------------------------------------------

||| GraphQL root operation types.
public export
data OperationType : Type where
  Query        : OperationType
  Mutation     : OperationType
  Subscription : OperationType

public export
Show OperationType where
  show Query        = "query"
  show Mutation     = "mutation"
  show Subscription = "subscription"

---------------------------------------------------------------------------
-- Type Kind
---------------------------------------------------------------------------

||| GraphQL type system kinds (introspection __TypeKind).
public export
data TypeKind : Type where
  Scalar      : TypeKind
  Object      : TypeKind
  Interface   : TypeKind
  Union       : TypeKind
  Enum        : TypeKind
  InputObject : TypeKind
  List        : TypeKind
  NonNull     : TypeKind

public export
Show TypeKind where
  show Scalar      = "SCALAR"
  show Object      = "OBJECT"
  show Interface   = "INTERFACE"
  show Union       = "UNION"
  show Enum        = "ENUM"
  show InputObject = "INPUT_OBJECT"
  show List        = "LIST"
  show NonNull     = "NON_NULL"

---------------------------------------------------------------------------
-- Directive Location
---------------------------------------------------------------------------

||| GraphQL directive locations (executable and type system).
public export
data DirectiveLocation : Type where
  QUERY                  : DirectiveLocation
  MUTATION               : DirectiveLocation
  SUBSCRIPTION           : DirectiveLocation
  FIELD                  : DirectiveLocation
  FRAGMENT_DEFINITION    : DirectiveLocation
  FRAGMENT_SPREAD        : DirectiveLocation
  INLINE_FRAGMENT        : DirectiveLocation
  SCHEMA                 : DirectiveLocation
  SCALAR_LOC             : DirectiveLocation
  OBJECT_LOC             : DirectiveLocation
  FIELD_DEFINITION       : DirectiveLocation
  ARGUMENT_DEFINITION    : DirectiveLocation
  INTERFACE_LOC          : DirectiveLocation
  UNION_LOC              : DirectiveLocation
  ENUM_LOC               : DirectiveLocation
  ENUM_VALUE             : DirectiveLocation
  INPUT_OBJECT_LOC       : DirectiveLocation
  INPUT_FIELD_DEFINITION : DirectiveLocation

public export
Show DirectiveLocation where
  show QUERY                  = "QUERY"
  show MUTATION               = "MUTATION"
  show SUBSCRIPTION           = "SUBSCRIPTION"
  show FIELD                  = "FIELD"
  show FRAGMENT_DEFINITION    = "FRAGMENT_DEFINITION"
  show FRAGMENT_SPREAD        = "FRAGMENT_SPREAD"
  show INLINE_FRAGMENT        = "INLINE_FRAGMENT"
  show SCHEMA                 = "SCHEMA"
  show SCALAR_LOC             = "SCALAR"
  show OBJECT_LOC             = "OBJECT"
  show FIELD_DEFINITION       = "FIELD_DEFINITION"
  show ARGUMENT_DEFINITION    = "ARGUMENT_DEFINITION"
  show INTERFACE_LOC          = "INTERFACE"
  show UNION_LOC              = "UNION"
  show ENUM_LOC               = "ENUM"
  show ENUM_VALUE             = "ENUM_VALUE"
  show INPUT_OBJECT_LOC       = "INPUT_OBJECT"
  show INPUT_FIELD_DEFINITION = "INPUT_FIELD_DEFINITION"

---------------------------------------------------------------------------
-- Error Category
---------------------------------------------------------------------------

||| GraphQL error categories for structured error reporting.
public export
data ErrorCategory : Type where
  ParseError      : ErrorCategory
  ValidationError : ErrorCategory
  ExecutionError  : ErrorCategory
  AuthError       : ErrorCategory
  RateLimited     : ErrorCategory

public export
Show ErrorCategory where
  show ParseError      = "PARSE_ERROR"
  show ValidationError = "VALIDATION_ERROR"
  show ExecutionError  = "EXECUTION_ERROR"
  show AuthError       = "AUTH_ERROR"
  show RateLimited     = "RATE_LIMITED"
