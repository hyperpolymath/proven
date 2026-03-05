-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Top-level module for proven-graphql.
||| Re-exports GraphQL.Types and provides server constants.
module GraphQL

import public GraphQL.Types

%default total

---------------------------------------------------------------------------
-- Server Constants
---------------------------------------------------------------------------

||| Default GraphQL HTTPS port.
public export
graphqlPort : Nat
graphqlPort = 443

||| Maximum allowed query nesting depth.
public export
maxQueryDepth : Nat
maxQueryDepth = 15

||| Maximum allowed query complexity score.
public export
maxQueryComplexity : Nat
maxQueryComplexity = 1000

||| Whether introspection queries are enabled.
public export
introspectionEnabled : Bool
introspectionEnabled = True
