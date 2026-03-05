-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for proven-graphql skeleton.
module Main

import GraphQL

%default total

||| Print server identification and type constructors.
covering
main : IO ()
main = do
  putStrLn "proven-graphql — GraphQL server skeleton"
  putStrLn $ "  Port: " ++ show graphqlPort
  putStrLn $ "  Max Query Depth: " ++ show maxQueryDepth
  putStrLn $ "  Max Query Complexity: " ++ show maxQueryComplexity
  putStrLn $ "  Introspection Enabled: " ++ show introspectionEnabled
  putStrLn "Operation Types:"
  putStrLn $ "  " ++ show Query
  putStrLn $ "  " ++ show Mutation
  putStrLn $ "  " ++ show Subscription
  putStrLn "Type Kinds:"
  putStrLn $ "  " ++ show Scalar
  putStrLn $ "  " ++ show Object
  putStrLn $ "  " ++ show Interface
  putStrLn $ "  " ++ show Union
  putStrLn $ "  " ++ show Enum
  putStrLn $ "  " ++ show InputObject
  putStrLn $ "  " ++ show List
  putStrLn $ "  " ++ show NonNull
  putStrLn "Directive Locations:"
  putStrLn $ "  " ++ show QUERY
  putStrLn $ "  " ++ show MUTATION
  putStrLn $ "  " ++ show SUBSCRIPTION
  putStrLn $ "  " ++ show FIELD
  putStrLn $ "  " ++ show FRAGMENT_DEFINITION
  putStrLn $ "  " ++ show FRAGMENT_SPREAD
  putStrLn $ "  " ++ show INLINE_FRAGMENT
  putStrLn $ "  " ++ show SCHEMA
  putStrLn $ "  " ++ show SCALAR_LOC
  putStrLn $ "  " ++ show OBJECT_LOC
  putStrLn $ "  " ++ show FIELD_DEFINITION
  putStrLn $ "  " ++ show ARGUMENT_DEFINITION
  putStrLn $ "  " ++ show INTERFACE_LOC
  putStrLn $ "  " ++ show UNION_LOC
  putStrLn $ "  " ++ show ENUM_LOC
  putStrLn $ "  " ++ show ENUM_VALUE
  putStrLn $ "  " ++ show INPUT_OBJECT_LOC
  putStrLn $ "  " ++ show INPUT_FIELD_DEFINITION
  putStrLn "Error Categories:"
  putStrLn $ "  " ++ show ParseError
  putStrLn $ "  " ++ show ValidationError
  putStrLn $ "  " ++ show ExecutionError
  putStrLn $ "  " ++ show AuthError
  putStrLn $ "  " ++ show RateLimited
