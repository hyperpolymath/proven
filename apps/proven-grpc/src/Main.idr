-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for the proven-grpc skeleton.
-- | Prints the server name and demonstrates type constructors.

module Main

import GRPC

%default total

||| All gRPC status code constructors for demonstration.
allStatusCodes : List StatusCode
allStatusCodes =
  [ Ok, Cancelled, Unknown, InvalidArgument, DeadlineExceeded
  , NotFound, AlreadyExists, PermissionDenied, ResourceExhausted
  , FailedPrecondition, Aborted, OutOfRange, Unimplemented
  , Internal, Unavailable, DataLoss, Unauthenticated ]

||| All gRPC stream type constructors for demonstration.
allStreamTypes : List StreamType
allStreamTypes = [Unary, ServerStreaming, ClientStreaming, BidiStreaming]

||| All gRPC compression constructors for demonstration.
allCompressions : List Compression
allCompressions = [Identity, Gzip, Deflate, Snappy, Zstd]

||| All gRPC content type constructors for demonstration.
allContentTypes : List ContentType
allContentTypes = [Protobuf, JSON]

main : IO ()
main = do
  putStrLn "proven-grpc: gRPC over HTTP/2"
  putStrLn $ "  Port:             " ++ show grpcPort
  putStrLn $ "  Max message size: " ++ show maxMessageSize
  putStrLn $ "  Max metadata:     " ++ show maxMetadataSize
  putStrLn $ "  Status codes:     " ++ show allStatusCodes
  putStrLn $ "  Stream types:     " ++ show allStreamTypes
  putStrLn $ "  Compressions:     " ++ show allCompressions
  putStrLn $ "  Content types:    " ++ show allContentTypes
