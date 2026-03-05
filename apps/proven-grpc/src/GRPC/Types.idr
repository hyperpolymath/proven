-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core protocol types for gRPC over HTTP/2.
-- | Defines status codes, stream types, compression algorithms,
-- | and content types as closed sum types with Show instances.

module GRPC.Types

%default total

||| gRPC status codes per gRPC specification.
public export
data StatusCode : Type where
  Ok                  : StatusCode
  Cancelled           : StatusCode
  Unknown             : StatusCode
  InvalidArgument     : StatusCode
  DeadlineExceeded    : StatusCode
  NotFound            : StatusCode
  AlreadyExists       : StatusCode
  PermissionDenied    : StatusCode
  ResourceExhausted   : StatusCode
  FailedPrecondition  : StatusCode
  Aborted             : StatusCode
  OutOfRange          : StatusCode
  Unimplemented       : StatusCode
  Internal            : StatusCode
  Unavailable         : StatusCode
  DataLoss            : StatusCode
  Unauthenticated     : StatusCode

public export
Show StatusCode where
  show Ok                 = "Ok"
  show Cancelled          = "Cancelled"
  show Unknown            = "Unknown"
  show InvalidArgument    = "InvalidArgument"
  show DeadlineExceeded   = "DeadlineExceeded"
  show NotFound           = "NotFound"
  show AlreadyExists      = "AlreadyExists"
  show PermissionDenied   = "PermissionDenied"
  show ResourceExhausted  = "ResourceExhausted"
  show FailedPrecondition = "FailedPrecondition"
  show Aborted            = "Aborted"
  show OutOfRange         = "OutOfRange"
  show Unimplemented      = "Unimplemented"
  show Internal           = "Internal"
  show Unavailable        = "Unavailable"
  show DataLoss           = "DataLoss"
  show Unauthenticated    = "Unauthenticated"

||| gRPC stream cardinality types.
public export
data StreamType : Type where
  Unary            : StreamType
  ServerStreaming   : StreamType
  ClientStreaming   : StreamType
  BidiStreaming     : StreamType

public export
Show StreamType where
  show Unary           = "Unary"
  show ServerStreaming  = "ServerStreaming"
  show ClientStreaming  = "ClientStreaming"
  show BidiStreaming    = "BidiStreaming"

||| gRPC message compression algorithms.
public export
data Compression : Type where
  Identity : Compression
  Gzip     : Compression
  Deflate  : Compression
  Snappy   : Compression
  Zstd     : Compression

public export
Show Compression where
  show Identity = "Identity"
  show Gzip     = "Gzip"
  show Deflate  = "Deflate"
  show Snappy   = "Snappy"
  show Zstd     = "Zstd"

||| gRPC content type encodings.
public export
data ContentType : Type where
  Protobuf : ContentType
  JSON     : ContentType

public export
Show ContentType where
  show Protobuf = "Protobuf"
  show JSON     = "JSON"
