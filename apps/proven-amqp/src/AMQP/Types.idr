-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core protocol types for AMQP 0-9-1 message queuing.
-- | Defines frame types, method classes, exchange types, delivery modes,
-- | and error codes as closed sum types with Show instances.

module AMQP.Types

%default total

||| AMQP frame types per AMQP 0-9-1 specification Section 2.3.5.
public export
data FrameType : Type where
  Method     : FrameType
  Header     : FrameType
  Body       : FrameType
  Heartbeat  : FrameType

public export
Show FrameType where
  show Method    = "Method"
  show Header    = "Header"
  show Body      = "Body"
  show Heartbeat = "Heartbeat"

||| AMQP method classes per AMQP 0-9-1 specification Section 4.
public export
data MethodClass : Type where
  Connection : MethodClass
  Channel    : MethodClass
  Exchange   : MethodClass
  Queue      : MethodClass
  Basic      : MethodClass
  Tx         : MethodClass
  Confirm    : MethodClass

public export
Show MethodClass where
  show Connection = "Connection"
  show Channel    = "Channel"
  show Exchange   = "Exchange"
  show Queue      = "Queue"
  show Basic      = "Basic"
  show Tx         = "Tx"
  show Confirm    = "Confirm"

||| AMQP exchange types per AMQP 0-9-1 specification Section 3.1.3.
public export
data ExchangeType : Type where
  Direct   : ExchangeType
  Fanout   : ExchangeType
  Topic    : ExchangeType
  Headers  : ExchangeType

public export
Show ExchangeType where
  show Direct  = "Direct"
  show Fanout  = "Fanout"
  show Topic   = "Topic"
  show Headers = "Headers"

||| AMQP delivery modes per AMQP 0-9-1 specification Section 4.2.6.
public export
data DeliveryMode : Type where
  NonPersistent : DeliveryMode
  Persistent    : DeliveryMode

public export
Show DeliveryMode where
  show NonPersistent = "NonPersistent"
  show Persistent    = "Persistent"

||| AMQP error/reply codes per AMQP 0-9-1 specification Appendix A.
public export
data ErrorCode : Type where
  ContentTooLarge   : ErrorCode
  NoConsumers       : ErrorCode
  ConnectionForced  : ErrorCode
  InvalidPath       : ErrorCode
  AccessRefused     : ErrorCode
  NotFound          : ErrorCode
  ResourceLocked    : ErrorCode
  FrameError        : ErrorCode
  CommandInvalid    : ErrorCode
  ChannelError      : ErrorCode
  ResourceError     : ErrorCode
  NotAllowed        : ErrorCode
  InternalError     : ErrorCode

public export
Show ErrorCode where
  show ContentTooLarge  = "ContentTooLarge"
  show NoConsumers      = "NoConsumers"
  show ConnectionForced = "ConnectionForced"
  show InvalidPath      = "InvalidPath"
  show AccessRefused    = "AccessRefused"
  show NotFound         = "NotFound"
  show ResourceLocked   = "ResourceLocked"
  show FrameError       = "FrameError"
  show CommandInvalid   = "CommandInvalid"
  show ChannelError     = "ChannelError"
  show ResourceError    = "ResourceError"
  show NotAllowed       = "NotAllowed"
  show InternalError    = "InternalError"
