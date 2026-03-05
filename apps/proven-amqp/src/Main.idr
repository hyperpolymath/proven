-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for the proven-amqp skeleton.
-- | Prints the server name and demonstrates type constructors.

module Main

import AMQP

%default total

||| All AMQP frame type constructors for demonstration.
allFrameTypes : List FrameType
allFrameTypes = [Method, Header, Body, Heartbeat]

||| All AMQP method class constructors for demonstration.
allMethodClasses : List MethodClass
allMethodClasses = [Connection, Channel, Exchange, Queue, Basic, Tx, Confirm]

||| All AMQP exchange type constructors for demonstration.
allExchangeTypes : List ExchangeType
allExchangeTypes = [Direct, Fanout, Topic, Headers]

||| All AMQP delivery mode constructors for demonstration.
allDeliveryModes : List DeliveryMode
allDeliveryModes = [NonPersistent, Persistent]

||| All AMQP error code constructors for demonstration.
allErrorCodes : List ErrorCode
allErrorCodes =
  [ ContentTooLarge, NoConsumers, ConnectionForced, InvalidPath
  , AccessRefused, NotFound, ResourceLocked, FrameError
  , CommandInvalid, ChannelError, ResourceError, NotAllowed, InternalError ]

main : IO ()
main = do
  putStrLn "proven-amqp: AMQP 0-9-1 Message Queuing"
  putStrLn $ "  AMQP port:       " ++ show amqpPort
  putStrLn $ "  AMQPS port:      " ++ show amqpsPort
  putStrLn $ "  Max frame size:  " ++ show maxFrameSize
  putStrLn $ "  Heartbeat:       " ++ show heartbeatInterval ++ "s"
  putStrLn $ "  Frame types:     " ++ show allFrameTypes
  putStrLn $ "  Method classes:  " ++ show allMethodClasses
  putStrLn $ "  Exchange types:  " ++ show allExchangeTypes
  putStrLn $ "  Delivery modes:  " ++ show allDeliveryModes
  putStrLn $ "  Error codes:     " ++ show allErrorCodes
