-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Entry point for proven-opcua skeleton.
module Main

import OPCUA

%default total

||| Print server identification and type constructors.
covering
main : IO ()
main = do
  putStrLn "proven-opcua — OPC UA (Industrial IoT) skeleton"
  putStrLn $ "  Port: " ++ show opcuaPort
  putStrLn $ "  TLS Port: " ++ show opcuaTLSPort
  putStrLn $ "  Max Nodes Per Read: " ++ show maxNodesPerRead
  putStrLn "Service Types:"
  putStrLn $ "  " ++ show Read
  putStrLn $ "  " ++ show Write
  putStrLn $ "  " ++ show Browse
  putStrLn $ "  " ++ show Subscribe
  putStrLn $ "  " ++ show Publish
  putStrLn $ "  " ++ show Call
  putStrLn $ "  " ++ show CreateSession
  putStrLn $ "  " ++ show ActivateSession
  putStrLn $ "  " ++ show CloseSession
  putStrLn $ "  " ++ show CreateSubscription
  putStrLn $ "  " ++ show DeleteSubscription
  putStrLn "Node Classes:"
  putStrLn $ "  " ++ show Object
  putStrLn $ "  " ++ show Variable
  putStrLn $ "  " ++ show Method
  putStrLn $ "  " ++ show ObjectType
  putStrLn $ "  " ++ show VariableType
  putStrLn $ "  " ++ show ReferenceType
  putStrLn $ "  " ++ show DataType
  putStrLn $ "  " ++ show View
  putStrLn "Status Codes:"
  putStrLn $ "  " ++ show Good
  putStrLn $ "  " ++ show Uncertain
  putStrLn $ "  " ++ show Bad
  putStrLn $ "  " ++ show BadNodeIdUnknown
  putStrLn $ "  " ++ show BadAttributeIdInvalid
  putStrLn $ "  " ++ show BadNotReadable
  putStrLn $ "  " ++ show BadNotWritable
  putStrLn $ "  " ++ show BadOutOfRange
  putStrLn $ "  " ++ show BadTypeMismatch
  putStrLn $ "  " ++ show BadSessionIdInvalid
  putStrLn $ "  " ++ show BadSubscriptionIdInvalid
  putStrLn $ "  " ++ show BadTimeout
  putStrLn "Security Modes:"
  putStrLn $ "  " ++ show None
  putStrLn $ "  " ++ show Sign
  putStrLn $ "  " ++ show SignAndEncrypt
