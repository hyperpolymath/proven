-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main : Entry point for proven-sdn. Prints server identity and
-- enumerates all protocol type constructors.

module Main

import SDN

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

||| All MessageType constructors for enumeration.
allMessageTypes : List MessageType
allMessageTypes =
  [ Hello, Error, EchoRequest, EchoReply
  , FeaturesRequest, FeaturesReply, FlowMod
  , PacketIn, PacketOut, PortStatus
  , BarrierRequest, BarrierReply
  ]

||| All FlowAction constructors for enumeration.
allFlowActions : List FlowAction
allFlowActions = [Output, SetField, Drop, PushVLAN, PopVLAN, SetQueue, Group]

||| All MatchField constructors for enumeration.
allMatchFields : List MatchField
allMatchFields =
  [InPort, EthDst, EthSrc, EthType, VLANID, IPSrc, IPDst, TCPSrc, TCPDst, UDPSrc, UDPDst]

||| All PortState constructors for enumeration.
allPortStates : List PortState
allPortStates = [Up, Down, Blocked]

main : IO ()
main = do
  putStrLn "proven-sdn : OpenFlow-style SDN server"
  putStrLn $ "  Port: " ++ show sdnPort
  putStrLn $ "  MessageTypes:  " ++ show allMessageTypes
  putStrLn $ "  FlowActions:   " ++ show allFlowActions
  putStrLn $ "  MatchFields:   " ++ show allMatchFields
  putStrLn $ "  PortStates:    " ++ show allPortStates
