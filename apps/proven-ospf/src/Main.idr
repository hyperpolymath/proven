-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for the proven-ospf skeleton.
-- | Prints the server name and demonstrates type constructors.

module Main

import OSPF

%default total

||| All OSPF packet type constructors for demonstration.
allPacketTypes : List PacketType
allPacketTypes =
  [Hello, DatabaseDescription, LinkStateRequest, LinkStateUpdate, LinkStateAck]

||| All OSPF neighbor state constructors for demonstration.
allNeighborStates : List NeighborState
allNeighborStates = [Down, Attempt, Init, TwoWay, ExStart, Exchange, Loading, Full]

||| All OSPF LSA type constructors for demonstration.
allLSATypes : List LSAType
allLSATypes = [RouterLSA, NetworkLSA, SummaryLSA, ASBRSummaryLSA, ASExternalLSA]

||| All OSPF area type constructors for demonstration.
allAreaTypes : List AreaType
allAreaTypes = [Normal, Stub, TotallyStub, NSSA]

main : IO ()
main = do
  putStrLn "proven-ospf: RFC 2328 OSPF v2"
  putStrLn $ "  Protocol number: " ++ show ospfProtocol
  putStrLn $ "  Hello interval:  " ++ show helloInterval ++ "s"
  putStrLn $ "  Dead interval:   " ++ show deadInterval ++ "s"
  putStrLn $ "  AllSPFRouters:   " ++ allSPFRouters
  putStrLn $ "  AllDRouters:     " ++ allDRouters
  putStrLn $ "  Packet types:    " ++ show allPacketTypes
  putStrLn $ "  Neighbor states: " ++ show allNeighborStates
  putStrLn $ "  LSA types:       " ++ show allLSATypes
  putStrLn $ "  Area types:      " ++ show allAreaTypes
