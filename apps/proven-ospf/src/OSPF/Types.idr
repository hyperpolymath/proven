-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core protocol types for RFC 2328 OSPF v2.
-- | Defines packet types, neighbor states, LSA types, and area types
-- | as closed sum types with Show instances.

module OSPF.Types

%default total

||| OSPF packet types per RFC 2328 Section A.3.1.
public export
data PacketType : Type where
  Hello                : PacketType
  DatabaseDescription  : PacketType
  LinkStateRequest     : PacketType
  LinkStateUpdate      : PacketType
  LinkStateAck         : PacketType

public export
Show PacketType where
  show Hello               = "Hello"
  show DatabaseDescription = "DatabaseDescription"
  show LinkStateRequest    = "LinkStateRequest"
  show LinkStateUpdate     = "LinkStateUpdate"
  show LinkStateAck        = "LinkStateAck"

||| OSPF neighbor state machine per RFC 2328 Section 10.1.
public export
data NeighborState : Type where
  Down      : NeighborState
  Attempt   : NeighborState
  Init      : NeighborState
  TwoWay    : NeighborState
  ExStart   : NeighborState
  Exchange  : NeighborState
  Loading   : NeighborState
  Full      : NeighborState

public export
Show NeighborState where
  show Down     = "Down"
  show Attempt  = "Attempt"
  show Init     = "Init"
  show TwoWay   = "TwoWay"
  show ExStart  = "ExStart"
  show Exchange = "Exchange"
  show Loading  = "Loading"
  show Full     = "Full"

||| OSPF Link State Advertisement types per RFC 2328 Section 12.1.
public export
data LSAType : Type where
  RouterLSA       : LSAType
  NetworkLSA      : LSAType
  SummaryLSA      : LSAType
  ASBRSummaryLSA  : LSAType
  ASExternalLSA   : LSAType

public export
Show LSAType where
  show RouterLSA      = "RouterLSA"
  show NetworkLSA     = "NetworkLSA"
  show SummaryLSA     = "SummaryLSA"
  show ASBRSummaryLSA = "ASBRSummaryLSA"
  show ASExternalLSA  = "ASExternalLSA"

||| OSPF area types per RFC 2328 / RFC 3101.
public export
data AreaType : Type where
  Normal      : AreaType
  Stub        : AreaType
  TotallyStub : AreaType
  NSSA        : AreaType

public export
Show AreaType where
  show Normal      = "Normal"
  show Stub        = "Stub"
  show TotallyStub = "TotallyStub"
  show NSSA        = "NSSA"
