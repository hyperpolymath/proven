-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- SDN.Types : Core protocol types for OpenFlow-style SDN communication.
-- Defines message types, flow actions, match fields, and port states
-- as closed sum types with Show instances.

module SDN.Types

%default total

---------------------------------------------------------------------------
-- MessageType : OpenFlow message types exchanged between controller and switch.
---------------------------------------------------------------------------

||| OpenFlow protocol message types for controller-switch communication.
public export
data MessageType : Type where
  Hello            : MessageType
  Error            : MessageType
  EchoRequest      : MessageType
  EchoReply        : MessageType
  FeaturesRequest  : MessageType
  FeaturesReply    : MessageType
  FlowMod          : MessageType
  PacketIn         : MessageType
  PacketOut        : MessageType
  PortStatus       : MessageType
  BarrierRequest   : MessageType
  BarrierReply     : MessageType

export
Show MessageType where
  show Hello           = "Hello"
  show Error           = "Error"
  show EchoRequest     = "EchoRequest"
  show EchoReply       = "EchoReply"
  show FeaturesRequest = "FeaturesRequest"
  show FeaturesReply   = "FeaturesReply"
  show FlowMod         = "FlowMod"
  show PacketIn        = "PacketIn"
  show PacketOut       = "PacketOut"
  show PortStatus      = "PortStatus"
  show BarrierRequest  = "BarrierRequest"
  show BarrierReply    = "BarrierReply"

---------------------------------------------------------------------------
-- FlowAction : Actions applied to packets matching a flow rule.
---------------------------------------------------------------------------

||| Actions that can be applied to packets within an OpenFlow flow rule.
public export
data FlowAction : Type where
  Output   : FlowAction
  SetField : FlowAction
  Drop     : FlowAction
  PushVLAN : FlowAction
  PopVLAN  : FlowAction
  SetQueue : FlowAction
  Group    : FlowAction

export
Show FlowAction where
  show Output   = "Output"
  show SetField = "SetField"
  show Drop     = "Drop"
  show PushVLAN = "PushVLAN"
  show PopVLAN  = "PopVLAN"
  show SetQueue = "SetQueue"
  show Group    = "Group"

---------------------------------------------------------------------------
-- MatchField : Packet header fields used in flow table matching.
---------------------------------------------------------------------------

||| Packet header fields that can be matched against in flow rules.
public export
data MatchField : Type where
  InPort  : MatchField
  EthDst  : MatchField
  EthSrc  : MatchField
  EthType : MatchField
  VLANID  : MatchField
  IPSrc   : MatchField
  IPDst   : MatchField
  TCPSrc  : MatchField
  TCPDst  : MatchField
  UDPSrc  : MatchField
  UDPDst  : MatchField

export
Show MatchField where
  show InPort  = "InPort"
  show EthDst  = "EthDst"
  show EthSrc  = "EthSrc"
  show EthType = "EthType"
  show VLANID  = "VLANID"
  show IPSrc   = "IPSrc"
  show IPDst   = "IPDst"
  show TCPSrc  = "TCPSrc"
  show TCPDst  = "TCPDst"
  show UDPSrc  = "UDPSrc"
  show UDPDst  = "UDPDst"

---------------------------------------------------------------------------
-- PortState : Physical or logical port operational states.
---------------------------------------------------------------------------

||| Operational state of a switch port.
public export
data PortState : Type where
  Up      : PortState
  Down    : PortState
  Blocked : PortState

export
Show PortState where
  show Up      = "Up"
  show Down    = "Down"
  show Blocked = "Blocked"
