-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- proven-mqtt: An MQTT 3.1.1 implementation that cannot crash.
--
-- Architecture:
--   - PacketType: 14 control packet types with compile-time exhaustive matching
--   - QoS: Three quality of service levels with proven downgrade rules
--   - Topic: Validated topic names and filters with wildcard placement proofs
--   - Session: 4-state connection lifecycle FSM with valid-transition-only types
--   - Packet: Full packet structure with remaining length validation
--
-- This module defines the core MQTT types and re-exports submodules.

module MQTT

import public MQTT.PacketType
import public MQTT.QoS
import public MQTT.Topic
import public MQTT.Session
import public MQTT.Packet

||| MQTT default port (unencrypted) as defined by IANA.
public export
mqttPort : Bits16
mqttPort = 1883

||| MQTT default port over TLS/SSL.
public export
mqttTlsPort : Bits16
mqttTlsPort = 8883

||| MQTT 3.1.1 protocol level (Section 3.1.2.2).
public export
protocolLevel : Bits8
protocolLevel = 4

||| Maximum packet size in bytes (268,435,455 = 256 MB - 1).
||| This is the maximum value representable by the 4-byte remaining length field.
public export
maxPacketSize : Nat
maxPacketSize = 268435455

||| Maximum topic length in bytes (MQTT 3.1.1 Section 4.7.3).
||| Encoded as a 2-byte length prefix, so maximum is 65535.
public export
maxTopicLength : Nat
maxTopicLength = 65535
