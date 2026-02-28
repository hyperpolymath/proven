-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- MQTT Control Packet Types (MQTT 3.1.1 Section 2.2)
--
-- All 14 MQTT control packet types encoded as a sum type with
-- compile-time exhaustive matching. Numeric codes map to the 4-bit
-- type field in the fixed header. Invalid type codes are rejected
-- at parse time via Maybe — no crashes on malformed input.

module MQTT.PacketType

%default total

-- ============================================================================
-- MQTT Control Packet Types (MQTT 3.1.1 Section 2.2.1)
-- ============================================================================

||| The 14 MQTT control packet types as defined in MQTT 3.1.1.
||| Each constructor corresponds to a 4-bit type code in the fixed header.
public export
data PacketType : Type where
  ||| Client request to connect to server (type 1).
  CONNECT     : PacketType
  ||| Server acknowledgement of connection (type 2).
  CONNACK     : PacketType
  ||| Publish message (type 3).
  PUBLISH     : PacketType
  ||| Publish acknowledgement for QoS 1 (type 4).
  PUBACK      : PacketType
  ||| Publish received for QoS 2, step 1 (type 5).
  PUBREC      : PacketType
  ||| Publish release for QoS 2, step 2 (type 6).
  PUBREL      : PacketType
  ||| Publish complete for QoS 2, step 3 (type 7).
  PUBCOMP     : PacketType
  ||| Client subscribe request (type 8).
  SUBSCRIBE   : PacketType
  ||| Server subscribe acknowledgement (type 9).
  SUBACK      : PacketType
  ||| Client unsubscribe request (type 10).
  UNSUBSCRIBE : PacketType
  ||| Server unsubscribe acknowledgement (type 11).
  UNSUBACK    : PacketType
  ||| Client ping request (type 12).
  PINGREQ     : PacketType
  ||| Server ping response (type 13).
  PINGRESP    : PacketType
  ||| Client disconnect notification (type 14).
  DISCONNECT  : PacketType

public export
Eq PacketType where
  CONNECT     == CONNECT     = True
  CONNACK     == CONNACK     = True
  PUBLISH     == PUBLISH     = True
  PUBACK      == PUBACK      = True
  PUBREC      == PUBREC      = True
  PUBREL      == PUBREL      = True
  PUBCOMP     == PUBCOMP     = True
  SUBSCRIBE   == SUBSCRIBE   = True
  SUBACK      == SUBACK      = True
  UNSUBSCRIBE == UNSUBSCRIBE = True
  UNSUBACK    == UNSUBACK    = True
  PINGREQ     == PINGREQ     = True
  PINGRESP    == PINGRESP    = True
  DISCONNECT  == DISCONNECT  = True
  _           == _           = False

public export
Show PacketType where
  show CONNECT     = "CONNECT"
  show CONNACK     = "CONNACK"
  show PUBLISH     = "PUBLISH"
  show PUBACK      = "PUBACK"
  show PUBREC      = "PUBREC"
  show PUBREL      = "PUBREL"
  show PUBCOMP     = "PUBCOMP"
  show SUBSCRIBE   = "SUBSCRIBE"
  show SUBACK      = "SUBACK"
  show UNSUBSCRIBE = "UNSUBSCRIBE"
  show UNSUBACK    = "UNSUBACK"
  show PINGREQ     = "PINGREQ"
  show PINGRESP    = "PINGRESP"
  show DISCONNECT  = "DISCONNECT"

-- ============================================================================
-- Numeric code conversion
-- ============================================================================

||| Convert a packet type to its 4-bit numeric code.
||| These codes appear in bits 7-4 of the fixed header byte.
public export
packetTypeCode : PacketType -> Bits8
packetTypeCode CONNECT     = 1
packetTypeCode CONNACK     = 2
packetTypeCode PUBLISH     = 3
packetTypeCode PUBACK      = 4
packetTypeCode PUBREC      = 5
packetTypeCode PUBREL      = 6
packetTypeCode PUBCOMP     = 7
packetTypeCode SUBSCRIBE   = 8
packetTypeCode SUBACK      = 9
packetTypeCode UNSUBSCRIBE = 10
packetTypeCode UNSUBACK    = 11
packetTypeCode PINGREQ     = 12
packetTypeCode PINGRESP    = 13
packetTypeCode DISCONNECT  = 14

||| Decode a 4-bit numeric code to a packet type.
||| Returns Nothing for reserved codes (0, 15) and unknown values.
public export
packetTypeFromCode : Bits8 -> Maybe PacketType
packetTypeFromCode 1  = Just CONNECT
packetTypeFromCode 2  = Just CONNACK
packetTypeFromCode 3  = Just PUBLISH
packetTypeFromCode 4  = Just PUBACK
packetTypeFromCode 5  = Just PUBREC
packetTypeFromCode 6  = Just PUBREL
packetTypeFromCode 7  = Just PUBCOMP
packetTypeFromCode 8  = Just SUBSCRIBE
packetTypeFromCode 9  = Just SUBACK
packetTypeFromCode 10 = Just UNSUBSCRIBE
packetTypeFromCode 11 = Just UNSUBACK
packetTypeFromCode 12 = Just PINGREQ
packetTypeFromCode 13 = Just PINGRESP
packetTypeFromCode 14 = Just DISCONNECT
packetTypeFromCode _  = Nothing

-- ============================================================================
-- Packet type classification
-- ============================================================================

||| Direction of a packet type: client-to-server, server-to-client, or both.
public export
data PacketDirection : Type where
  ||| Sent from client to server only.
  ClientToServer : PacketDirection
  ||| Sent from server to client only.
  ServerToClient : PacketDirection
  ||| Can be sent in either direction.
  Bidirectional  : PacketDirection

public export
Eq PacketDirection where
  ClientToServer == ClientToServer = True
  ServerToClient == ServerToClient = True
  Bidirectional  == Bidirectional  = True
  _              == _              = False

public export
Show PacketDirection where
  show ClientToServer = "Client->Server"
  show ServerToClient = "Server->Client"
  show Bidirectional  = "Bidirectional"

||| Determine the allowed direction for a given packet type.
||| This is used to validate that packets flow in the correct direction.
public export
packetDirection : PacketType -> PacketDirection
packetDirection CONNECT     = ClientToServer
packetDirection CONNACK     = ServerToClient
packetDirection PUBLISH     = Bidirectional
packetDirection PUBACK      = Bidirectional
packetDirection PUBREC      = Bidirectional
packetDirection PUBREL      = Bidirectional
packetDirection PUBCOMP     = Bidirectional
packetDirection SUBSCRIBE   = ClientToServer
packetDirection SUBACK      = ServerToClient
packetDirection UNSUBSCRIBE = ClientToServer
packetDirection UNSUBACK    = ServerToClient
packetDirection PINGREQ     = ClientToServer
packetDirection PINGRESP    = ServerToClient
packetDirection DISCONNECT  = ClientToServer

||| Check whether a packet type requires a packet identifier.
||| PUBLISH with QoS > 0, PUBACK, PUBREC, PUBREL, PUBCOMP,
||| SUBSCRIBE, SUBACK, UNSUBSCRIBE, UNSUBACK all require one.
public export
requiresPacketId : PacketType -> Bool
requiresPacketId PUBACK      = True
requiresPacketId PUBREC      = True
requiresPacketId PUBREL      = True
requiresPacketId PUBCOMP     = True
requiresPacketId SUBSCRIBE   = True
requiresPacketId SUBACK      = True
requiresPacketId UNSUBSCRIBE = True
requiresPacketId UNSUBACK    = True
requiresPacketId _           = False
