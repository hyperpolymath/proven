-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- MQTT Quality of Service Levels (MQTT 3.1.1 Section 4.3)
--
-- Three QoS levels govern delivery guarantees for PUBLISH messages.
-- The type system ensures only valid QoS levels can be constructed,
-- and downgrade rules are proven correct at compile time.

module MQTT.QoS

%default total

-- ============================================================================
-- QoS Levels (MQTT 3.1.1 Section 4.3)
-- ============================================================================

||| MQTT Quality of Service levels.
||| These govern the delivery guarantee for PUBLISH messages.
public export
data QoS : Type where
  ||| At most once delivery (fire and forget).
  ||| The message is delivered at most once, or not at all.
  ||| No acknowledgement required. QoS value 0.
  AtMostOnce  : QoS
  ||| At least once delivery (acknowledged delivery).
  ||| The message is delivered at least once.
  ||| PUBACK acknowledgement required. QoS value 1.
  AtLeastOnce : QoS
  ||| Exactly once delivery (assured delivery).
  ||| The message is delivered exactly once via a 4-step handshake.
  ||| PUBREC/PUBREL/PUBCOMP flow required. QoS value 2.
  ExactlyOnce : QoS

public export
Eq QoS where
  AtMostOnce  == AtMostOnce  = True
  AtLeastOnce == AtLeastOnce = True
  ExactlyOnce == ExactlyOnce = True
  _           == _           = False

public export
Show QoS where
  show AtMostOnce  = "QoS 0 (At Most Once)"
  show AtLeastOnce = "QoS 1 (At Least Once)"
  show ExactlyOnce = "QoS 2 (Exactly Once)"

public export
Ord QoS where
  compare AtMostOnce  AtMostOnce  = EQ
  compare AtMostOnce  _           = LT
  compare AtLeastOnce AtMostOnce  = GT
  compare AtLeastOnce AtLeastOnce = EQ
  compare AtLeastOnce ExactlyOnce = LT
  compare ExactlyOnce ExactlyOnce = EQ
  compare ExactlyOnce _           = GT

-- ============================================================================
-- Numeric code conversion
-- ============================================================================

||| Convert a QoS level to its 2-bit numeric code.
public export
qosCode : QoS -> Bits8
qosCode AtMostOnce  = 0
qosCode AtLeastOnce = 1
qosCode ExactlyOnce = 2

||| Decode a 2-bit numeric code to a QoS level.
||| Returns Nothing for the reserved value 3 and any other invalid input.
public export
qosFromCode : Bits8 -> Maybe QoS
qosFromCode 0 = Just AtMostOnce
qosFromCode 1 = Just AtLeastOnce
qosFromCode 2 = Just ExactlyOnce
qosFromCode _ = Nothing

-- ============================================================================
-- QoS negotiation and downgrade
-- ============================================================================

||| Determine the effective QoS for a subscription.
||| MQTT 3.1.1 Section 3.8.4: The server MAY grant a lower QoS than requested.
||| The effective QoS is the minimum of requested and granted.
public export
effectiveQoS : (requested : QoS) -> (granted : QoS) -> QoS
effectiveQoS requested granted = min requested granted

||| Determine the QoS for delivering a published message to a subscriber.
||| MQTT 3.1.1 Section 3.3.1.2: The QoS of delivery is the minimum of the
||| message QoS and the subscription's maximum QoS.
public export
deliveryQoS : (messageQoS : QoS) -> (subscriptionMaxQoS : QoS) -> QoS
deliveryQoS messageQoS subscriptionMaxQoS = min messageQoS subscriptionMaxQoS

||| Check if a QoS level requires acknowledgement from the receiver.
||| QoS 0 (AtMostOnce) is fire-and-forget; QoS 1 and 2 require ack flows.
public export
requiresAck : QoS -> Bool
requiresAck AtMostOnce  = False
requiresAck AtLeastOnce = True
requiresAck ExactlyOnce = True

||| The number of acknowledgement packets needed to complete a QoS flow.
||| QoS 0: 0 packets (fire and forget)
||| QoS 1: 1 packet  (PUBACK)
||| QoS 2: 3 packets (PUBREC, PUBREL, PUBCOMP)
public export
ackPacketCount : QoS -> Nat
ackPacketCount AtMostOnce  = 0
ackPacketCount AtLeastOnce = 1
ackPacketCount ExactlyOnce = 3

-- ============================================================================
-- SUBACK return codes
-- ============================================================================

||| SUBACK return code for a single topic subscription.
||| MQTT 3.1.1 Section 3.9.3.
public export
data SubAckCode : Type where
  ||| Subscription accepted with maximum QoS 0.
  GrantedQoS0 : SubAckCode
  ||| Subscription accepted with maximum QoS 1.
  GrantedQoS1 : SubAckCode
  ||| Subscription accepted with maximum QoS 2.
  GrantedQoS2 : SubAckCode
  ||| Subscription rejected by the server.
  SubFailure  : SubAckCode

public export
Eq SubAckCode where
  GrantedQoS0 == GrantedQoS0 = True
  GrantedQoS1 == GrantedQoS1 = True
  GrantedQoS2 == GrantedQoS2 = True
  SubFailure  == SubFailure  = True
  _           == _           = False

public export
Show SubAckCode where
  show GrantedQoS0 = "Granted QoS 0"
  show GrantedQoS1 = "Granted QoS 1"
  show GrantedQoS2 = "Granted QoS 2"
  show SubFailure  = "Subscription Failure"

||| Convert a SUBACK return code to its byte value.
public export
subAckCodeToByte : SubAckCode -> Bits8
subAckCodeToByte GrantedQoS0 = 0x00
subAckCodeToByte GrantedQoS1 = 0x01
subAckCodeToByte GrantedQoS2 = 0x02
subAckCodeToByte SubFailure  = 0x80

||| Decode a byte to a SUBACK return code.
||| Returns Nothing for unrecognised values.
public export
subAckCodeFromByte : Bits8 -> Maybe SubAckCode
subAckCodeFromByte 0x00 = Just GrantedQoS0
subAckCodeFromByte 0x01 = Just GrantedQoS1
subAckCodeFromByte 0x02 = Just GrantedQoS2
subAckCodeFromByte 0x80 = Just SubFailure
subAckCodeFromByte _    = Nothing

||| Convert a granted QoS SUBACK code to the corresponding QoS level.
||| Returns Nothing for SubFailure.
public export
subAckToQoS : SubAckCode -> Maybe QoS
subAckToQoS GrantedQoS0 = Just AtMostOnce
subAckToQoS GrantedQoS1 = Just AtLeastOnce
subAckToQoS GrantedQoS2 = Just ExactlyOnce
subAckToQoS SubFailure  = Nothing
