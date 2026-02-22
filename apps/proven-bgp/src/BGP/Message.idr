-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- BGP Message Types and Parser (RFC 4271 Section 4)
--
-- All message parsing is bounds-checked. Buffer reads use proven lengths
-- so out-of-bounds access is rejected at compile time or returns an error.
-- No message, no matter how malformed, can crash this parser.

module BGP.Message

%default total

-- ============================================================================
-- BGP Message Types (RFC 4271 Section 4.1)
-- ============================================================================

||| BGP message type codes.
public export
data MessageType : Type where
  OPEN         : MessageType  -- Type 1
  UPDATE       : MessageType  -- Type 2
  NOTIFICATION : MessageType  -- Type 3
  KEEPALIVE    : MessageType  -- Type 4

public export
Show MessageType where
  show OPEN         = "OPEN"
  show UPDATE       = "UPDATE"
  show NOTIFICATION = "NOTIFICATION"
  show KEEPALIVE    = "KEEPALIVE"

||| Convert a byte to a message type. Returns Nothing for unknown types.
public export
messageTypeFromByte : Bits8 -> Maybe MessageType
messageTypeFromByte 1 = Just OPEN
messageTypeFromByte 2 = Just UPDATE
messageTypeFromByte 3 = Just NOTIFICATION
messageTypeFromByte 4 = Just KEEPALIVE
messageTypeFromByte _ = Nothing

||| Convert a message type to its byte code.
public export
messageTypeToByte : MessageType -> Bits8
messageTypeToByte OPEN         = 1
messageTypeToByte UPDATE       = 2
messageTypeToByte NOTIFICATION = 3
messageTypeToByte KEEPALIVE    = 4

-- ============================================================================
-- BGP Header (RFC 4271 Section 4.1)
-- ============================================================================

||| BGP message header: 16-byte marker + 2-byte length + 1-byte type.
||| Length includes the header itself (minimum 19, maximum 4096).
public export
record BGPHeader where
  constructor MkHeader
  msgLength : Bits16   -- Total message length including header
  msgType   : MessageType

-- ============================================================================
-- OPEN Message (RFC 4271 Section 4.2)
-- ============================================================================

||| BGP OPEN message parameters.
public export
record OpenMessage where
  constructor MkOpen
  version       : Bits8     -- BGP version (must be 4)
  myAS          : Bits16    -- Sender's AS number (2-byte for BGP-4)
  holdTime      : Bits16    -- Proposed hold time in seconds
  bgpIdentifier : Bits32    -- BGP Identifier (Router ID, typically IPv4)
  optParamLen   : Bits8     -- Length of optional parameters
  -- Optional parameters stored separately

-- ============================================================================
-- UPDATE Message (RFC 4271 Section 4.3)
-- ============================================================================

||| An IP prefix with length (used in NLRI and withdrawn routes).
public export
record IPPrefix where
  constructor MkPrefix
  prefixLen : Bits8         -- Number of significant bits (0-32 for IPv4)
  prefix    : Bits32        -- IP address prefix (host byte order)

public export
Show IPPrefix where
  show p = let addr = p.prefix
               a = cast {to=Nat} (prim__shr_Bits32 addr 24)
               b = cast {to=Nat} (prim__and_Bits32 (prim__shr_Bits32 addr 16) 0xFF)
               c = cast {to=Nat} (prim__and_Bits32 (prim__shr_Bits32 addr 8) 0xFF)
               d = cast {to=Nat} (prim__and_Bits32 addr 0xFF)
           in show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
              ++ "/" ++ show (cast {to=Nat} p.prefixLen)

-- ============================================================================
-- Path Attributes (RFC 4271 Section 4.3, 5)
-- ============================================================================

||| Well-known path attribute type codes.
public export
data PathAttrType : Type where
  ORIGIN      : PathAttrType  -- Type 1: IGP, EGP, or INCOMPLETE
  AS_PATH     : PathAttrType  -- Type 2: Sequence of AS numbers
  NEXT_HOP    : PathAttrType  -- Type 3: Next-hop IP address
  MED         : PathAttrType  -- Type 4: Multi-Exit Discriminator
  LOCAL_PREF  : PathAttrType  -- Type 5: Local Preference
  ATOMIC_AGGR : PathAttrType  -- Type 6: Atomic Aggregate
  AGGREGATOR  : PathAttrType  -- Type 7: Aggregator AS + IP
  UnknownAttr : Bits8 -> PathAttrType

public export
Show PathAttrType where
  show ORIGIN      = "ORIGIN"
  show AS_PATH     = "AS_PATH"
  show NEXT_HOP    = "NEXT_HOP"
  show MED         = "MED"
  show LOCAL_PREF  = "LOCAL_PREF"
  show ATOMIC_AGGR = "ATOMIC_AGGREGATE"
  show AGGREGATOR  = "AGGREGATOR"
  show (UnknownAttr n) = "UNKNOWN(" ++ show (cast {to=Nat} n) ++ ")"

||| Origin values (RFC 4271 Section 4.3).
public export
data Origin : Type where
  IGP        : Origin  -- 0
  EGP        : Origin  -- 1
  INCOMPLETE : Origin  -- 2

||| AS_PATH segment types.
public export
data ASPathSegmentType : Type where
  AS_SET      : ASPathSegmentType  -- 1: Unordered set of ASes
  AS_SEQUENCE : ASPathSegmentType  -- 2: Ordered sequence of ASes

||| A single AS_PATH segment.
public export
record ASPathSegment where
  constructor MkASPathSegment
  segmentType : ASPathSegmentType
  asNumbers   : List Bits32  -- 4-byte AS numbers (RFC 6793)

||| Parsed path attributes for a route.
public export
record PathAttributes where
  constructor MkPathAttrs
  origin    : Maybe Origin
  asPath    : List ASPathSegment
  nextHop   : Maybe Bits32       -- IPv4 next-hop address
  med       : Maybe Bits32       -- Multi-Exit Discriminator
  localPref : Maybe Bits32       -- Local Preference

||| Empty path attributes (no attributes parsed yet).
public export
emptyPathAttrs : PathAttributes
emptyPathAttrs = MkPathAttrs Nothing [] Nothing Nothing Nothing

-- ============================================================================
-- UPDATE Message structure
-- ============================================================================

||| Parsed UPDATE message.
public export
record UpdateMessage where
  constructor MkUpdate
  withdrawnRoutes   : List IPPrefix
  pathAttributes    : PathAttributes
  nlri              : List IPPrefix    -- Network Layer Reachability Info

-- ============================================================================
-- NOTIFICATION Message (RFC 4271 Section 4.5)
-- ============================================================================

||| BGP error codes (RFC 4271 Section 4.5).
public export
data ErrorCode : Type where
  MessageHeaderError : ErrorCode  -- 1
  OpenMessageError   : ErrorCode  -- 2
  UpdateMessageError : ErrorCode  -- 3
  HoldTimerExpired   : ErrorCode  -- 4
  FSMError           : ErrorCode  -- 5
  Cease              : ErrorCode  -- 6

||| NOTIFICATION message.
public export
record NotificationMessage where
  constructor MkNotification
  errorCode    : Bits8
  errorSubcode : Bits8
  errorData    : List Bits8  -- Variable-length diagnostic data

-- ============================================================================
-- Unified BGP Message
-- ============================================================================

||| A fully parsed BGP message.
public export
data BGPMessage : Type where
  MsgOpen         : OpenMessage -> BGPMessage
  MsgUpdate       : UpdateMessage -> BGPMessage
  MsgNotification : NotificationMessage -> BGPMessage
  MsgKeepalive    : BGPMessage

public export
Show BGPMessage where
  show (MsgOpen o)         = "OPEN(AS=" ++ show (cast {to=Nat} o.myAS)
                             ++ ", hold=" ++ show (cast {to=Nat} o.holdTime) ++ ")"
  show (MsgUpdate u)       = "UPDATE(withdrawn=" ++ show (length u.withdrawnRoutes)
                             ++ ", nlri=" ++ show (length u.nlri) ++ ")"
  show (MsgNotification n) = "NOTIFICATION(code=" ++ show (cast {to=Nat} n.errorCode)
                             ++ ", sub=" ++ show (cast {to=Nat} n.errorSubcode) ++ ")"
  show MsgKeepalive        = "KEEPALIVE"

-- ============================================================================
-- Parse errors (no crashes, only typed errors)
-- ============================================================================

||| Parse errors are values, not exceptions. Cannot crash.
public export
data ParseError : Type where
  InvalidMarker     : ParseError
  MessageTooShort   : (actual : Nat) -> ParseError
  MessageTooLong    : (actual : Nat) -> ParseError
  UnknownType       : (typeByte : Bits8) -> ParseError
  TruncatedMessage  : (expected : Nat) -> (actual : Nat) -> ParseError
  InvalidVersion    : (version : Bits8) -> ParseError
  InvalidHoldTime   : (holdTime : Bits16) -> ParseError
  MalformedAttr     : (attrType : Bits8) -> ParseError
  InternalError     : String -> ParseError

public export
Show ParseError where
  show InvalidMarker           = "Invalid BGP marker (expected 16 x 0xFF)"
  show (MessageTooShort n)     = "Message too short: " ++ show n ++ " bytes (minimum 19)"
  show (MessageTooLong n)      = "Message too long: " ++ show n ++ " bytes (maximum 4096)"
  show (UnknownType b)         = "Unknown message type: " ++ show (cast {to=Nat} b)
  show (TruncatedMessage e a)  = "Truncated: expected " ++ show e ++ ", got " ++ show a
  show (InvalidVersion v)      = "Invalid BGP version: " ++ show (cast {to=Nat} v)
  show (InvalidHoldTime h)     = "Invalid hold time: " ++ show (cast {to=Nat} h)
  show (MalformedAttr t)       = "Malformed attribute type: " ++ show (cast {to=Nat} t)
  show (InternalError s)       = "Internal: " ++ s
