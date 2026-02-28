-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- DNS Record Types (RFC 1035, RFC 3596, RFC 2782, RFC 6891)
--
-- All standard DNS resource record types as a closed sum type.
-- Each type carries its numeric code. Unknown types parse to Nothing
-- rather than crashing. The type system guarantees exhaustive handling.

module DNS.RecordType

%default total

-- ============================================================================
-- DNS Record Types
-- ============================================================================

||| Standard DNS resource record types.
||| Each constructor represents a well-known RR type from the IANA registry.
public export
data RecordType : Type where
  ||| Host address (IPv4), type 1 (RFC 1035 Section 3.4.1).
  A     : RecordType
  ||| IPv6 host address, type 28 (RFC 3596).
  AAAA  : RecordType
  ||| Canonical name (alias), type 5 (RFC 1035 Section 3.3.1).
  CNAME : RecordType
  ||| Mail exchange, type 15 (RFC 1035 Section 3.3.9).
  MX    : RecordType
  ||| Authoritative name server, type 2 (RFC 1035 Section 3.3.11).
  NS    : RecordType
  ||| Text strings, type 16 (RFC 1035 Section 3.3.14).
  TXT   : RecordType
  ||| Start of authority, type 6 (RFC 1035 Section 3.3.13).
  SOA   : RecordType
  ||| Domain name pointer (reverse DNS), type 12 (RFC 1035 Section 3.3.12).
  PTR   : RecordType
  ||| Service locator, type 33 (RFC 2782).
  SRV   : RecordType

public export
Eq RecordType where
  A     == A     = True
  AAAA  == AAAA  = True
  CNAME == CNAME = True
  MX    == MX    = True
  NS    == NS    = True
  TXT   == TXT   = True
  SOA   == SOA   = True
  PTR   == PTR   = True
  SRV   == SRV   = True
  _     == _     = False

public export
Show RecordType where
  show A     = "A"
  show AAAA  = "AAAA"
  show CNAME = "CNAME"
  show MX    = "MX"
  show NS    = "NS"
  show TXT   = "TXT"
  show SOA   = "SOA"
  show PTR   = "PTR"
  show SRV   = "SRV"

-- ============================================================================
-- Numeric code mapping (IANA DNS Parameters)
-- ============================================================================

||| Convert a record type to its numeric code (IANA registry).
public export
toCode : RecordType -> Bits16
toCode A     = 1
toCode AAAA  = 28
toCode CNAME = 5
toCode MX    = 15
toCode NS    = 2
toCode TXT   = 16
toCode SOA   = 6
toCode PTR   = 12
toCode SRV   = 33

||| Parse a numeric code to a record type.
||| Returns Nothing for unknown or unimplemented types (no crash).
public export
fromCode : Bits16 -> Maybe RecordType
fromCode 1  = Just A
fromCode 2  = Just NS
fromCode 5  = Just CNAME
fromCode 6  = Just SOA
fromCode 12 = Just PTR
fromCode 15 = Just MX
fromCode 16 = Just TXT
fromCode 28 = Just AAAA
fromCode 33 = Just SRV
fromCode _  = Nothing

||| Parse a string name to a record type (case-insensitive).
public export
fromString : String -> Maybe RecordType
fromString s = case toUpper s of
  "A"     => Just A
  "AAAA"  => Just AAAA
  "CNAME" => Just CNAME
  "MX"    => Just MX
  "NS"    => Just NS
  "TXT"   => Just TXT
  "SOA"   => Just SOA
  "PTR"   => Just PTR
  "SRV"   => Just SRV
  _       => Nothing

-- ============================================================================
-- Record type classification
-- ============================================================================

||| Whether this record type holds address data (A or AAAA).
public export
isAddressType : RecordType -> Bool
isAddressType A    = True
isAddressType AAAA = True
isAddressType _    = False

||| Whether this record type references another domain name.
public export
isReferenceType : RecordType -> Bool
isReferenceType CNAME = True
isReferenceType NS    = True
isReferenceType PTR   = True
isReferenceType MX    = True
isReferenceType SRV   = True
isReferenceType _     = False

||| Whether this record type is a zone-level record (SOA, NS).
public export
isZoneType : RecordType -> Bool
isZoneType SOA = True
isZoneType NS  = True
isZoneType _   = False

||| List all supported record types.
public export
allTypes : List RecordType
allTypes = [A, AAAA, CNAME, MX, NS, TXT, SOA, PTR, SRV]
