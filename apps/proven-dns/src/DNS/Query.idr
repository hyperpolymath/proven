-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- DNS Query Construction (RFC 1035 Section 4.1)
--
-- Queries are constructed from validated domain names and record types.
-- The query builder ensures that all fields are within bounds before
-- producing a query. Malformed input is rejected as a typed error.

module DNS.Query

import DNS.Name
import DNS.RecordType

%default total

-- ============================================================================
-- DNS Query Class (RFC 1035 Section 3.2.4)
-- ============================================================================

||| DNS query classes. IN (Internet) is by far the most common.
public export
data QueryClass : Type where
  ||| Internet (1) -- the standard class for almost all queries.
  IN  : QueryClass
  ||| Chaos (3) -- used for Chaosnet, rarely seen in practice.
  CH  : QueryClass
  ||| Hesiod (4) -- used by MIT's Hesiod name service.
  HS  : QueryClass
  ||| Any class (255) -- used in queries to match any class.
  ANY : QueryClass

public export
Eq QueryClass where
  IN  == IN  = True
  CH  == CH  = True
  HS  == HS  = True
  ANY == ANY = True
  _   == _   = False

public export
Show QueryClass where
  show IN  = "IN"
  show CH  = "CH"
  show HS  = "HS"
  show ANY = "ANY"

||| Convert a query class to its numeric code.
public export
classToCode : QueryClass -> Bits16
classToCode IN  = 1
classToCode CH  = 3
classToCode HS  = 4
classToCode ANY = 255

||| Parse a numeric code to a query class.
public export
classFromCode : Bits16 -> Maybe QueryClass
classFromCode 1   = Just IN
classFromCode 3   = Just CH
classFromCode 4   = Just HS
classFromCode 255 = Just ANY
classFromCode _   = Nothing

-- ============================================================================
-- DNS Question (RFC 1035 Section 4.1.2)
-- ============================================================================

||| A single DNS question: "what record type for this name in this class?"
public export
record Question where
  constructor MkQuestion
  ||| The domain name being queried.
  qname  : DomainName
  ||| The record type requested (A, AAAA, MX, etc.).
  qtype  : RecordType
  ||| The query class (almost always IN).
  qclass : QueryClass

public export
Show Question where
  show q = show q.qname ++ " " ++ show q.qclass ++ " " ++ show q.qtype

public export
Eq Question where
  a == b = a.qname == b.qname && a.qtype == b.qtype && a.qclass == b.qclass

-- ============================================================================
-- DNS Query header flags (RFC 1035 Section 4.1.1)
-- ============================================================================

||| DNS header opcode values.
public export
data Opcode : Type where
  ||| Standard query (0).
  StandardQuery : Opcode
  ||| Inverse query (1) -- obsolete per RFC 3425.
  InverseQuery  : Opcode
  ||| Server status request (2).
  StatusRequest : Opcode

public export
Eq Opcode where
  StandardQuery == StandardQuery = True
  InverseQuery  == InverseQuery  = True
  StatusRequest == StatusRequest = True
  _             == _             = False

public export
Show Opcode where
  show StandardQuery = "QUERY"
  show InverseQuery  = "IQUERY"
  show StatusRequest = "STATUS"

-- ============================================================================
-- DNS Query message
-- ============================================================================

||| A complete DNS query message.
public export
record DNSQuery where
  constructor MkDNSQuery
  ||| Transaction ID for matching responses to queries.
  transactionId   : Bits16
  ||| The operation code for this query.
  opcode          : Opcode
  ||| Whether recursion is desired (RD flag).
  recursionDesired : Bool
  ||| The list of questions (typically exactly one).
  questions       : List Question

public export
Show DNSQuery where
  show q = "Query(id=" ++ show (cast {to=Nat} q.transactionId)
           ++ ", op=" ++ show q.opcode
           ++ ", rd=" ++ show q.recursionDesired
           ++ ", questions=" ++ show (length q.questions) ++ ")"

-- ============================================================================
-- Query construction helpers
-- ============================================================================

||| Build a simple A record query for a domain name string.
||| Returns Left if the domain name is invalid.
public export
simpleQuery : (txId : Bits16) -> (name : String) -> Either NameError DNSQuery
simpleQuery txId nameStr =
  case parseName nameStr of
    Left err   => Left err
    Right name => Right (MkDNSQuery
      { transactionId    = txId
      , opcode           = StandardQuery
      , recursionDesired = True
      , questions        = [MkQuestion name A IN]
      })

||| Build a query for a specific record type.
public export
typedQuery : (txId : Bits16) -> (name : String) -> RecordType -> Either NameError DNSQuery
typedQuery txId nameStr rtype =
  case parseName nameStr of
    Left err   => Left err
    Right name => Right (MkDNSQuery
      { transactionId    = txId
      , opcode           = StandardQuery
      , recursionDesired = True
      , questions        = [MkQuestion name rtype IN]
      })

||| Build a reverse DNS (PTR) query from an IPv4 address string.
||| The address "192.168.1.1" becomes "1.1.168.192.in-addr.arpa".
public export
reverseQuery : (txId : Bits16) -> (ipv4 : String) -> Either NameError DNSQuery
reverseQuery txId ipStr =
  let parts   = toList (split (== '.') ipStr)
      revParts = reverse parts
      arpaName = concat (intersperse "." revParts) ++ ".in-addr.arpa"
  in case parseName arpaName of
       Left err   => Left err
       Right name => Right (MkDNSQuery
         { transactionId    = txId
         , opcode           = StandardQuery
         , recursionDesired = True
         , questions        = [MkQuestion name PTR IN]
         })
