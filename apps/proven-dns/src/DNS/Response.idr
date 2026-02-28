-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- DNS Response Construction (RFC 1035 Section 4.1)
--
-- Responses contain answer, authority, and additional sections, each
-- holding a list of resource records. Response codes are represented
-- as a sum type so that every possible outcome is explicitly handled.

module DNS.Response

import DNS.Name
import DNS.RecordType
import DNS.Query

%default total

-- ============================================================================
-- DNS Response Codes (RFC 1035 Section 4.1.1)
-- ============================================================================

||| DNS response codes (RCODE field).
public export
data ResponseCode : Type where
  ||| No error condition (0).
  NOERROR  : ResponseCode
  ||| Format error -- server unable to interpret query (1).
  FORMERR  : ResponseCode
  ||| Server failure -- internal error (2).
  SERVFAIL : ResponseCode
  ||| Name error -- domain name does not exist (3).
  NXDOMAIN : ResponseCode
  ||| Not implemented -- server does not support query type (4).
  NOTIMP   : ResponseCode
  ||| Refused -- server refuses to perform operation (5).
  REFUSED  : ResponseCode

public export
Eq ResponseCode where
  NOERROR  == NOERROR  = True
  FORMERR  == FORMERR  = True
  SERVFAIL == SERVFAIL = True
  NXDOMAIN == NXDOMAIN = True
  NOTIMP   == NOTIMP   = True
  REFUSED  == REFUSED  = True
  _        == _        = False

public export
Show ResponseCode where
  show NOERROR  = "NOERROR"
  show FORMERR  = "FORMERR"
  show SERVFAIL = "SERVFAIL"
  show NXDOMAIN = "NXDOMAIN"
  show NOTIMP   = "NOTIMP"
  show REFUSED  = "REFUSED"

||| Convert a response code to its numeric value.
public export
rcodeToNat : ResponseCode -> Nat
rcodeToNat NOERROR  = 0
rcodeToNat FORMERR  = 1
rcodeToNat SERVFAIL = 2
rcodeToNat NXDOMAIN = 3
rcodeToNat NOTIMP   = 4
rcodeToNat REFUSED  = 5

||| Parse a numeric value to a response code.
public export
rcodeFromNat : Nat -> Maybe ResponseCode
rcodeFromNat 0 = Just NOERROR
rcodeFromNat 1 = Just FORMERR
rcodeFromNat 2 = Just SERVFAIL
rcodeFromNat 3 = Just NXDOMAIN
rcodeFromNat 4 = Just NOTIMP
rcodeFromNat 5 = Just REFUSED
rcodeFromNat _ = Nothing

-- ============================================================================
-- Resource Record data (RFC 1035 Section 3.2.1)
-- ============================================================================

||| The data payload of a resource record, typed by record type.
public export
data RData : Type where
  ||| A record: IPv4 address as four octets.
  RDataA     : (a : Bits8) -> (b : Bits8) -> (c : Bits8) -> (d : Bits8) -> RData
  ||| AAAA record: IPv6 address as eight 16-bit groups.
  RDataAAAA  : Vect 8 Bits16 -> RData
  ||| CNAME record: canonical name.
  RDataCNAME : DomainName -> RData
  ||| MX record: preference + exchange server.
  RDataMX    : (preference : Bits16) -> (exchange : DomainName) -> RData
  ||| NS record: authoritative name server.
  RDataNS    : DomainName -> RData
  ||| TXT record: text data.
  RDataTXT   : String -> RData
  ||| PTR record: pointer to a domain name.
  RDataPTR   : DomainName -> RData
  ||| SOA record: zone authority information.
  RDataSOA   : (mname : DomainName) -> (rname : DomainName)
            -> (serial : Bits32) -> (refresh : Bits32)
            -> (retry : Bits32) -> (expire : Bits32)
            -> (minimum : Bits32) -> RData
  ||| SRV record: service location.
  RDataSRV   : (priority : Bits16) -> (weight : Bits16)
            -> (port : Bits16) -> (target : DomainName) -> RData

public export
Show RData where
  show (RDataA a b c d) =
    show (cast {to=Nat} a) ++ "." ++ show (cast {to=Nat} b) ++ "."
    ++ show (cast {to=Nat} c) ++ "." ++ show (cast {to=Nat} d)
  show (RDataAAAA _)    = "<IPv6>"
  show (RDataCNAME n)   = show n
  show (RDataMX p e)    = show (cast {to=Nat} p) ++ " " ++ show e
  show (RDataNS n)      = show n
  show (RDataTXT t)     = "\"" ++ t ++ "\""
  show (RDataPTR n)     = show n
  show (RDataSOA m r s _ _ _ _) =
    show m ++ " " ++ show r ++ " " ++ show (cast {to=Nat} s)
  show (RDataSRV _ _ p t) =
    show (cast {to=Nat} p) ++ " " ++ show t

-- ============================================================================
-- Resource Record (RFC 1035 Section 3.2.1)
-- ============================================================================

||| A DNS resource record.
public export
record ResourceRecord where
  constructor MkRR
  ||| The domain name this record belongs to.
  rrName  : DomainName
  ||| The record type.
  rrType  : RecordType
  ||| The query class (almost always IN).
  rrClass : QueryClass
  ||| Time to live in seconds.
  rrTTL   : Bits32
  ||| The record-type-specific data.
  rrData  : RData

public export
Show ResourceRecord where
  show rr = show rr.rrName ++ " " ++ show (cast {to=Nat} rr.rrTTL)
            ++ " " ++ show rr.rrClass ++ " " ++ show rr.rrType
            ++ " " ++ show rr.rrData

-- ============================================================================
-- DNS Response message
-- ============================================================================

||| A complete DNS response message.
public export
record DNSResponse where
  constructor MkDNSResponse
  ||| Transaction ID matching the original query.
  transactionId      : Bits16
  ||| The response code (NOERROR, NXDOMAIN, etc.).
  rcode              : ResponseCode
  ||| Whether this is an authoritative answer (AA flag).
  authoritative      : Bool
  ||| Whether recursion was available (RA flag).
  recursionAvailable : Bool
  ||| Whether the response was truncated (TC flag).
  truncated          : Bool
  ||| The original questions section.
  questions          : List Question
  ||| Answer section: direct answers to the query.
  answers            : List ResourceRecord
  ||| Authority section: name servers for the queried zone.
  authority          : List ResourceRecord
  ||| Additional section: records that may help (e.g., glue records).
  additional         : List ResourceRecord

public export
Show DNSResponse where
  show r = "Response(id=" ++ show (cast {to=Nat} r.transactionId)
           ++ ", rcode=" ++ show r.rcode
           ++ ", answers=" ++ show (length r.answers)
           ++ ", authority=" ++ show (length r.authority)
           ++ ", additional=" ++ show (length r.additional) ++ ")"

-- ============================================================================
-- Response construction helpers
-- ============================================================================

||| Build a successful response with answer records.
public export
successResponse : (txId : Bits16) -> List Question -> List ResourceRecord -> DNSResponse
successResponse txId qs answers = MkDNSResponse
  { transactionId      = txId
  , rcode              = NOERROR
  , authoritative      = True
  , recursionAvailable = True
  , truncated          = False
  , questions          = qs
  , answers            = answers
  , authority          = []
  , additional         = []
  }

||| Build an NXDOMAIN response (name does not exist).
public export
nxdomainResponse : (txId : Bits16) -> List Question -> DNSResponse
nxdomainResponse txId qs = MkDNSResponse
  { transactionId      = txId
  , rcode              = NXDOMAIN
  , authoritative      = True
  , recursionAvailable = True
  , truncated          = False
  , questions          = qs
  , answers            = []
  , authority          = []
  , additional         = []
  }

||| Build a SERVFAIL response (server failure).
public export
servfailResponse : (txId : Bits16) -> List Question -> DNSResponse
servfailResponse txId qs = MkDNSResponse
  { transactionId      = txId
  , rcode              = SERVFAIL
  , authoritative      = False
  , recursionAvailable = False
  , truncated          = False
  , questions          = qs
  , answers            = []
  , authority          = []
  , additional         = []
  }

||| Whether the response indicates success.
public export
isSuccessful : DNSResponse -> Bool
isSuccessful r = r.rcode == NOERROR

||| Get the total number of resource records across all sections.
public export
totalRecords : DNSResponse -> Nat
totalRecords r = length r.answers + length r.authority + length r.additional
