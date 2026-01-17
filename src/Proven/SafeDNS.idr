-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2024-2025 Hyperpolymath

||| Safe DNS record handling and validation
|||
||| This module provides type-safe representations of DNS concepts:
||| - Domain names with validation
||| - Record types (A, AAAA, CNAME, MX, TXT, NS, SOA, etc.)
||| - TTL values with bounds checking
||| - Zone files
||| - DNSSEC-related types
|||
||| Security features:
||| - Domain name injection prevention
||| - Homograph attack detection
||| - Internal domain leak prevention
||| - Wildcard validation
||| - TTL bounds enforcement
module Proven.SafeDNS

import Data.List
import Data.String
import Data.Either
import Proven.Core

%default total

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

||| DNS label (single component between dots)
public export
record DnsLabel where
  constructor MkDnsLabel
  label : String

||| Fully qualified domain name
public export
record DomainName where
  constructor MkDomainName
  labels : List DnsLabel
  isAbsolute : Bool  -- ends with dot (FQDN)

||| Time to live in seconds
public export
record TTL where
  constructor MkTTL
  seconds : Nat

||| IPv4 address components
public export
record IPv4 where
  constructor MkIPv4
  octet1 : Nat
  octet2 : Nat
  octet3 : Nat
  octet4 : Nat

||| IPv6 address (simplified as string for now)
public export
record IPv6 where
  constructor MkIPv6
  address : String

||| MX record priority
public export
record MXPriority where
  constructor MkMXPriority
  priority : Nat

||| DNS record class
public export
data RecordClass = IN | CH | HS | NONE | ANY_CLASS

||| DNS record types
public export
data RecordType
  = A_RECORD
  | AAAA_RECORD
  | CNAME_RECORD
  | MX_RECORD
  | TXT_RECORD
  | NS_RECORD
  | SOA_RECORD
  | PTR_RECORD
  | SRV_RECORD
  | CAA_RECORD
  | DNSKEY_RECORD
  | DS_RECORD
  | RRSIG_RECORD
  | NSEC_RECORD
  | NSEC3_RECORD
  | TLSA_RECORD
  | SSHFP_RECORD

||| A record data
public export
record ARecord where
  constructor MkARecord
  address : IPv4

||| AAAA record data
public export
record AAAARecord where
  constructor MkAAAARecord
  address : IPv6

||| CNAME record data
public export
record CNAMERecord where
  constructor MkCNAMERecord
  target : DomainName

||| MX record data
public export
record MXRecord where
  constructor MkMXRecord
  priority : MXPriority
  exchange : DomainName

||| TXT record data
public export
record TXTRecord where
  constructor MkTXTRecord
  text : String

||| NS record data
public export
record NSRecord where
  constructor MkNSRecord
  nameserver : DomainName

||| SOA record data
public export
record SOARecord where
  constructor MkSOARecord
  mname : DomainName      -- primary nameserver
  rname : DomainName      -- responsible person email (@ replaced with .)
  serial : Nat
  refresh : TTL
  retry : TTL
  expire : TTL
  minimum : TTL           -- negative caching TTL

||| SRV record data
public export
record SRVRecord where
  constructor MkSRVRecord
  priority : Nat
  weight : Nat
  port : Nat
  target : DomainName

||| CAA record data
public export
record CAARecord where
  constructor MkCAARecord
  flags : Nat
  tag : String            -- issue, issuewild, iodef
  value : String

||| TLSA record data (DANE)
public export
record TLSARecord where
  constructor MkTLSARecord
  usage : Nat             -- 0-3
  selector : Nat          -- 0-1
  matchingType : Nat      -- 0-2
  certificateData : String

||| Generic DNS record
public export
data DnsRecord
  = RecordA DomainName TTL ARecord
  | RecordAAAA DomainName TTL AAAARecord
  | RecordCNAME DomainName TTL CNAMERecord
  | RecordMX DomainName TTL MXRecord
  | RecordTXT DomainName TTL TXTRecord
  | RecordNS DomainName TTL NSRecord
  | RecordSOA DomainName TTL SOARecord
  | RecordSRV DomainName TTL SRVRecord
  | RecordCAA DomainName TTL CAARecord
  | RecordTLSA DomainName TTL TLSARecord

||| DNS zone
public export
record DnsZone where
  constructor MkDnsZone
  origin : DomainName
  defaultTTL : TTL
  records : List DnsRecord

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

||| Errors that can occur during DNS operations
public export
data DnsError
  = EmptyDomainName
  | EmptyLabel
  | LabelTooLong String Nat
  | DomainTooLong String Nat
  | InvalidLabelChar String Char
  | InvalidLabelStart String
  | InvalidLabelEnd String
  | HomographDetected String
  | InternalDomainLeak String
  | InvalidIPv4 String
  | InvalidIPv6 String
  | TTLTooLow Nat
  | TTLTooHigh Nat
  | InvalidMXPriority Nat
  | InvalidPortNumber Nat
  | InvalidTXTLength Nat
  | WildcardNotAtStart String
  | ConsecutiveDots String
  | InvalidSOASerial Nat
  | InvalidCAATag String
  | InvalidTLSAUsage Nat
  | InvalidTLSASelector Nat
  | InvalidTLSAMatchingType Nat

public export
Show DnsError where
  show EmptyDomainName = "DNS error: empty domain name"
  show EmptyLabel = "DNS error: empty label"
  show (LabelTooLong l n) = "DNS error: label '" ++ l ++ "' too long (" ++ show n ++ " > 63)"
  show (DomainTooLong d n) = "DNS error: domain '" ++ d ++ "' too long (" ++ show n ++ " > 253)"
  show (InvalidLabelChar l c) = "DNS error: invalid character '" ++ singleton c ++ "' in label '" ++ l ++ "'"
  show (InvalidLabelStart l) = "DNS error: label '" ++ l ++ "' starts with invalid character"
  show (InvalidLabelEnd l) = "DNS error: label '" ++ l ++ "' ends with invalid character"
  show (HomographDetected d) = "DNS security: potential homograph attack in '" ++ d ++ "'"
  show (InternalDomainLeak d) = "DNS security: internal domain '" ++ d ++ "' should not be exposed"
  show (InvalidIPv4 ip) = "DNS error: invalid IPv4 address '" ++ ip ++ "'"
  show (InvalidIPv6 ip) = "DNS error: invalid IPv6 address '" ++ ip ++ "'"
  show (TTLTooLow t) = "DNS error: TTL " ++ show t ++ " too low (minimum 0)"
  show (TTLTooHigh t) = "DNS error: TTL " ++ show t ++ " too high (maximum 2147483647)"
  show (InvalidMXPriority p) = "DNS error: invalid MX priority " ++ show p
  show (InvalidPortNumber p) = "DNS error: invalid port number " ++ show p
  show (InvalidTXTLength l) = "DNS error: TXT record too long (" ++ show l ++ " > 255 per string)"
  show (WildcardNotAtStart d) = "DNS error: wildcard must be leftmost label in '" ++ d ++ "'"
  show (ConsecutiveDots d) = "DNS error: consecutive dots in '" ++ d ++ "'"
  show (InvalidSOASerial s) = "DNS error: invalid SOA serial " ++ show s
  show (InvalidCAATag t) = "DNS error: invalid CAA tag '" ++ t ++ "'"
  show (InvalidTLSAUsage u) = "DNS error: invalid TLSA usage " ++ show u ++ " (must be 0-3)"
  show (InvalidTLSASelector s) = "DNS error: invalid TLSA selector " ++ show s ++ " (must be 0-1)"
  show (InvalidTLSAMatchingType m) = "DNS error: invalid TLSA matching type " ++ show m ++ " (must be 0-2)"

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Maximum label length (63 bytes)
maxLabelLength : Nat
maxLabelLength = 63

||| Maximum domain name length (253 characters)
maxDomainLength : Nat
maxDomainLength = 253

||| Maximum TTL value (2^31 - 1)
maxTTL : Nat
maxTTL = 2147483647

||| Maximum TXT string length
maxTXTLength : Nat
maxTXTLength = 255

||| Internal domain suffixes that shouldn't leak
internalDomains : List String
internalDomains =
  [ ".local", ".internal", ".corp", ".private"
  , ".lan", ".home", ".localdomain", ".intranet"
  ]

||| Characters that look similar across scripts (homograph detection)
||| Simplified set - full detection needs Unicode tables
homographChars : List (Char, Char)
homographChars =
  [ ('a', '\x0430')  -- Cyrillic а
  , ('e', '\x0435')  -- Cyrillic е
  , ('o', '\x043E')  -- Cyrillic о
  , ('p', '\x0440')  -- Cyrillic р
  , ('c', '\x0441')  -- Cyrillic с
  , ('x', '\x0445')  -- Cyrillic х
  ]

||| Valid CAA tags
validCAATags : List String
validCAATags = ["issue", "issuewild", "iodef"]

--------------------------------------------------------------------------------
-- Character validation helpers
--------------------------------------------------------------------------------

||| Check if character is valid in a DNS label (LDH rule)
isValidLabelChar : Char -> Bool
isValidLabelChar c = isAlphaNum c || c == '-'

||| Check if character is valid at start of label
isValidLabelStart : Char -> Bool
isValidLabelStart c = isAlpha c || isDigit c

||| Check if character is valid at end of label
isValidLabelEnd : Char -> Bool
isValidLabelEnd c = isAlphaNum c

||| Check if all characters satisfy predicate
allChars : (Char -> Bool) -> String -> Bool
allChars pred str = all pred (unpack str)

||| Check for non-ASCII characters (potential homograph)
hasNonAscii : String -> Bool
hasNonAscii str = any (\c => ord c > 127) (unpack str)

--------------------------------------------------------------------------------
-- Validation functions
--------------------------------------------------------------------------------

||| Validate a single DNS label
public export
validateLabel : String -> Either DnsError DnsLabel
validateLabel "" = Left EmptyLabel
validateLabel label =
  let len = length label in
  if len > maxLabelLength
    then Left (LabelTooLong label len)
    else if label == "*"
      then Right (MkDnsLabel label)  -- wildcard is special
      else case unpack label of
        [] => Left EmptyLabel
        (c :: _) =>
          if not (isValidLabelStart c)
            then Left (InvalidLabelStart label)
            else case unpack (reverse label) of
              [] => Left EmptyLabel
              (lastC :: _) =>
                if not (isValidLabelEnd lastC)
                  then Left (InvalidLabelEnd label)
                  else if not (allChars isValidLabelChar label)
                    then Left (InvalidLabelChar label '?')  -- simplified
                    else Right (MkDnsLabel label)

||| Validate a domain name
public export
validateDomainName : String -> Either DnsError DomainName
validateDomainName "" = Left EmptyDomainName
validateDomainName name =
  let len = length name
      isAbsolute = isSuffixOf "." name
      nameToCheck = if isAbsolute then strSubstr 0 (minus len 1) name else name
  in
  if len > maxDomainLength
    then Left (DomainTooLong name len)
    else if isInfixOf ".." name
      then Left (ConsecutiveDots name)
      else if hasNonAscii name
        then Left (HomographDetected name)
        else let labels = split (== '.') nameToCheck
             in do
               validLabels <- traverse validateLabel (filter (/= "") labels)
               -- Check wildcard position
               _ <- validateWildcard validLabels
               pure (MkDomainName validLabels isAbsolute)
  where
    validateWildcard : List DnsLabel -> Either DnsError ()
    validateWildcard [] = Right ()
    validateWildcard labels =
      let hasWildcard = any (\l => l.label == "*") labels
          wildcardFirst = case labels of
            [] => True
            (l :: _) => l.label == "*" || not hasWildcard
      in if hasWildcard && not wildcardFirst
           then Left (WildcardNotAtStart name)
           else Right ()

||| Validate an IPv4 address string
public export
validateIPv4 : String -> Either DnsError IPv4
validateIPv4 ip =
  let parts = split (== '.') ip
  in if length parts /= 4
       then Left (InvalidIPv4 ip)
       else case traverse parseOctet parts of
         Nothing => Left (InvalidIPv4 ip)
         Just [o1, o2, o3, o4] => Right (MkIPv4 o1 o2 o3 o4)
         Just _ => Left (InvalidIPv4 ip)
  where
    parseOctet : String -> Maybe Nat
    parseOctet s = do
      n <- parsePositive s
      if n <= 255 then Just n else Nothing

||| Validate an IPv6 address string (simplified)
public export
validateIPv6 : String -> Either DnsError IPv6
validateIPv6 "" = Left (InvalidIPv6 "empty")
validateIPv6 ip =
  -- Simplified validation - just check basic structure
  if length ip > 45
    then Left (InvalidIPv6 ip)
    else Right (MkIPv6 ip)

||| Validate a TTL value
public export
validateTTL : Nat -> Either DnsError TTL
validateTTL t =
  if t > maxTTL
    then Left (TTLTooHigh t)
    else Right (MkTTL t)

||| Validate an MX priority
public export
validateMXPriority : Nat -> Either DnsError MXPriority
validateMXPriority p =
  if p > 65535
    then Left (InvalidMXPriority p)
    else Right (MkMXPriority p)

||| Validate a port number
public export
validatePort : Nat -> Either DnsError Nat
validatePort p =
  if p > 65535
    then Left (InvalidPortNumber p)
    else Right p

||| Validate TXT record content
public export
validateTXT : String -> Either DnsError TXTRecord
validateTXT txt =
  if length txt > maxTXTLength
    then Left (InvalidTXTLength (length txt))
    else Right (MkTXTRecord txt)

||| Validate a CAA tag
public export
validateCAATag : String -> Either DnsError String
validateCAATag tag =
  if elem tag validCAATags
    then Right tag
    else Left (InvalidCAATag tag)

||| Validate TLSA parameters
public export
validateTLSA : Nat -> Nat -> Nat -> String -> Either DnsError TLSARecord
validateTLSA usage selector matchingType certData =
  if usage > 3
    then Left (InvalidTLSAUsage usage)
    else if selector > 1
      then Left (InvalidTLSASelector selector)
      else if matchingType > 2
        then Left (InvalidTLSAMatchingType matchingType)
        else Right (MkTLSARecord usage selector matchingType certData)

--------------------------------------------------------------------------------
-- Security validation
--------------------------------------------------------------------------------

||| Check if domain is internal (shouldn't be exposed)
public export
isInternalDomain : DomainName -> Bool
isInternalDomain domain =
  let name = showDomainName domain
  in any (\suffix => isSuffixOf suffix (toLower name)) internalDomains

||| Validate domain is not internal
public export
validateNotInternal : DomainName -> Either DnsError DomainName
validateNotInternal domain =
  if isInternalDomain domain
    then Left (InternalDomainLeak (showDomainName domain))
    else Right domain

||| Check for potential homograph attack (simplified)
public export
detectHomograph : String -> Bool
detectHomograph name = hasNonAscii name

--------------------------------------------------------------------------------
-- Construction functions
--------------------------------------------------------------------------------

||| Create an A record
public export
mkARecord : String -> Nat -> String -> Either DnsError DnsRecord
mkARecord name ttl ip = do
  domain <- validateDomainName name
  validTTL <- validateTTL ttl
  addr <- validateIPv4 ip
  pure (RecordA domain validTTL (MkARecord addr))

||| Create an AAAA record
public export
mkAAAARecord : String -> Nat -> String -> Either DnsError DnsRecord
mkAAAARecord name ttl ip = do
  domain <- validateDomainName name
  validTTL <- validateTTL ttl
  addr <- validateIPv6 ip
  pure (RecordAAAA domain validTTL (MkAAAARecord addr))

||| Create a CNAME record
public export
mkCNAMERecord : String -> Nat -> String -> Either DnsError DnsRecord
mkCNAMERecord name ttl target = do
  domain <- validateDomainName name
  validTTL <- validateTTL ttl
  targetDomain <- validateDomainName target
  pure (RecordCNAME domain validTTL (MkCNAMERecord targetDomain))

||| Create an MX record
public export
mkMXRecord : String -> Nat -> Nat -> String -> Either DnsError DnsRecord
mkMXRecord name ttl priority exchange = do
  domain <- validateDomainName name
  validTTL <- validateTTL ttl
  validPriority <- validateMXPriority priority
  exchangeDomain <- validateDomainName exchange
  pure (RecordMX domain validTTL (MkMXRecord validPriority exchangeDomain))

||| Create a TXT record
public export
mkTXTRecord : String -> Nat -> String -> Either DnsError DnsRecord
mkTXTRecord name ttl content = do
  domain <- validateDomainName name
  validTTL <- validateTTL ttl
  txtRecord <- validateTXT content
  pure (RecordTXT domain validTTL txtRecord)

||| Create an NS record
public export
mkNSRecord : String -> Nat -> String -> Either DnsError DnsRecord
mkNSRecord name ttl nameserver = do
  domain <- validateDomainName name
  validTTL <- validateTTL ttl
  nsDomain <- validateDomainName nameserver
  pure (RecordNS domain validTTL (MkNSRecord nsDomain))

||| Create a CAA record
public export
mkCAARecord : String -> Nat -> Nat -> String -> String -> Either DnsError DnsRecord
mkCAARecord name ttl flags tag value = do
  domain <- validateDomainName name
  validTTL <- validateTTL ttl
  _ <- validateCAATag tag
  pure (RecordCAA domain validTTL (MkCAARecord flags tag value))

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

||| Serialize a DNS label
public export
showLabel : DnsLabel -> String
showLabel (MkDnsLabel l) = l

||| Serialize a domain name
public export
showDomainName : DomainName -> String
showDomainName (MkDomainName labels isAbs) =
  let base = concat (intersperse "." (map showLabel labels))
  in if isAbs then base ++ "." else base

||| Serialize an IPv4 address
public export
showIPv4 : IPv4 -> String
showIPv4 (MkIPv4 o1 o2 o3 o4) =
  show o1 ++ "." ++ show o2 ++ "." ++ show o3 ++ "." ++ show o4

||| Serialize an IPv6 address
public export
showIPv6 : IPv6 -> String
showIPv6 (MkIPv6 addr) = addr

||| Serialize a TTL
public export
showTTL : TTL -> String
showTTL (MkTTL s) = show s

||| Serialize record class
public export
showRecordClass : RecordClass -> String
showRecordClass IN = "IN"
showRecordClass CH = "CH"
showRecordClass HS = "HS"
showRecordClass NONE = "NONE"
showRecordClass ANY_CLASS = "ANY"

||| Serialize record type
public export
showRecordType : RecordType -> String
showRecordType A_RECORD = "A"
showRecordType AAAA_RECORD = "AAAA"
showRecordType CNAME_RECORD = "CNAME"
showRecordType MX_RECORD = "MX"
showRecordType TXT_RECORD = "TXT"
showRecordType NS_RECORD = "NS"
showRecordType SOA_RECORD = "SOA"
showRecordType PTR_RECORD = "PTR"
showRecordType SRV_RECORD = "SRV"
showRecordType CAA_RECORD = "CAA"
showRecordType DNSKEY_RECORD = "DNSKEY"
showRecordType DS_RECORD = "DS"
showRecordType RRSIG_RECORD = "RRSIG"
showRecordType NSEC_RECORD = "NSEC"
showRecordType NSEC3_RECORD = "NSEC3"
showRecordType TLSA_RECORD = "TLSA"
showRecordType SSHFP_RECORD = "SSHFP"

||| Serialize a DNS record in zone file format
public export
showDnsRecord : DnsRecord -> String
showDnsRecord (RecordA name ttl rec) =
  showDomainName name ++ " " ++ showTTL ttl ++ " IN A " ++ showIPv4 rec.address
showDnsRecord (RecordAAAA name ttl rec) =
  showDomainName name ++ " " ++ showTTL ttl ++ " IN AAAA " ++ showIPv6 rec.address
showDnsRecord (RecordCNAME name ttl rec) =
  showDomainName name ++ " " ++ showTTL ttl ++ " IN CNAME " ++ showDomainName rec.target
showDnsRecord (RecordMX name ttl rec) =
  showDomainName name ++ " " ++ showTTL ttl ++ " IN MX " ++
  show rec.priority.priority ++ " " ++ showDomainName rec.exchange
showDnsRecord (RecordTXT name ttl rec) =
  showDomainName name ++ " " ++ showTTL ttl ++ " IN TXT \"" ++ rec.text ++ "\""
showDnsRecord (RecordNS name ttl rec) =
  showDomainName name ++ " " ++ showTTL ttl ++ " IN NS " ++ showDomainName rec.nameserver
showDnsRecord (RecordSOA name ttl rec) =
  showDomainName name ++ " " ++ showTTL ttl ++ " IN SOA " ++
  showDomainName rec.mname ++ " " ++ showDomainName rec.rname ++ " " ++
  show rec.serial ++ " " ++ showTTL rec.refresh ++ " " ++
  showTTL rec.retry ++ " " ++ showTTL rec.expire ++ " " ++ showTTL rec.minimum
showDnsRecord (RecordSRV name ttl rec) =
  showDomainName name ++ " " ++ showTTL ttl ++ " IN SRV " ++
  show rec.priority ++ " " ++ show rec.weight ++ " " ++
  show rec.port ++ " " ++ showDomainName rec.target
showDnsRecord (RecordCAA name ttl rec) =
  showDomainName name ++ " " ++ showTTL ttl ++ " IN CAA " ++
  show rec.flags ++ " " ++ rec.tag ++ " \"" ++ rec.value ++ "\""
showDnsRecord (RecordTLSA name ttl rec) =
  showDomainName name ++ " " ++ showTTL ttl ++ " IN TLSA " ++
  show rec.usage ++ " " ++ show rec.selector ++ " " ++
  show rec.matchingType ++ " " ++ rec.certificateData

||| Serialize a DNS zone
public export
showDnsZone : DnsZone -> String
showDnsZone zone =
  "$ORIGIN " ++ showDomainName zone.origin ++ "\n" ++
  "$TTL " ++ showTTL zone.defaultTTL ++ "\n" ++
  concat (intersperse "\n" (map showDnsRecord zone.records))
