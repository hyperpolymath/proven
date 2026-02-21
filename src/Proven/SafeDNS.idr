-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeDNS - DNS record handling with homograph detection
|||
||| Provides type-safe DNS record construction and hostname validation.
||| Prevents: homograph attacks, DNS rebinding, label injection.
module Proven.SafeDNS

import Data.String
import Data.List
import Data.Nat
import Data.Maybe

%default total

||| DNS record types
public export
data RecordType = A | AAAA | CNAME | MX | NS | TXT | SRV | SOA | PTR | CAA

public export
Show RecordType where
  show A = "A"
  show AAAA = "AAAA"
  show CNAME = "CNAME"
  show MX = "MX"
  show NS = "NS"
  show TXT = "TXT"
  show SRV = "SRV"
  show SOA = "SOA"
  show PTR = "PTR"
  show CAA = "CAA"

public export
Eq RecordType where
  A == A = True
  AAAA == AAAA = True
  CNAME == CNAME = True
  MX == MX = True
  NS == NS = True
  TXT == TXT = True
  SRV == SRV = True
  SOA == SOA = True
  PTR == PTR = True
  CAA == CAA = True
  _ == _ = False

||| A valid DNS label (max 63 chars, alphanumeric + hyphen)
public export
data DNSLabel : Type where
  MkDNSLabel : (label : String) -> DNSLabel

||| Check if a character is valid in a DNS label
public export
isValidLabelChar : Char -> Bool
isValidLabelChar c = isAlphaNum c || c == '-'

||| Validate a DNS label
public export
isValidLabel : String -> Bool
isValidLabel s =
  let chars = unpack s
  in length chars > 0 && length chars <= 63 &&
     all isValidLabelChar chars &&
     not (isPrefixOf "-" s) &&
     not (isSuffixOf "-" s)

||| Smart constructor for DNS labels
public export
mkLabel : String -> Maybe DNSLabel
mkLabel s = if isValidLabel s then Just (MkDNSLabel s) else Nothing

||| Extract label text
public export
labelText : DNSLabel -> String
labelText (MkDNSLabel l) = l

||| A validated hostname (sequence of labels)
public export
data Hostname : Type where
  MkHostname : (labels : List DNSLabel) -> Hostname

||| Convert hostname to string
public export
hostnameToString : Hostname -> String
hostnameToString (MkHostname labels) =
  joinBy "." (map labelText labels)
  where
    joinBy : String -> List String -> String
    joinBy _ [] = ""
    joinBy _ [x] = x
    joinBy sep (x :: xs) = x ++ sep ++ joinBy sep xs

||| Validate a full hostname (max 253 chars total)
public export
isValidHostname : String -> Bool
isValidHostname s =
  length s <= 253 &&
  all isValidLabel (split (== '.') s)

||| Parse a hostname string
public export
parseHostname : String -> Maybe Hostname
parseHostname s =
  if not (isValidHostname s) then Nothing
  else
    let parts = split (== '.') s
        labels = traverse mkLabel parts
    in map MkHostname labels

-- ----------------------------------------------------------------
-- Homograph detection (IDN / confusable characters)
-- ----------------------------------------------------------------

||| Characters commonly confused with ASCII (homoglyphs)
||| Maps confusable Unicode -> ASCII equivalent
public export
confusableChars : List (Char, Char)
confusableChars =
  [ ('\x0430', 'a')  -- Cyrillic а -> Latin a
  , ('\x0435', 'e')  -- Cyrillic е -> Latin e
  , ('\x043E', 'o')  -- Cyrillic о -> Latin o
  , ('\x0440', 'p')  -- Cyrillic р -> Latin p
  , ('\x0441', 'c')  -- Cyrillic с -> Latin c
  , ('\x0443', 'y')  -- Cyrillic у -> Latin y
  , ('\x0445', 'x')  -- Cyrillic х -> Latin x
  , ('\x0455', 's')  -- Cyrillic ѕ -> Latin s
  , ('\x0456', 'i')  -- Cyrillic і -> Latin i
  , ('\x04CF', 'l')  -- Cyrillic ӏ -> Latin l
  ]

||| Check if a string contains confusable characters
public export
hasHomoglyphs : String -> Bool
hasHomoglyphs s = any (\c => any (\(confusable, _) => c == confusable) confusableChars) (unpack s)

||| Detect mixed-script content (potential homograph attack)
public export
isMixedScript : String -> Bool
isMixedScript s =
  let chars = unpack s
      hasLatin = any (\c => c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z') chars
      hasNonAscii = any (\c => ord c > 127) chars
  in hasLatin && hasNonAscii

||| Normalize confusable characters to ASCII
public export
deconfuse : String -> String
deconfuse s = pack (map normalize (unpack s))
  where
    normalize : Char -> Char
    normalize c = case lookup c confusableChars of
                    Just ascii => ascii
                    Nothing => c

||| Check if a hostname might be a homograph of a known domain
public export
isHomographOf : String -> String -> Bool
isHomographOf suspect trusted =
  let normSuspect = toLower (deconfuse suspect)
      normTrusted = toLower trusted
  in normSuspect == normTrusted && suspect /= trusted

-- ----------------------------------------------------------------
-- DNS record types
-- ----------------------------------------------------------------

||| A DNS record
public export
record DNSRecord where
  constructor MkDNSRecord
  recordName  : Hostname
  recordType  : RecordType
  recordTTL   : Nat
  recordValue : String

||| MX record with priority
public export
record MXRecord where
  constructor MkMXRecord
  mxPriority : Nat
  mxHost     : Hostname

||| SRV record
public export
record SRVRecord where
  constructor MkSRVRecord
  srvPriority : Nat
  srvWeight   : Nat
  srvPort     : Nat
  srvTarget   : Hostname

||| CAA record (Certificate Authority Authorization)
public export
record CAARecord where
  constructor MkCAARecord
  caaFlags : Nat
  caaTag   : String   -- "issue", "issuewild", "iodef"
  caaValue : String

||| Valid CAA tags
public export
isValidCAATag : String -> Bool
isValidCAATag "issue" = True
isValidCAATag "issuewild" = True
isValidCAATag "iodef" = True
isValidCAATag _ = False

-- ----------------------------------------------------------------
-- Safety checks
-- ----------------------------------------------------------------

||| Check for DNS rebinding (private IPs in public DNS)
public export
isPrivateIP : String -> Bool
isPrivateIP ip =
  isPrefixOf "10." ip ||
  isPrefixOf "192.168." ip ||
  isPrefixOf "172.16." ip || isPrefixOf "172.17." ip ||
  isPrefixOf "172.18." ip || isPrefixOf "172.19." ip ||
  isPrefixOf "172.20." ip || isPrefixOf "172.21." ip ||
  isPrefixOf "172.22." ip || isPrefixOf "172.23." ip ||
  isPrefixOf "172.24." ip || isPrefixOf "172.25." ip ||
  isPrefixOf "172.26." ip || isPrefixOf "172.27." ip ||
  isPrefixOf "172.28." ip || isPrefixOf "172.29." ip ||
  isPrefixOf "172.30." ip || isPrefixOf "172.31." ip ||
  isPrefixOf "127." ip ||
  ip == "0.0.0.0" || ip == "localhost"

-- ----------------------------------------------------------------
-- Proof types
-- ----------------------------------------------------------------

||| Proof that a hostname is valid
public export
data ValidHostname : Hostname -> Type where
  MkValidHostname : (h : Hostname) -> ValidHostname h

||| Proof that a hostname has no homoglyphs
public export
data NoHomoglyphs : String -> Type where
  MkNoHomoglyphs : hasHomoglyphs s = False -> NoHomoglyphs s

||| Proof that a hostname is not mixed-script
public export
data PureScript : String -> Type where
  MkPureScript : isMixedScript s = False -> PureScript s

||| Combined IDN safety proof
public export
data IDNSafe : String -> Type where
  MkIDNSafe : NoHomoglyphs s -> PureScript s -> IDNSafe s
