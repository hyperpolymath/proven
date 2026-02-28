-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- DNS Zone Management (RFC 1035 Section 6)
--
-- A zone represents a contiguous portion of the DNS namespace under
-- a single administrative authority. Zones contain an SOA record,
-- NS records, and the resource records for names within the zone.
-- Zone validation ensures structural integrity at construction time.

module DNS.Zone

import DNS.Name
import DNS.RecordType
import DNS.Query
import DNS.Response

%default total

-- ============================================================================
-- Zone record
-- ============================================================================

||| A DNS zone: a collection of resource records under a common origin.
public export
record Zone where
  constructor MkZone
  ||| The zone origin (e.g. "example.com").
  origin    : DomainName
  ||| The SOA record for this zone (required, exactly one).
  soa       : ResourceRecord
  ||| Name server records for this zone.
  nsRecords : List ResourceRecord
  ||| All other resource records in the zone.
  records   : List ResourceRecord

public export
Show Zone where
  show z = "Zone(" ++ show z.origin
           ++ ", ns=" ++ show (length z.nsRecords)
           ++ ", records=" ++ show (length z.records) ++ ")"

-- ============================================================================
-- Zone validation errors
-- ============================================================================

||| Errors detected during zone validation.
public export
data ZoneError : Type where
  ||| The SOA record is missing or has the wrong type.
  MissingSOA       : ZoneError
  ||| No NS records are defined for the zone.
  MissingNS        : ZoneError
  ||| A record's name is not within the zone origin.
  OutOfZone        : (name : DomainName) -> (origin : DomainName) -> ZoneError
  ||| Duplicate records detected (same name, type, class, data).
  DuplicateRecord  : (name : DomainName) -> (rtype : RecordType) -> ZoneError
  ||| CNAME record coexists with other records at the same name.
  CnameConflict    : (name : DomainName) -> ZoneError
  ||| SOA serial number is zero (likely uninitialised).
  ZeroSerial       : ZoneError

public export
Show ZoneError where
  show MissingSOA          = "Zone has no SOA record"
  show MissingNS           = "Zone has no NS records"
  show (OutOfZone n o)     = show n ++ " is outside zone " ++ show o
  show (DuplicateRecord n t) = "Duplicate " ++ show t ++ " record for " ++ show n
  show (CnameConflict n)  = "CNAME at " ++ show n ++ " conflicts with other records"
  show ZeroSerial          = "SOA serial number is zero"

-- ============================================================================
-- Zone validation
-- ============================================================================

||| Check if a domain name is within a zone's origin.
public export
isInZone : DomainName -> Zone -> Bool
isInZone name zone = name == zone.origin || isSubdomainOf name zone.origin

||| Validate a zone for structural correctness.
||| Returns a list of validation errors (empty = valid).
public export
validateZone : Zone -> List ZoneError
validateZone zone =
  let errors1 = checkSOA zone
      errors2 = checkNS zone
      errors3 = checkRecordsInZone zone
      errors4 = checkCnameConflicts zone
  in errors1 ++ errors2 ++ errors3 ++ errors4
  where
    ||| Check SOA record validity.
    checkSOA : Zone -> List ZoneError
    checkSOA z = case z.soa.rrType of
      SOA => case z.soa.rrData of
               RDataSOA _ _ serial _ _ _ _ =>
                 if serial == 0 then [ZeroSerial] else []
               _ => [MissingSOA]
      _   => [MissingSOA]

    ||| Check that NS records are present.
    checkNS : Zone -> List ZoneError
    checkNS z = if length z.nsRecords == 0 then [MissingNS] else []

    ||| Check that all records are within the zone.
    checkRecordsInZone : Zone -> List ZoneError
    checkRecordsInZone z =
      mapMaybe (\rr =>
        if isInZone rr.rrName z
          then Nothing
          else Just (OutOfZone rr.rrName z.origin)
      ) z.records

    ||| Check for CNAME conflicts (CNAME must not coexist with other types).
    checkCnameConflicts : Zone -> List ZoneError
    checkCnameConflicts z =
      let cnameNames = mapMaybe (\rr =>
            case rr.rrType of
              CNAME => Just rr.rrName
              _     => Nothing
            ) z.records
          otherNames = mapMaybe (\rr =>
            case rr.rrType of
              CNAME => Nothing
              _     => Just rr.rrName
            ) z.records
      in mapMaybe (\cn =>
           if any (== cn) otherNames
             then Just (CnameConflict cn)
             else Nothing
         ) cnameNames

-- ============================================================================
-- Zone query resolution
-- ============================================================================

||| Look up resource records matching a name and type within a zone.
public export
lookupInZone : DomainName -> RecordType -> Zone -> List ResourceRecord
lookupInZone name rtype zone =
  filter (\rr => rr.rrName == name && rr.rrType == rtype) zone.records

||| Look up all records for a given name within a zone.
public export
lookupAllForName : DomainName -> Zone -> List ResourceRecord
lookupAllForName name zone =
  filter (\rr => rr.rrName == name) zone.records

-- ============================================================================
-- Zone construction helpers
-- ============================================================================

||| Build a minimal zone with SOA and NS records.
public export
mkZone : (origin : DomainName) -> (mname : DomainName) -> (rname : DomainName)
       -> (serial : Bits32) -> (nsList : List DomainName) -> Zone
mkZone origin mname rname serial nsList =
  let soaRR = MkRR
        { rrName  = origin
        , rrType  = SOA
        , rrClass = IN
        , rrTTL   = 86400
        , rrData  = RDataSOA mname rname serial 3600 900 604800 86400
        }
      nsRRs = map (\ns => MkRR
        { rrName  = origin
        , rrType  = NS
        , rrClass = IN
        , rrTTL   = 86400
        , rrData  = RDataNS ns
        }) nsList
  in MkZone
    { origin    = origin
    , soa       = soaRR
    , nsRecords = nsRRs
    , records   = []
    }

||| Add a resource record to a zone.
public export
addRecord : ResourceRecord -> Zone -> Zone
addRecord rr zone = { records $= (rr ::) } zone

||| Get the total number of records in a zone (including SOA and NS).
public export
totalZoneRecords : Zone -> Nat
totalZoneRecords z = 1 + length z.nsRecords + length z.records
