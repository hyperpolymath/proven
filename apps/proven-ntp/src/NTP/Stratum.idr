-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- NTP Stratum Levels (RFC 5905 Section 7.3)
--
-- The stratum field indicates the distance from the reference clock.
-- Stratum 0 is reserved for unspecified/kiss-o'-death packets, stratum 1
-- is a primary reference (directly connected to a reference clock like GPS),
-- strata 2-15 are secondary references, and stratum 16 means unsynchronised.
-- Values 17-255 are reserved.

module NTP.Stratum

%default total

-- ============================================================================
-- Stratum levels
-- ============================================================================

||| NTP stratum levels as defined in RFC 5905 Section 7.3.
public export
data Stratum : Type where
  ||| Stratum 0: Unspecified or kiss-o'-death packet.
  ||| Used in KoD responses to indicate rate limiting or access denial.
  Unspecified        : Stratum
  ||| Stratum 1: Primary reference.
  ||| Directly connected to a reference clock (GPS, WWVB, atomic clock, etc.).
  PrimaryReference   : Stratum
  ||| Stratum 2-15: Secondary reference.
  ||| Synchronised via NTP to a server at stratum n-1.
  ||| The Nat parameter is the raw stratum value (2-15).
  SecondaryReference : (level : Nat) -> Stratum
  ||| Stratum 16: Unsynchronised.
  ||| The server's clock is not synchronised to any reference.
  Unsynchronised     : Stratum

public export
Eq Stratum where
  Unspecified            == Unspecified            = True
  PrimaryReference       == PrimaryReference       = True
  (SecondaryReference a) == (SecondaryReference b) = a == b
  Unsynchronised         == Unsynchronised         = True
  _                      == _                      = False

public export
Show Stratum where
  show Unspecified            = "Stratum 0 (Unspecified/KoD)"
  show PrimaryReference       = "Stratum 1 (Primary Reference)"
  show (SecondaryReference n) = "Stratum " ++ show n ++ " (Secondary Reference)"
  show Unsynchronised         = "Stratum 16 (Unsynchronised)"

public export
Ord Stratum where
  compare Unspecified        Unspecified        = EQ
  compare Unspecified        _                  = LT
  compare PrimaryReference   Unspecified        = GT
  compare PrimaryReference   PrimaryReference   = EQ
  compare PrimaryReference   _                  = LT
  compare (SecondaryReference _) Unspecified     = GT
  compare (SecondaryReference _) PrimaryReference = GT
  compare (SecondaryReference a) (SecondaryReference b) = compare a b
  compare (SecondaryReference _) Unsynchronised  = LT
  compare Unsynchronised     Unsynchronised     = EQ
  compare Unsynchronised     _                  = GT

-- ============================================================================
-- Numeric code conversion
-- ============================================================================

||| Convert a stratum to its byte value (0-16).
public export
stratumCode : Stratum -> Bits8
stratumCode Unspecified            = 0
stratumCode PrimaryReference       = 1
stratumCode (SecondaryReference n) = cast n
stratumCode Unsynchronised         = 16

||| Decode a byte value to a stratum level.
||| Values 0-16 are mapped to their corresponding constructors.
||| Values 17-255 are mapped to Unsynchronised (reserved, treated as invalid).
public export
stratumFromCode : Bits8 -> Stratum
stratumFromCode 0  = Unspecified
stratumFromCode 1  = PrimaryReference
stratumFromCode 16 = Unsynchronised
stratumFromCode n  =
  let nat = cast {to=Nat} n
  in if nat >= 2 && nat <= 15
       then SecondaryReference nat
       else Unsynchronised  -- 17-255 are reserved

-- ============================================================================
-- Stratum classification and properties
-- ============================================================================

||| Whether the stratum indicates a usable time source.
||| Unspecified (KoD) and Unsynchronised are not usable for time sync.
public export
isUsable : Stratum -> Bool
isUsable Unspecified      = False
isUsable Unsynchronised   = False
isUsable _                = True

||| Whether the stratum indicates a primary reference (directly connected
||| to a reference clock such as GPS, PPS, or radio).
public export
isPrimary : Stratum -> Bool
isPrimary PrimaryReference = True
isPrimary _                = False

||| Get the distance from the primary reference in hops.
||| Returns Nothing for Unspecified and Unsynchronised.
public export
hopsFromPrimary : Stratum -> Maybe Nat
hopsFromPrimary Unspecified            = Nothing
hopsFromPrimary PrimaryReference       = Just 0
hopsFromPrimary (SecondaryReference n) = Just (minus n 1)
hopsFromPrimary Unsynchronised         = Nothing

||| The maximum valid secondary reference stratum.
public export
maxStratum : Nat
maxStratum = 15

-- ============================================================================
-- Reference identifiers for stratum 1 (RFC 5905 Section 7.3)
-- ============================================================================

||| Common reference identifier codes for stratum 1 servers.
||| These are 4-character ASCII strings packed into Bits32.
public export
data RefIdCode : Type where
  ||| GOES satellite receiver.
  GOES : RefIdCode
  ||| GPS satellite receiver.
  GPS  : RefIdCode
  ||| Pulse Per Second (generic).
  PPS  : RefIdCode
  ||| CDMA mobile phone timing reference.
  CDMA : RefIdCode
  ||| GLONASS satellite receiver.
  GLO  : RefIdCode
  ||| Galileo satellite receiver.
  GAL  : RefIdCode
  ||| NIST telephone modem service.
  NIST : RefIdCode
  ||| USNO telephone modem service.
  USNO : RefIdCode
  ||| Inter-Range Instrumentation Group.
  IRIG : RefIdCode
  ||| WWV radio.
  WWV  : RefIdCode
  ||| WWVB radio.
  WWVB : RefIdCode
  ||| WWVH radio.
  WWVH : RefIdCode
  ||| Other/unknown reference identifier.
  OtherRef : (code : String) -> RefIdCode

public export
Show RefIdCode where
  show GOES = "GOES"
  show GPS  = "GPS"
  show PPS  = "PPS"
  show CDMA = "CDMA"
  show GLO  = "GLO"
  show GAL  = "GAL"
  show NIST = "NIST"
  show USNO = "USNO"
  show IRIG = "IRIG"
  show WWV  = "WWV"
  show WWVB = "WWVB"
  show WWVH = "WWVH"
  show (OtherRef s) = s

||| Parse a 4-character reference identifier string.
public export
refIdFromString : String -> RefIdCode
refIdFromString "GOES" = GOES
refIdFromString "GPS"  = GPS
refIdFromString "PPS"  = PPS
refIdFromString "CDMA" = CDMA
refIdFromString "GLO"  = GLO
refIdFromString "GAL"  = GAL
refIdFromString "NIST" = NIST
refIdFromString "USNO" = USNO
refIdFromString "IRIG" = IRIG
refIdFromString "WWV"  = WWV
refIdFromString "WWVB" = WWVB
refIdFromString "WWVH" = WWVH
refIdFromString s      = OtherRef s

||| Describe what reference clock a stratum 1 identifier represents.
public export
refIdDescription : RefIdCode -> String
refIdDescription GOES = "Geosynchronous Orbit Environment Satellite"
refIdDescription GPS  = "Global Positioning System"
refIdDescription PPS  = "Generic Pulse-Per-Second"
refIdDescription CDMA = "CDMA Reference (mobile network)"
refIdDescription GLO  = "GLONASS Satellite"
refIdDescription GAL  = "Galileo Satellite"
refIdDescription NIST = "NIST Telephone Modem"
refIdDescription USNO = "USNO Telephone Modem"
refIdDescription IRIG = "Inter-Range Instrumentation Group"
refIdDescription WWV  = "WWV Radio (Fort Collins, CO)"
refIdDescription WWVB = "WWVB Radio (Fort Collins, CO)"
refIdDescription WWVH = "WWVH Radio (Kauai, HI)"
refIdDescription (OtherRef _) = "Unknown Reference Clock"
