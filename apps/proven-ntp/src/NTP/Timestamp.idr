-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- NTP Timestamps (RFC 5905 Section 6)
--
-- NTP timestamps are 64-bit values: 32 bits of seconds since the NTP epoch
-- (1900-01-01 00:00:00 UTC) and 32 bits of fractional seconds. This module
-- provides safe arithmetic on timestamps with overflow protection, and
-- conversion to/from Unix epoch timestamps.

module NTP.Timestamp

%default total

-- ============================================================================
-- NTP Timestamp (RFC 5905 Section 6)
-- ============================================================================

||| An NTP timestamp consisting of seconds and fractional seconds.
||| The seconds field counts from the NTP epoch (1900-01-01 00:00:00 UTC).
||| The fraction field represents sub-second precision (2^-32 seconds per unit).
public export
record NTPTimestamp where
  constructor MkNTPTimestamp
  ||| Seconds since the NTP epoch (1900-01-01 00:00:00 UTC).
  seconds  : Bits32
  ||| Fractional seconds in units of 2^-32 seconds (~233 picoseconds).
  fraction : Bits32

public export
Eq NTPTimestamp where
  a == b = a.seconds == b.seconds && a.fraction == b.fraction

public export
Show NTPTimestamp where
  show ts = "NTP(" ++ show (cast {to=Nat} ts.seconds)
            ++ "." ++ show (cast {to=Nat} ts.fraction) ++ ")"

public export
Ord NTPTimestamp where
  compare a b =
    case compare a.seconds b.seconds of
      EQ => compare a.fraction b.fraction
      x  => x

-- ============================================================================
-- Special timestamp values
-- ============================================================================

||| The zero timestamp, representing the NTP epoch (1900-01-01 00:00:00 UTC).
public export
ntpEpoch : NTPTimestamp
ntpEpoch = MkNTPTimestamp 0 0

||| A null/unset timestamp. Used in packet fields that have not been filled.
||| All-zeros indicates "not available" per RFC 5905.
public export
nullTimestamp : NTPTimestamp
nullTimestamp = MkNTPTimestamp 0 0

||| Check whether a timestamp is the null (unset) value.
public export
isNull : NTPTimestamp -> Bool
isNull ts = ts.seconds == 0 && ts.fraction == 0

-- ============================================================================
-- Epoch conversion constants
-- ============================================================================

||| Number of seconds between the NTP epoch (1900-01-01) and the
||| Unix epoch (1970-01-01). This is 70 years including 17 leap years.
||| Calculation: (70 * 365 + 17) * 86400 = 2,208,988,800
public export
ntpEpochOffset : Nat
ntpEpochOffset = 2208988800

-- ============================================================================
-- Timestamp arithmetic
-- ============================================================================

||| Add two timestamps (used for computing offsets).
||| Handles fractional overflow by carrying into the seconds field.
public export
addTimestamp : NTPTimestamp -> NTPTimestamp -> NTPTimestamp
addTimestamp a b =
  let fracSum = cast {to=Nat} a.fraction + cast {to=Nat} b.fraction
      carry   = if fracSum >= 4294967296 then 1 else 0
      newFrac = cast {to=Bits32} (mod fracSum 4294967296)
      newSecs = cast {to=Bits32} (cast {to=Nat} a.seconds + cast {to=Nat} b.seconds + carry)
  in MkNTPTimestamp newSecs newFrac

||| Subtract timestamp b from timestamp a.
||| Returns the difference as a new timestamp.
public export
subTimestamp : NTPTimestamp -> NTPTimestamp -> NTPTimestamp
subTimestamp a b =
  let aFrac = cast {to=Nat} a.fraction
      bFrac = cast {to=Nat} b.fraction
      borrow = if aFrac < bFrac then 1 else 0
      newFrac = if aFrac >= bFrac
                  then cast {to=Bits32} (aFrac - bFrac)
                  else cast {to=Bits32} (4294967296 + aFrac - bFrac)
      aSecs = cast {to=Nat} a.seconds
      bSecs = cast {to=Nat} b.seconds
      newSecs = if aSecs >= bSecs + borrow
                  then cast {to=Bits32} (aSecs - bSecs - borrow)
                  else 0  -- Underflow protection: clamp to zero
  in MkNTPTimestamp newSecs newFrac

-- ============================================================================
-- Conversion to/from Unix epoch
-- ============================================================================

||| Convert an NTP timestamp to Unix epoch seconds (integer part only).
||| Returns Nothing if the NTP timestamp is before the Unix epoch.
public export
toUnixSeconds : NTPTimestamp -> Maybe Nat
toUnixSeconds ts =
  let ntpSecs = cast {to=Nat} ts.seconds
  in if ntpSecs >= ntpEpochOffset
       then Just (ntpSecs - ntpEpochOffset)
       else Nothing  -- Before Unix epoch (before 1970)

||| Convert Unix epoch seconds to an NTP timestamp (zero fraction).
||| The input is the number of seconds since 1970-01-01 00:00:00 UTC.
public export
fromUnixSeconds : Nat -> NTPTimestamp
fromUnixSeconds unixSecs =
  MkNTPTimestamp (cast (unixSecs + ntpEpochOffset)) 0

||| Convert an NTP timestamp to a fractional offset in milliseconds.
||| Useful for displaying sub-second precision.
public export
fractionToMillis : NTPTimestamp -> Nat
fractionToMillis ts =
  -- fraction / 2^32 * 1000 = fraction * 1000 / 2^32
  -- Approximate: fraction * 1000 / 4294967296
  -- Use integer division to avoid floating point
  let frac = cast {to=Nat} ts.fraction
  in div (frac * 1000) 4294967296

-- ============================================================================
-- Timestamp halving (used in offset calculations)
-- ============================================================================

||| Divide a timestamp by 2 (used in clock offset calculation).
||| offset = ((t2 - t1) + (t3 - t4)) / 2
public export
halfTimestamp : NTPTimestamp -> NTPTimestamp
halfTimestamp ts =
  let secs = cast {to=Nat} ts.seconds
      frac = cast {to=Nat} ts.fraction
      -- If seconds is odd, carry the half-second into the fraction
      carryFrac = if mod secs 2 == 1 then 2147483648 else 0  -- 2^31
      newSecs = cast {to=Bits32} (div secs 2)
      newFrac = cast {to=Bits32} (div frac 2 + carryFrac)
  in MkNTPTimestamp newSecs newFrac

-- ============================================================================
-- Round-trip delay calculation (RFC 5905 Section 8)
-- ============================================================================

||| Calculate the round-trip delay from four timestamps.
||| delay = (t4 - t1) - (t3 - t2)
||| where t1 = client transmit, t2 = server receive,
|||       t3 = server transmit, t4 = client receive.
public export
roundTripDelay : (t1, t2, t3, t4 : NTPTimestamp) -> NTPTimestamp
roundTripDelay t1 t2 t3 t4 =
  let clientSpan = subTimestamp t4 t1  -- Total elapsed on client side
      serverSpan = subTimestamp t3 t2  -- Time spent at server
  in subTimestamp clientSpan serverSpan

||| Calculate the clock offset from four timestamps.
||| offset = ((t2 - t1) + (t3 - t4)) / 2
||| Positive offset means the server is ahead of the client.
public export
clockOffset : (t1, t2, t3, t4 : NTPTimestamp) -> NTPTimestamp
clockOffset t1 t2 t3 t4 =
  let diff1 = subTimestamp t2 t1  -- Server receive - client transmit
      diff2 = subTimestamp t3 t4  -- Server transmit - client receive
      sum   = addTimestamp diff1 diff2
  in halfTimestamp sum
