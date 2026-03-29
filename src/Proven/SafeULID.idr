-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeULID - Universally Unique Lexicographically Sortable Identifier
|||
||| Type-safe ULID handling per the ULID spec:
||| - 48-bit timestamp (millisecond precision, epoch ms)
||| - 80-bit randomness
||| - Crockford's Base32 encoding (26 characters)
||| - Lexicographic sort = chronological sort (monotonicity)
||| Suitable for Agda cross-verification (monotonicity proof).
module Proven.SafeULID

import Data.String
import Data.List
import Data.Nat

%default total

-- ============================================================================
-- CROCKFORD'S BASE32
-- ============================================================================

||| Crockford's Base32 alphabet (excludes I, L, O, U)
crockfordAlphabet : String
crockfordAlphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

||| Check if a character is valid Crockford's Base32
public export
isCrockfordChar : Char -> Bool
isCrockfordChar c =
  let u = toUpper c
  in (u >= '0' && u <= '9') || (u >= 'A' && u <= 'Z' && u /= 'I' && u /= 'L' && u /= 'O' && u /= 'U')

-- ============================================================================
-- ULID TYPE
-- ============================================================================

||| ULID length is always 26 Crockford's Base32 characters
public export
ULIDLength : Nat
ULIDLength = 26

||| A validated ULID
public export
record ULID where
  constructor MkULID
  encoded : String  -- 26 char Crockford's Base32

public export
Show ULID where
  show u = u.encoded

public export
Eq ULID where
  a == b = a.encoded == b.encoded

||| Lexicographic order = chronological order (monotonicity property)
public export
Ord ULID where
  compare a b = compare a.encoded b.encoded

-- ============================================================================
-- PARSING AND VALIDATION
-- ============================================================================

||| Validate and parse a ULID string
public export
parseULID : String -> Maybe ULID
parseULID s =
  let upper = pack (map toUpper (unpack s))
  in if length upper == ULIDLength && all isCrockfordChar (unpack upper)
       then Just (MkULID upper)
       else Nothing

||| Check if a string is a valid ULID
public export
isValidULID : String -> Bool
isValidULID s = length s == ULIDLength && all isCrockfordChar (unpack s)

-- ============================================================================
-- TIMESTAMP EXTRACTION
-- ============================================================================

||| Extract the timestamp portion (first 10 chars = 48 bits)
public export
timestampPart : ULID -> String
timestampPart u = substr 0 10 u.encoded

||| Extract the randomness portion (last 16 chars = 80 bits)
public export
randomPart : ULID -> String
randomPart u = substr 10 16 u.encoded

||| Decode a Crockford's Base32 character to its value (0-31)
crockfordValue : Char -> Maybe Nat
crockfordValue c =
  let u = toUpper c
  in if u >= '0' && u <= '9' then Just (cast (ord u - ord '0'))
     else case u of
       'A' => Just 10; 'B' => Just 11; 'C' => Just 12; 'D' => Just 13
       'E' => Just 14; 'F' => Just 15; 'G' => Just 16; 'H' => Just 17
       'J' => Just 18; 'K' => Just 19; 'M' => Just 20; 'N' => Just 21
       'P' => Just 22; 'Q' => Just 23; 'R' => Just 24; 'S' => Just 25
       'T' => Just 26; 'V' => Just 27; 'W' => Just 28; 'X' => Just 29
       'Y' => Just 30; 'Z' => Just 31
       _   => Nothing

||| Extract timestamp as milliseconds since Unix epoch
public export covering
extractTimestamp : ULID -> Maybe Integer
extractTimestamp u =
  let chars = unpack (timestampPart u)
  in foldlM (\acc, c => do
       v <- crockfordValue c
       Just (acc * 32 + cast v)
     ) 0 chars

-- ============================================================================
-- MONOTONICITY
-- ============================================================================

||| Check if one ULID is chronologically after another
||| Due to lexicographic encoding, this is just string comparison.
public export
isAfter : ULID -> ULID -> Bool
isAfter a b = compare a b == GT

||| Check if a list of ULIDs is monotonically ordered
public export
isMonotonic : List ULID -> Bool
isMonotonic [] = True
isMonotonic [_] = True
isMonotonic (a :: b :: rest) = compare a b /= GT && isMonotonic (b :: rest)

-- ============================================================================
-- OVERFLOW CHECK
-- ============================================================================

||| Maximum ULID timestamp (2^48 - 1 ms ≈ year 10889)
public export
maxTimestamp : Integer
maxTimestamp = 281474976710655

||| Check if a timestamp is within ULID range
public export
isValidTimestamp : Integer -> Bool
isValidTimestamp ms = ms >= 0 && ms <= maxTimestamp

-- ============================================================================
-- MONOTONICITY PROOF SKETCH (for Agda)
-- ============================================================================

||| The core ULID monotonicity property:
||| Given ULIDs u1 and u2, if extractTimestamp u1 < extractTimestamp u2,
||| then compare u1 u2 = LT.
|||
||| This holds because:
||| 1. The timestamp occupies the high 48 bits (first 10 chars)
||| 2. Crockford's Base32 preserves ordering (0<1<...<9<A<B<...<Z)
||| 3. String comparison is lexicographic (high chars dominate)
|||
||| An Agda proof would show:
|||   monotonicity : (u1 u2 : ULID) ->
|||     extractTimestamp u1 < extractTimestamp u2 ->
|||     compare u1 u2 ≡ LT
public export
monotonicityDoc : String
monotonicityDoc = "Lexicographic order = chronological order by construction"
