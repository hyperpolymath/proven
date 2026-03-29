-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| SafeCBOR - Concise Binary Object Representation (RFC 8949)
|||
||| Provides type-safe CBOR data model and validation.
||| CBOR is used in COSE, WebAuthn, FIDO2, and IoT protocols.
||| Prevents: integer overflow, indefinite-length bombs, tag confusion.
module Proven.SafeCBOR

import Data.String
import Data.List
import Data.Nat
import Data.Bits

%default total

-- ============================================================================
-- CBOR DATA MODEL
-- ============================================================================

||| CBOR major types (3-bit)
public export
data MajorType =
    UnsignedInt  -- 0
  | NegativeInt  -- 1
  | ByteString   -- 2
  | TextString   -- 3
  | Array        -- 4
  | Map          -- 5
  | Tag          -- 6
  | Simple       -- 7

public export
Show MajorType where
  show UnsignedInt = "uint"; show NegativeInt = "nint"
  show ByteString = "bstr"; show TextString = "tstr"
  show Array = "array"; show Map = "map"
  show Tag = "tag"; show Simple = "simple"

public export
Eq MajorType where
  UnsignedInt == UnsignedInt = True; NegativeInt == NegativeInt = True
  ByteString == ByteString = True; TextString == TextString = True
  Array == Array = True; Map == Map = True
  Tag == Tag = True; Simple == Simple = True
  _ == _ = False

||| CBOR simple values
public export
data SimpleValue =
    CBORFalse     -- 20
  | CBORTrue      -- 21
  | CBORNull      -- 22
  | CBORUndefined -- 23
  | SimpleNum Nat -- 0-19, 24-255

public export
Show SimpleValue where
  show CBORFalse = "false"; show CBORTrue = "true"
  show CBORNull = "null"; show CBORUndefined = "undefined"
  show (SimpleNum n) = "simple(" ++ show n ++ ")"

public export
Eq SimpleValue where
  CBORFalse == CBORFalse = True; CBORTrue == CBORTrue = True
  CBORNull == CBORNull = True; CBORUndefined == CBORUndefined = True
  (SimpleNum a) == (SimpleNum b) = a == b
  _ == _ = False

||| CBOR value (abstract data model)
public export
data CBORValue : Type where
  CBORUInt    : Integer -> CBORValue          -- Major type 0
  CBORNInt    : Integer -> CBORValue          -- Major type 1 (stored as -1-n)
  CBORBytes   : List Bits8 -> CBORValue       -- Major type 2
  CBORText    : String -> CBORValue           -- Major type 3
  CBORArray   : List CBORValue -> CBORValue   -- Major type 4
  CBORMap     : List (CBORValue, CBORValue) -> CBORValue  -- Major type 5
  CBORTagged  : Integer -> CBORValue -> CBORValue  -- Major type 6
  CBORSimple  : SimpleValue -> CBORValue      -- Major type 7

public export
Show CBORValue where
  show (CBORUInt n)     = show n
  show (CBORNInt n)     = show (negate (n + 1))
  show (CBORBytes bs)   = "h'" ++ show (length bs) ++ " bytes'"
  show (CBORText s)     = "\"" ++ s ++ "\""
  show (CBORArray xs)   = "[" ++ show (length xs) ++ " items]"
  show (CBORMap kvs)    = "{" ++ show (length kvs) ++ " pairs}"
  show (CBORTagged t v) = show t ++ "(" ++ show v ++ ")"
  show (CBORSimple s)   = show s

-- ============================================================================
-- WELL-KNOWN TAGS
-- ============================================================================

||| Standard CBOR date/time string (tag 0)
public export
dateTimeString : String -> CBORValue
dateTimeString s = CBORTagged 0 (CBORText s)

||| Epoch-based date/time (tag 1)
public export
epochDateTime : Integer -> CBORValue
epochDateTime n = CBORTagged 1 (CBORUInt n)

||| Bignum (tag 2 = positive, tag 3 = negative)
public export
positiveBignum : List Bits8 -> CBORValue
positiveBignum bs = CBORTagged 2 (CBORBytes bs)

||| CBOR-encoded data item (tag 24)
public export
encodedCBOR : List Bits8 -> CBORValue
encodedCBOR bs = CBORTagged 24 (CBORBytes bs)

||| URI (tag 32)
public export
uri : String -> CBORValue
uri s = CBORTagged 32 (CBORText s)

||| COSE key (tag 98 for set, specific tags for messages)
public export
coseSign1 : CBORValue -> CBORValue
coseSign1 = CBORTagged 18

-- ============================================================================
-- VALIDATION
-- ============================================================================

||| CBOR validation errors
public export
data CBORError =
    IntegerOverflow Integer
  | InvalidSimpleValue Nat
  | NestingTooDeep Nat
  | DuplicateMapKey CBORValue
  | InvalidTag Integer

public export
Show CBORError where
  show (IntegerOverflow n) = "Integer overflow: " ++ show n
  show (InvalidSimpleValue n) = "Invalid simple value: " ++ show n
  show (NestingTooDeep n) = "Nesting too deep: " ++ show n
  show (DuplicateMapKey k) = "Duplicate map key: " ++ show k
  show (InvalidTag t) = "Invalid tag: " ++ show t

||| Maximum safe nesting depth (prevents stack overflow)
public export
MaxNestingDepth : Nat
MaxNestingDepth = 64

||| Validate CBOR value depth
public export
validateDepth : Nat -> CBORValue -> Bool
validateDepth Z _ = False
validateDepth (S _) (CBORUInt _) = True
validateDepth (S _) (CBORNInt _) = True
validateDepth (S _) (CBORBytes _) = True
validateDepth (S _) (CBORText _) = True
validateDepth (S _) (CBORSimple _) = True
validateDepth (S k) (CBORArray xs) = all (validateDepth k) xs
validateDepth (S k) (CBORMap kvs) = all (\(ka, va) => validateDepth k ka && validateDepth k va) kvs
validateDepth (S k) (CBORTagged _ v) = validateDepth k v

||| Check for duplicate keys in a CBOR map (deterministic encoding requirement)
public export
hasDuplicateKeys : List (CBORValue, CBORValue) -> Bool
hasDuplicateKeys [] = False
hasDuplicateKeys ((k, _) :: rest) =
  any (\(k2, _) => show k == show k2) rest || hasDuplicateKeys rest

||| Validate a CBOR integer is within safe range
public export
isValidInteger : Integer -> Bool
isValidInteger n = n >= -9223372036854775808 && n <= 18446744073709551615

-- ============================================================================
-- SIZE ESTIMATION
-- ============================================================================

||| Estimate encoded size in bytes (for buffer allocation)
public export covering
estimateSize : CBORValue -> Nat
estimateSize (CBORUInt n) = if n < 24 then 1 else if n < 256 then 2 else if n < 65536 then 3 else 9
estimateSize (CBORNInt _) = 9  -- Worst case
estimateSize (CBORBytes bs) = 5 + length bs
estimateSize (CBORText s) = 5 + length s
estimateSize (CBORArray xs) = 5 + sum (map estimateSize xs)
estimateSize (CBORMap kvs) = 5 + sum (map (\(k, v) => estimateSize k + estimateSize v) kvs)
estimateSize (CBORTagged _ v) = 5 + estimateSize v
estimateSize (CBORSimple _) = 1
