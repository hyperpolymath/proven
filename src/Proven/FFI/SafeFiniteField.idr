-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeFiniteField operations
|||
||| This module exports finite field arithmetic helpers to the C ABI
||| via Idris2's RefC backend. All functions are proven total.
|||
||| Return conventions:
||| - Value → Int (field element value)
||| - Validation → Int (0 = invalid/false, 1 = valid/true)
||| - Symbol → Int (-1, 0, 1 for Legendre symbol)
|||
||| CRITICAL: Finite field operations stay within field bounds.
|||           Used in cryptography (elliptic curves, AES, GCM).
|||
||| Prime Fields GF(p):
||| - Elements in range [0, p-1]
||| - Addition, subtraction, multiplication modulo p
||| - Multiplicative inverse via Extended Euclidean Algorithm
||| - Division: a/b = a × b^(-1)
||| - Exponentiation by squaring
|||
||| Binary Fields GF(2^n):
||| - Polynomial representation with binary coefficients
||| - Addition/subtraction = XOR (characteristic 2)
||| - Multiplication = polynomial multiplication mod irreducible
||| - Used in AES (GF(2^8)), GCM (GF(2^128))
|||
||| Quadratic Residues:
||| - Legendre symbol (a/p): 1 if QR, -1 if not, 0 if a=0
||| - Square root via Tonelli-Shanks (when p ≡ 3 mod 4)
|||
||| Common Fields:
||| - GF(2): Binary
||| - Curve25519: p = 2^255 - 19
||| - secp256k1: p = 2^256 - 2^32 - 977
|||
||| NOTE: Actual field arithmetic managed in Zig.
|||       These helpers validate operations and provide constants.
module Proven.FFI.SafeFiniteField

import Proven.SafeFiniteField
import Proven.Core
import Data.String

%default total

--------------------------------------------------------------------------------
-- FFI Result Encoding
--------------------------------------------------------------------------------

||| Encode Bool as Int
encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

--------------------------------------------------------------------------------
-- Field Element Validation
--------------------------------------------------------------------------------

export
proven_idris_ff_is_in_range : Int -> Int -> Int
proven_idris_ff_is_in_range value modulus =
  encodeBool (value >= 0 && value < modulus)

export
proven_idris_ff_is_zero : Int -> Int
proven_idris_ff_is_zero value =
  encodeBool (value == 0)

export
proven_idris_ff_is_one : Int -> Int
proven_idris_ff_is_one value =
  encodeBool (value == 1)

export
proven_idris_ff_is_valid_modulus : Int -> Int
proven_idris_ff_is_valid_modulus modulus =
  encodeBool (modulus > 1)

--------------------------------------------------------------------------------
-- Modular Arithmetic Helpers
--------------------------------------------------------------------------------

export
proven_idris_ff_mod_add : Int -> Int -> Int -> Int
proven_idris_ff_mod_add a b modulus =
  (a + b) `mod` modulus

export
proven_idris_ff_mod_sub : Int -> Int -> Int -> Int
proven_idris_ff_mod_sub a b modulus =
  (a + modulus - b) `mod` modulus

export
proven_idris_ff_mod_mul : Int -> Int -> Int -> Int
proven_idris_ff_mod_mul a b modulus =
  (a * b) `mod` modulus

export
proven_idris_ff_mod_neg : Int -> Int -> Int
proven_idris_ff_mod_neg a modulus =
  (modulus - a) `mod` modulus

export
proven_idris_ff_mod_square : Int -> Int -> Int
proven_idris_ff_mod_square a modulus =
  proven_idris_ff_mod_mul a a modulus

--------------------------------------------------------------------------------
-- Extended GCD Helper
--------------------------------------------------------------------------------

export
proven_idris_ff_gcd : Int -> Int -> Int
proven_idris_ff_gcd a b =
  if b == 0 then a
  else proven_idris_ff_gcd b (a `mod` b)

export
proven_idris_ff_coprime : Int -> Int -> Int
proven_idris_ff_coprime a b =
  encodeBool (proven_idris_ff_gcd a b == 1)

--------------------------------------------------------------------------------
-- Modular Inverse
--------------------------------------------------------------------------------

export
proven_idris_ff_has_inverse : Int -> Int -> Int
proven_idris_ff_has_inverse a modulus =
  encodeBool (a /= 0 && proven_idris_ff_coprime a modulus == 1)

export
proven_idris_ff_inverse_exists : Int -> Int -> Int
proven_idris_ff_inverse_exists = proven_idris_ff_has_inverse

--------------------------------------------------------------------------------
-- Exponentiation
--------------------------------------------------------------------------------

export
proven_idris_ff_pow_base_case : Int -> Int
proven_idris_ff_pow_base_case _ = 1  -- a^0 = 1

export
proven_idris_ff_pow_is_even : Int -> Int
proven_idris_ff_pow_is_even exp =
  encodeBool (exp `mod` 2 == 0)

export
proven_idris_ff_pow_half : Int -> Int
proven_idris_ff_pow_half exp = exp `div` 2

--------------------------------------------------------------------------------
-- Binary Field Operations (GF(2^n))
--------------------------------------------------------------------------------

export
proven_idris_bf_xor : Int -> Int -> Int
proven_idris_bf_xor a b = a `xor` b

export
proven_idris_bf_and : Int -> Int -> Int
proven_idris_bf_and a b = a .&. b

export
proven_idris_bf_shift_left : Int -> Int -> Int
proven_idris_bf_shift_left a shift =
  a `shiftL` shift

export
proven_idris_bf_shift_right : Int -> Int -> Int
proven_idris_bf_shift_right a shift =
  a `shiftR` shift

export
proven_idris_bf_mask : Int -> Int
proven_idris_bf_mask n =
  (1 `shiftL` n) - 1

export
proven_idris_bf_degree : Int -> Int
proven_idris_bf_degree poly =
  if poly == 0 then 0
  else go poly 0
  where
    go : Int -> Int -> Int
    go 0 acc = acc
    go p acc = go (p `shiftR` 1) (acc + 1)

--------------------------------------------------------------------------------
-- Legendre Symbol
--------------------------------------------------------------------------------

export
proven_idris_ff_legendre_symbol : Int -> Int
proven_idris_ff_legendre_symbol symbol = symbol  -- -1, 0, or 1

export
proven_idris_ff_is_quadratic_residue : Int -> Int
proven_idris_ff_is_quadratic_residue symbol =
  encodeBool (symbol == 1)

export
proven_idris_ff_is_non_residue : Int -> Int
proven_idris_ff_is_non_residue symbol =
  encodeBool (symbol == (-1))

export
proven_idris_ff_has_sqrt : Int -> Int
proven_idris_ff_has_sqrt symbol =
  encodeBool (symbol >= 0)  -- 0 or 1

--------------------------------------------------------------------------------
-- Square Root Conditions
--------------------------------------------------------------------------------

export
proven_idris_ff_sqrt_easy_case : Int -> Int
proven_idris_ff_sqrt_easy_case modulus =
  -- p ≡ 3 (mod 4) makes sqrt easy: a^((p+1)/4)
  encodeBool (modulus `mod` 4 == 3)

export
proven_idris_ff_sqrt_exponent : Int -> Int
proven_idris_ff_sqrt_exponent modulus =
  -- For p ≡ 3 (mod 4), sqrt exponent is (p+1)/4
  (modulus + 1) `div` 4

--------------------------------------------------------------------------------
-- Polynomial Operations
--------------------------------------------------------------------------------

export
proven_idris_poly_degree : Int -> Int
proven_idris_poly_degree coeffCount =
  if coeffCount == 0 then 0 else coeffCount - 1

export
proven_idris_poly_is_zero : Int -> Int
proven_idris_poly_is_zero coeffCount =
  encodeBool (coeffCount == 0)

export
proven_idris_poly_is_constant : Int -> Int
proven_idris_poly_is_constant coeffCount =
  encodeBool (coeffCount <= 1)

export
proven_idris_poly_is_monic : Int -> Int
proven_idris_poly_is_monic leadingCoeff =
  encodeBool (leadingCoeff == 1)

--------------------------------------------------------------------------------
-- Prime Checking
--------------------------------------------------------------------------------

export
proven_idris_ff_is_prime : Int -> Int
proven_idris_ff_is_prime n =
  if n < 2 then 0
  else if n == 2 then 1
  else if n `mod` 2 == 0 then 0
  else checkDivisors 3 n
  where
    checkDivisors : Int -> Int -> Int
    checkDivisors i n =
      if i * i > n then 1
      else if n `mod` i == 0 then 0
      else checkDivisors (i + 2) n

export
proven_idris_ff_next_prime_candidate : Int -> Int
proven_idris_ff_next_prime_candidate n =
  if n `mod` 2 == 0 then n + 1 else n + 2

--------------------------------------------------------------------------------
-- Common Field Constants
--------------------------------------------------------------------------------

export
proven_idris_ff_gf2_modulus : Int
proven_idris_ff_gf2_modulus = 2

export
proven_idris_ff_gf3_modulus : Int
proven_idris_ff_gf3_modulus = 3

export
proven_idris_ff_aes_irreducible : Int
proven_idris_ff_aes_irreducible = 0x11B  -- x^8 + x^4 + x^3 + x + 1

export
proven_idris_ff_gcm_irreducible : Int
proven_idris_ff_gcm_irreducible = 0x87  -- Lower bits of GCM polynomial

export
proven_idris_ff_gf256_modulus : Int
proven_idris_ff_gf256_modulus = 256

--------------------------------------------------------------------------------
-- Field Properties
--------------------------------------------------------------------------------

export
proven_idris_ff_field_size : Int -> Int
proven_idris_ff_field_size modulus = modulus

export
proven_idris_ff_num_nonzero_elements : Int -> Int
proven_idris_ff_num_nonzero_elements modulus =
  if modulus > 0 then modulus - 1 else 0

export
proven_idris_ff_characteristic : Int -> Int
proven_idris_ff_characteristic modulus = modulus

export
proven_idris_ff_is_binary_field : Int -> Int
proven_idris_ff_is_binary_field modulus =
  encodeBool (modulus == 2)

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

export
proven_idris_ff_is_valid_element : Int -> Int -> Int
proven_idris_ff_is_valid_element value modulus =
  proven_idris_ff_is_in_range value modulus

export
proven_idris_ff_is_valid_exponent : Int -> Int
proven_idris_ff_is_valid_exponent exp =
  encodeBool (exp >= 0)

export
proven_idris_ff_is_invertible : Int -> Int
proven_idris_ff_is_invertible value =
  encodeBool (value /= 0)

--------------------------------------------------------------------------------
-- Error Messages
--------------------------------------------------------------------------------

export
proven_idris_ff_friendly_error : String -> String
proven_idris_ff_friendly_error errorMsg =
  if isInfixOf "modulus" (toLower errorMsg) || isInfixOf "prime" (toLower errorMsg)
    then "Invalid field modulus (must be prime > 1)"
  else if isInfixOf "inverse" (toLower errorMsg) || isInfixOf "division" (toLower errorMsg)
    then "No multiplicative inverse (element is zero or not coprime)"
  else if isInfixOf "range" (toLower errorMsg) || isInfixOf "bounds" (toLower errorMsg)
    then "Field element out of range (must be in [0, p-1])"
  else if isInfixOf "residue" (toLower errorMsg) || isInfixOf "sqrt" (toLower errorMsg)
    then "Not a quadratic residue (no square root exists)"
  else if isInfixOf "polynomial" (toLower errorMsg)
    then "Invalid polynomial operation"
  else
    "Finite field error"

export
proven_idris_ff_field_description : Int -> String
proven_idris_ff_field_description modulus =
  if modulus == 2 then "GF(2) - Binary field"
  else if modulus == 3 then "GF(3) - Ternary field"
  else if modulus == 256 then "GF(256) - AES field"
  else "GF(" ++ show modulus ++ ") - Prime field"
