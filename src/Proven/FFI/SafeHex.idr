-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeHex operations (60/75)
module Proven.FFI.SafeHex

import Proven.SafeHex
import Proven.Core

%default total

encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

%export
proven_idris_hex_is_valid_char : Int -> Int
proven_idris_hex_is_valid_char charCode =
  let c = charCode
  in encodeBool ((c >= 48 && c <= 57)    -- '0'-'9'
              || (c >= 97 && c <= 102)   -- 'a'-'f'
              || (c >= 65 && c <= 70))   -- 'A'-'F'

%export
proven_idris_hex_char_to_nibble : Int -> Int
proven_idris_hex_char_to_nibble charCode =
  if charCode >= 48 && charCode <= 57 then charCode - 48        -- '0'-'9'
  else if charCode >= 97 && charCode <= 102 then charCode - 87  -- 'a'-'f' (97-10)
  else if charCode >= 65 && charCode <= 70 then charCode - 55   -- 'A'-'F' (65-10)
  else (-1)  -- Invalid

%export
proven_idris_hex_nibble_to_char : Int -> Int
proven_idris_hex_nibble_to_char n =
  if n >= 0 && n <= 9 then n + 48      -- '0'-'9'
  else if n >= 10 && n <= 15 then n + 87  -- 'a'-'f'
  else 48  -- Default to '0'

%export
proven_idris_hex_even_length : Int -> Int
proven_idris_hex_even_length len = encodeBool (len `mod` 2 == 0)
