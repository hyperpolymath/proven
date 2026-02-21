--  SPDX-License-Identifier: PMPL-1.0-or-later
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe hexadecimal encoding and decoding.

with Interfaces; use Interfaces;

package Proven.Safe_Hex is

   --  Byte array for decoding results.
   type Byte_Array is array (Natural range <>) of Unsigned_8;

   --  Maximum decode size (128 bytes).
   Max_Decode_Bytes : constant := 128;

   --  Fixed-size byte array for decode results.
   subtype Decode_Bytes is Byte_Array (0 .. Max_Decode_Bytes - 1);

   --  Decode result with fixed-size array.
   type Decode_Result (Valid : Boolean := False) is record
      case Valid is
         when True  =>
            Bytes  : Decode_Bytes;
            Length : Natural;
         when False => null;
      end case;
   end record;

   --  Maximum format output size.
   Max_Format_Len : constant := 384;

   --  Format result discriminated record.
   type Format_Result (Valid : Boolean := False) is record
      case Valid is
         when True  =>
            Value : String (1 .. Max_Format_Len);
            Last  : Natural;
         when False => null;
      end case;
   end record;

   --  Check if character is valid hex.
   function Is_Hex_Char (C : Character) return Boolean;

   --  Convert hex char to nibble value (0-15).
   --  Returns 255 if invalid.
   function Hex_Char_To_Nibble (C : Character) return Unsigned_8;

   --  Convert nibble to lowercase hex char.
   function Nibble_To_Hex_Char (N : Unsigned_8) return Character;

   --  Convert nibble to uppercase hex char.
   function Nibble_To_Hex_Char_Upper (N : Unsigned_8) return Character;

   --  Encode bytes to lowercase hex string.
   function Encode (Bytes : Byte_Array) return String;

   --  Encode bytes to uppercase hex string.
   function Encode_Upper (Bytes : Byte_Array) return String;

   --  Decode hex string to bytes.
   --  Limited to 128 bytes output (256 hex chars).
   function Decode (Hex : String) return Decode_Result;

   --  Decode hex string, raising exception on failure.
   function Decode_Or_Raise (Hex : String) return Byte_Array;

   --  Validate hex string (all characters are hex).
   function Is_Valid (S : String) return Boolean;

   --  Validate hex string has even length and all hex chars.
   function Is_Valid_Bytes (S : String) return Boolean;

   --  Format hex with spaces between bytes.
   function Format_Spaced (Hex : String) return Format_Result;

   --  Format hex with colons between bytes.
   function Format_Colons (Hex : String) return Format_Result;

   --  Constant-time comparison of hex strings.
   --  Returns True only if equal (case-insensitive).
   function Constant_Time_Equal (A, B : String) return Boolean;

   --  Convert unsigned integer to hex string with minimum width.
   function Int_To_Hex
     (Value : Unsigned_64; Min_Width : Positive := 1) return String;

   --  Parse hex string to unsigned integer.
   --  Returns (Valid => False) on overflow or invalid input.
   type Int_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Unsigned_64;
         when False => null;
      end case;
   end record;

   function Hex_To_Int (Hex : String) return Int_Result;

   --  Exception for hex operations.
   Hex_Error : exception;

end Proven.Safe_Hex;
