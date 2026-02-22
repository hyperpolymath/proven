--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe hexadecimal encoding/decoding -- thin FFI wrapper over libproven.
--  Hex operations performed by the formally verified Idris2/Zig core.

with Interfaces; use Interfaces;

package Proven.Safe_Hex is

   type Byte_Array is array (Natural range <>) of Unsigned_8;

   Max_Decode_Bytes : constant := 128;
   Max_Encode_Chars : constant := 512;

   type Encode_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : String (1 .. Max_Encode_Chars);
                       Last       : Natural;
         when False => Error_Code : Integer;
      end case;
   end record;

   type Decode_Result (Success : Boolean := False) is record
      case Success is
         when True  => Bytes      : Byte_Array (0 .. Max_Decode_Bytes - 1);
                       Length     : Natural;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Encode bytes to lowercase hex string (calls hex_encode).
   function Encode (Bytes : Byte_Array) return Encode_Result;

   --  Decode hex string to bytes (calls hex_decode).
   function Decode (Hex : String) return Decode_Result;

   --  Validate hex string (calls hex_is_valid).
   function Is_Valid (S : String) return Boolean;

   --  Validate hex byte string -- even length and all hex chars
   --  (calls hex_is_valid_bytes).
   function Is_Valid_Bytes (S : String) return Boolean;

   --  Constant-time hex string comparison (calls hex_constant_time_eq).
   function Constant_Time_Equal (A, B : String) return Boolean;

   --  Parse hex string to unsigned integer (calls hex_to_int).
   type Int_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : Unsigned_64;
         when False => Error_Code : Integer;
      end case;
   end record;

   function Hex_To_Int (Hex : String) return Int_Result;

end Proven.Safe_Hex;
