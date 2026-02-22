--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe UUID operations -- thin FFI wrapper over libproven.
--  UUID generation, parsing, and formatting done by the Idris2/Zig core.

package Proven.Safe_UUID is

   type UUID_Bytes is array (0 .. 15) of Natural range 0 .. 255;

   type UUID is record
      Bytes : UUID_Bytes;
   end record;

   type UUID_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : UUID;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  UUID canonical string length (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx).
   UUID_String_Length : constant := 36;

   type Format_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : String (1 .. 45);
                       Last       : Natural;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Generate a v4 (random) UUID (calls uuid_v4_generate).
   function V4_Generate return UUID_Result;

   --  Parse UUID from canonical string (calls uuid_parse).
   function Parse (S : String) return UUID_Result;

   --  Format UUID as canonical string (calls uuid_to_string).
   function Format (U : UUID) return Format_Result;

   --  Format UUID as URN (calls uuid_to_urn).
   function To_URN (U : UUID) return Format_Result;

   --  Check if UUID is nil (calls uuid_is_nil).
   function Is_Nil (U : UUID) return Boolean;

   --  Check if two UUIDs are equal (calls uuid_equals).
   function Equal (A, B : UUID) return Boolean;

   --  Check if string is valid UUID format (calls uuid_is_valid).
   function Is_Valid (S : String) return Boolean;

end Proven.Safe_UUID;
