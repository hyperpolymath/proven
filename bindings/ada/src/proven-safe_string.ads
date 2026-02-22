--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe string operations -- thin FFI wrapper over libproven.
--  All escaping/validation performed by the formally verified Idris2/Zig core.

package Proven.Safe_String is

   type Safe_String_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : String (1 .. 65536);
                       Last       : Natural;
         when False => Error_Code : Integer;
      end case;
   end record;

   type Safe_Bool_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : Boolean;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Check if bytes are valid UTF-8 (calls proven_string_is_valid_utf8).
   function Is_Valid_UTF8 (Value : String) return Safe_Bool_Result;

   --  Escape HTML special characters to prevent XSS
   --  (calls proven_string_escape_html).
   function Escape_Html (Value : String) return Safe_String_Result;

   --  Escape single quotes for SQL strings
   --  (calls proven_string_escape_sql).
   function Escape_Sql (Value : String) return Safe_String_Result;

   --  Escape JavaScript special characters
   --  (calls proven_string_escape_js).
   function Escape_Js (Value : String) return Safe_String_Result;

end Proven.Safe_String;
