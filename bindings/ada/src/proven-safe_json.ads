--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe JSON validation -- thin FFI wrapper over libproven.
--  JSON validation and type detection done by the Idris2/Zig core.

package Proven.Safe_Json is

   type JSON_Kind is (
      JSON_Null,
      JSON_Boolean,
      JSON_Number,
      JSON_String,
      JSON_Array,
      JSON_Object,
      JSON_Invalid
   );

   type Safe_Bool_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : Boolean;
         when False => Error_Code : Integer;
      end case;
   end record;

   type Safe_Kind_Result (Success : Boolean := False) is record
      case Success is
         when True  => Kind       : JSON_Kind;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Check if JSON string is valid (calls proven_json_is_valid).
   function Is_Valid (JSON : String) return Safe_Bool_Result;

   --  Get JSON value type at root level (calls proven_json_get_type).
   function Get_Type (JSON : String) return Safe_Kind_Result;

end Proven.Safe_Json;
