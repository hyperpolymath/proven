--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe filesystem path operations -- thin FFI wrapper over libproven.
--  Traversal detection and filename sanitization done by the Idris2/Zig core.

package Proven.Safe_Path is

   type Safe_Bool_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : Boolean;
         when False => Error_Code : Integer;
      end case;
   end record;

   Max_Path_Len : constant := 4096;

   type Safe_String_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : String (1 .. Max_Path_Len);
                       Last       : Natural;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Check if a path contains directory traversal sequences
   --  (calls proven_path_has_traversal).
   function Has_Traversal (Path : String) return Safe_Bool_Result;

   --  Sanitize a filename by removing dangerous characters
   --  (calls proven_path_sanitize_filename).
   function Sanitize_Filename (Filename : String) return Safe_String_Result;

end Proven.Safe_Path;
