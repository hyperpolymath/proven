--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe semantic versioning -- thin FFI wrapper over libproven.
--  SemVer parsing and comparison done by the Idris2/Zig core.

package Proven.Safe_Version is

   Max_Prerelease_Len : constant := 256;

   type Version is record
      Major      : Natural;
      Minor      : Natural;
      Patch      : Natural;
      Prerelease : String (1 .. Max_Prerelease_Len);
      Pre_Last   : Natural;
   end record;

   type Version_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : Version;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Parse semantic version string (calls proven_version_parse).
   function Parse (S : String) return Version_Result;

   --  Compare two semantic versions (calls proven_version_compare).
   --  Returns -1 if A < B, 0 if A = B, 1 if A > B.
   function Compare (A, B : Version) return Integer;

end Proven.Safe_Version;
