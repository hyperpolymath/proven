--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe semantic versioning operations following SemVer 2.0.0.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Proven.Safe_Version is
   pragma Pure;

   --  Semantic version components.
   type Version is record
      Major      : Natural;
      Minor      : Natural;
      Patch      : Natural;
      Prerelease : Unbounded_String;
      Build      : Unbounded_String;
   end record;

   --  Parse result.
   type Version_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Version;
         when False => null;
      end case;
   end record;

   --  Create version from components.
   function Make
     (Major, Minor, Patch : Natural;
      Prerelease          : String := "";
      Build               : String := "") return Version;

   --  Parse version string.
   function Parse (S : String) return Version_Result;

   --  Parse version, raising exception on failure.
   function Parse_Or_Raise (S : String) return Version;

   --  Check if string is valid version.
   function Is_Valid (S : String) return Boolean;

   --  Format version as string.
   function Format (V : Version) return String;

   --  Compare versions.
   function Compare (A, B : Version) return Integer;
   --  Returns: -1 if A < B, 0 if A = B, 1 if A > B

   --  Comparison operators.
   function Equal (A, B : Version) return Boolean;
   function Less_Than (A, B : Version) return Boolean;
   function Greater_Than (A, B : Version) return Boolean;
   function Less_Or_Equal (A, B : Version) return Boolean;
   function Greater_Or_Equal (A, B : Version) return Boolean;

   --  Check if version is prerelease.
   function Is_Prerelease (V : Version) return Boolean;

   --  Check if version is stable (1.0.0 or higher, no prerelease).
   function Is_Stable (V : Version) return Boolean;

   --  Increment versions.
   function Increment_Major (V : Version) return Version;
   function Increment_Minor (V : Version) return Version;
   function Increment_Patch (V : Version) return Version;

   --  Get version without prerelease/build metadata.
   function Core_Version (V : Version) return Version;

   --  Check if version satisfies range (simplified, supports ^, ~, =, >, <).
   function Satisfies (V : Version; Range_Spec : String) return Boolean;

   --  Exception for version operations.
   Version_Error : exception;

end Proven.Safe_Version;
