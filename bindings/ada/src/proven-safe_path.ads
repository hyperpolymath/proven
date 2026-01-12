--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe filesystem path operations with traversal attack prevention.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Proven.Safe_Path is

   type Optional_String (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Unbounded_String;
         when False => null;
      end case;
   end record;

   --  Check if a path contains directory traversal sequences.
   function Has_Traversal (Path : String) return Boolean;

   --  Check if a path is safe (no traversal attacks).
   function Is_Safe (Path : String) return Boolean;

   --  Sanitize a filename by removing dangerous characters.
   function Sanitize_Filename (Filename : String) return String;

   --  Safely join path components, rejecting traversal attempts.
   function Safe_Join (Base : String; Part : String) return Optional_String;

end Proven.Safe_Path;
