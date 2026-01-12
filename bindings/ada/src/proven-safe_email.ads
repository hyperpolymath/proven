--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe email validation and parsing operations.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Proven.Safe_Email is

   type Email_Parts is record
      Local_Part : Unbounded_String;
      Domain     : Unbounded_String;
   end record;

   type Optional_Email_Parts (Valid : Boolean := False) is record
      case Valid is
         when True  => Parts : Email_Parts;
         when False => null;
      end case;
   end record;

   type Optional_String (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Unbounded_String;
         when False => null;
      end case;
   end record;

   --  Check if an email address is valid (basic check).
   function Is_Valid (Email : String) return Boolean;

   --  Split an email into local part and domain.
   function Split_Email (Email : String) return Optional_Email_Parts;

   --  Extract the domain from an email address.
   function Get_Domain (Email : String) return Optional_String;

   --  Extract the local part from an email address.
   function Get_Local_Part (Email : String) return Optional_String;

   --  Normalize an email address (lowercase domain).
   function Normalize (Email : String) return Optional_String;

end Proven.Safe_Email;
