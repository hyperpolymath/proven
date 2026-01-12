-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe network operations for IP address validation and classification.

package Proven.Safe_Network is

   type IPv4 is record
      A, B, C, D : Natural range 0 .. 255;
   end record;

   type Optional_IPv4 (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : IPv4;
         when False => null;
      end case;
   end record;

   --  Parse an IPv4 address string.
   function Parse_IPv4 (Address : String) return Optional_IPv4;

   --  Check if a string is a valid IPv4 address.
   function Is_Valid_IPv4 (Address : String) return Boolean;

   --  Check if an IPv4 address is in a private range.
   function Is_Private (Address : String) return Boolean;

   --  Check if an IPv4 address is a loopback address (127.0.0.0/8).
   function Is_Loopback (Address : String) return Boolean;

   --  Check if an IPv4 address is public (not private or loopback).
   function Is_Public (Address : String) return Boolean;

   --  Format an IPv4 address as a string.
   function Format_IPv4 (IP : IPv4) return String;

end Proven.Safe_Network;
