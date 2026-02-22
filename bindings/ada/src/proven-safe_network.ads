--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe network operations -- thin FFI wrapper over libproven.
--  IPv4 parsing and classification done by the Idris2/Zig core.

package Proven.Safe_Network is

   type IPv4 is record
      A, B, C, D : Natural range 0 .. 255;
   end record;

   type IPv4_Parse_Result (Success : Boolean := False) is record
      case Success is
         when True  => Address    : IPv4;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Parse an IPv4 address string (calls proven_network_parse_ipv4).
   function Parse_IPv4 (Address : String) return IPv4_Parse_Result;

   --  Check if an IPv4 address is in a private range
   --  (calls proven_network_ipv4_is_private).
   function Is_Private (Addr : IPv4) return Boolean;

   --  Check if an IPv4 address is a loopback address
   --  (calls proven_network_ipv4_is_loopback).
   function Is_Loopback (Addr : IPv4) return Boolean;

end Proven.Safe_Network;
