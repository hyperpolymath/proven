--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C; use Interfaces.C;

package body Proven.Safe_Network is

   type Byte_Array is array (Natural range <>) of aliased unsigned_char;

   function To_Bytes (S : String) return Byte_Array is
      Result : Byte_Array (0 .. S'Length - 1);
   begin
      for I in S'Range loop
         Result (I - S'First) := unsigned_char (Character'Pos (S (I)));
      end loop;
      return Result;
   end To_Bytes;

   function To_C_IPv4 (Addr : IPv4) return FFI.IPv4_Address is
      Result : FFI.IPv4_Address;
   begin
      Result.Octets (0) := unsigned_char (Addr.A);
      Result.Octets (1) := unsigned_char (Addr.B);
      Result.Octets (2) := unsigned_char (Addr.C);
      Result.Octets (3) := unsigned_char (Addr.D);
      return Result;
   end To_C_IPv4;

   function Parse_IPv4 (Address : String) return IPv4_Parse_Result is
      Bytes  : aliased Byte_Array := To_Bytes (Address);
      Result : FFI.IPv4_Result;
   begin
      if Address'Length = 0 then
         return (Success => False, Error_Code => Integer (FFI.PROVEN_ERR_INVALID_ARGUMENT));
      end if;
      Result := FFI.Network_Parse_IPv4
        (Bytes (Bytes'First)'Access, Bytes'Length);
      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      return (Success => True,
              Address => (A => Natural (Result.Address.Octets (0)),
                          B => Natural (Result.Address.Octets (1)),
                          C => Natural (Result.Address.Octets (2)),
                          D => Natural (Result.Address.Octets (3))));
   end Parse_IPv4;

   function Is_Private (Addr : IPv4) return Boolean is
      C_Addr : constant FFI.IPv4_Address := To_C_IPv4 (Addr);
   begin
      return Boolean (FFI.Network_IPv4_Is_Private (C_Addr));
   end Is_Private;

   function Is_Loopback (Addr : IPv4) return Boolean is
      C_Addr : constant FFI.IPv4_Address := To_C_IPv4 (Addr);
   begin
      return Boolean (FFI.Network_IPv4_Is_Loopback (C_Addr));
   end Is_Loopback;

end Proven.Safe_Network;
