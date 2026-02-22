--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C; use Interfaces.C;

package body Proven.Safe_Crypto is

   type Byte_Array is array (Natural range <>) of aliased unsigned_char;

   function To_Bytes (S : String) return Byte_Array is
      Result : Byte_Array (0 .. S'Length - 1);
   begin
      for I in S'Range loop
         Result (I - S'First) := unsigned_char (Character'Pos (S (I)));
      end loop;
      return Result;
   end To_Bytes;

   function Constant_Time_Compare (A, B : String) return Safe_Bool_Result is
      Bytes_A : aliased Byte_Array := To_Bytes (A);
      Bytes_B : aliased Byte_Array := To_Bytes (B);
      Result  : FFI.Bool_Result;
   begin
      if A'Length = 0 and then B'Length = 0 then
         return (Success => True, Value => True);
      end if;
      if A'Length = 0 then
         --  Need at least one byte to pass; lengths differ means not equal.
         return (Success => True, Value => False);
      end if;
      if B'Length = 0 then
         return (Success => True, Value => False);
      end if;
      Result := FFI.Crypto_Constant_Time_Eq
        (Bytes_A (Bytes_A'First)'Access, Bytes_A'Length,
         Bytes_B (Bytes_B'First)'Access, Bytes_B'Length);
      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      return (Success => True, Value => Boolean (Result.Value));
   end Constant_Time_Compare;

   function Random_Bytes (Buffer : out String) return Boolean is
      Bytes  : aliased Byte_Array (0 .. Buffer'Length - 1);
      Status : int;
   begin
      if Buffer'Length = 0 then
         return True;
      end if;
      Status := FFI.Crypto_Random_Bytes
        (Bytes (Bytes'First)'Access, Bytes'Length);
      if Status /= FFI.PROVEN_OK then
         Buffer := (others => ASCII.NUL);
         return False;
      end if;
      for I in Buffer'Range loop
         Buffer (I) := Character'Val (Integer (Bytes (I - Buffer'First)));
      end loop;
      return True;
   end Random_Bytes;

end Proven.Safe_Crypto;
