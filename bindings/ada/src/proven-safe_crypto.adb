--  SPDX-License-Identifier: Apache-2.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Interfaces; use Interfaces;

package body Proven.Safe_Crypto is

   function Constant_Time_Compare (A, B : String) return Boolean is
      Result : Unsigned_8 := 0;
   begin
      if A'Length /= B'Length then
         return False;
      end if;

      if A'Length = 0 then
         return True;
      end if;

      for I in 0 .. A'Length - 1 loop
         Result := Result or
                   (Unsigned_8 (Character'Pos (A (A'First + I))) xor
                    Unsigned_8 (Character'Pos (B (B'First + I))));
      end loop;

      return Result = 0;
   end Constant_Time_Compare;

   function Secure_Zero (Length : Natural) return String is
   begin
      return (1 .. Length => ASCII.NUL);
   end Secure_Zero;

end Proven.Safe_Crypto;
