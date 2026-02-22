--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Proven.Safe_Version is

   type Byte_Array is array (Natural range <>) of aliased unsigned_char;

   function To_Bytes (S : String) return Byte_Array is
      Result : Byte_Array (0 .. S'Length - 1);
   begin
      for I in S'Range loop
         Result (I - S'First) := unsigned_char (Character'Pos (S (I)));
      end loop;
      return Result;
   end To_Bytes;

   function Parse (S : String) return Version_Result is
      Bytes  : aliased Byte_Array := To_Bytes (S);
      Result : FFI.Version_Result;
   begin
      if S'Length = 0 then
         return (Success => False,
                 Error_Code => Integer (FFI.PROVEN_ERR_INVALID_ARGUMENT));
      end if;
      Result := FFI.Version_Parse
        (Bytes (Bytes'First)'Access, Bytes'Length);
      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      declare
         V : Version;
      begin
         V.Major := Natural (Result.Version.Major);
         V.Minor := Natural (Result.Version.Minor);
         V.Patch := Natural (Result.Version.Patch);
         V.Prerelease := (others => ' ');
         V.Pre_Last := 0;
         if Result.Version.Prerelease /= Null_Ptr and then
            Result.Version.Prerelease_Len > 0
         then
            declare
               Pre_Str : constant String :=
                  Value (Result.Version.Prerelease,
                         Result.Version.Prerelease_Len);
               Pre_Len : constant Natural :=
                  Natural'Min (Pre_Str'Length, Max_Prerelease_Len);
            begin
               V.Prerelease (1 .. Pre_Len) := Pre_Str (Pre_Str'First .. Pre_Str'First + Pre_Len - 1);
               V.Pre_Last := Pre_Len;
            end;
         end if;
         --  Free C-allocated prerelease string
         declare
            Free_Ver : aliased FFI.C_Semantic_Version := Result.Version;
         begin
            FFI.Proven_Version_Free (Free_Ver'Access);
         end;
         return (Success => True, Value => V);
      end;
   end Parse;

   function Compare (A, B : Version) return Integer is
      C_A, C_B : FFI.C_Semantic_Version;
      C_Pre_A  : aliased char_array := To_C (A.Prerelease (1 .. A.Pre_Last));
      C_Pre_B  : aliased char_array := To_C (B.Prerelease (1 .. B.Pre_Last));
   begin
      C_A.Major := unsigned (A.Major);
      C_A.Minor := unsigned (A.Minor);
      C_A.Patch := unsigned (A.Patch);
      C_A.Prerelease_Len := size_t (A.Pre_Last);
      if A.Pre_Last > 0 then
         C_A.Prerelease := To_Chars_Ptr (C_Pre_A'Unchecked_Access);
      else
         C_A.Prerelease := Null_Ptr;
      end if;

      C_B.Major := unsigned (B.Major);
      C_B.Minor := unsigned (B.Minor);
      C_B.Patch := unsigned (B.Patch);
      C_B.Prerelease_Len := size_t (B.Pre_Last);
      if B.Pre_Last > 0 then
         C_B.Prerelease := To_Chars_Ptr (C_Pre_B'Unchecked_Access);
      else
         C_B.Prerelease := Null_Ptr;
      end if;

      return Integer (FFI.Version_Compare (C_A, C_B));
   end Compare;

end Proven.Safe_Version;
