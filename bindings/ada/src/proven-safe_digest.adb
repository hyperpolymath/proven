--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Proven.Safe_Digest is

   function To_C_Algorithm (Algo : Hash_Algorithm) return unsigned_char is
   begin
      case Algo is
         when SHA256 => return 0;
         when SHA384 => return 1;
         when SHA512 => return 2;
         when Blake3 => return 3;
      end case;
   end To_C_Algorithm;

   function From_C_Algorithm (C_Algo : unsigned_char) return Hash_Algorithm is
   begin
      case C_Algo is
         when 0      => return SHA256;
         when 1      => return SHA384;
         when 2      => return SHA512;
         when 3      => return Blake3;
         when others => return SHA256;
      end case;
   end From_C_Algorithm;

   function Parse (Digest_String : String) return Parse_Result is
      C_Input : chars_ptr := New_String (Digest_String);
      Result  : FFI.Digest_Result;
   begin
      Result := FFI.Digest_Parse (C_Input, Digest_String'Length);
      Free (C_Input);

      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;

      declare
         D : Digest;
      begin
         D.Algorithm := From_C_Algorithm (Result.Digest.Algorithm);
         D.Value := (others => ' ');
         D.Value_Last := 0;
         if Result.Digest.Value /= Null_Ptr and then
            Result.Digest.Value_Len > 0
         then
            declare
               C_Str : constant String :=
                  Value (Result.Digest.Value, Result.Digest.Value_Len);
               Len   : constant Natural :=
                  Natural'Min (C_Str'Length, Max_Digest_Len);
            begin
               D.Value (1 .. Len) := C_Str (C_Str'First .. C_Str'First + Len - 1);
               D.Value_Last := Len;
            end;
            FFI.Proven_Free_String (Result.Digest.Value);
         end if;
         return (Success => True, Digest_Value => D);
      end;
   end Parse;

   function Verify (Expected, Actual : Digest) return Verify_Result is
      C_Expected       : aliased FFI.C_Digest;
      C_Actual         : aliased FFI.C_Digest;
      C_Expected_Value : chars_ptr;
      C_Actual_Value   : chars_ptr;
      Result           : FFI.Bool_Result;
   begin
      C_Expected.Algorithm := To_C_Algorithm (Expected.Algorithm);
      C_Expected_Value := New_String (Expected.Value (1 .. Expected.Value_Last));
      C_Expected.Value := C_Expected_Value;
      C_Expected.Value_Len := size_t (Expected.Value_Last);

      C_Actual.Algorithm := To_C_Algorithm (Actual.Algorithm);
      C_Actual_Value := New_String (Actual.Value (1 .. Actual.Value_Last));
      C_Actual.Value := C_Actual_Value;
      C_Actual.Value_Len := size_t (Actual.Value_Last);

      Result := FFI.Digest_Verify (C_Expected'Access, C_Actual'Access);

      Free (C_Expected_Value);
      Free (C_Actual_Value);

      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      return (Success => True, Match => Boolean (Result.Value));
   end Verify;

   function To_String (D : Digest) return String is
      Algo_Str : constant String := (case D.Algorithm is
         when SHA256 => "sha256",
         when SHA384 => "sha384",
         when SHA512 => "sha512",
         when Blake3 => "blake3");
   begin
      return Algo_Str & ":" & D.Value (1 .. D.Value_Last);
   end To_String;

   function Is_Valid (Digest_String : String) return Boolean is
      Result : constant Parse_Result := Parse (Digest_String);
   begin
      return Result.Success;
   end Is_Valid;

end Proven.Safe_Digest;
