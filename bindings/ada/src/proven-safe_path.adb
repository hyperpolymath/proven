--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Proven.Safe_Path is

   type Byte_Array is array (Natural range <>) of aliased unsigned_char;

   function To_Bytes (S : String) return Byte_Array is
      Result : Byte_Array (0 .. S'Length - 1);
   begin
      for I in S'Range loop
         Result (I - S'First) := unsigned_char (Character'Pos (S (I)));
      end loop;
      return Result;
   end To_Bytes;

   function Has_Traversal (Path : String) return Safe_Bool_Result is
      Bytes  : aliased Byte_Array := To_Bytes (Path);
      Result : FFI.Bool_Result;
   begin
      if Path'Length = 0 then
         return (Success => True, Value => False);
      end if;
      Result := FFI.Path_Has_Traversal
        (Bytes (Bytes'First)'Access, Bytes'Length);
      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      return (Success => True, Value => Boolean (Result.Value));
   end Has_Traversal;

   function Sanitize_Filename (Filename : String) return Safe_String_Result is
      Bytes  : aliased Byte_Array := To_Bytes (Filename);
      Result : FFI.String_Result;
   begin
      if Filename'Length = 0 then
         return (Success => True, Value => (others => ' '), Last => 0);
      end if;
      Result := FFI.Path_Sanitize_Filename
        (Bytes (Bytes'First)'Access, Bytes'Length);
      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;
      if Result.Value = Null_Ptr then
         return (Success => False,
                 Error_Code => Integer (FFI.PROVEN_ERR_NULL_POINTER));
      end if;
      declare
         C_Str : constant String := Value (Result.Value, Result.Length);
         Res   : Safe_String_Result (Success => True);
      begin
         Res.Last := C_Str'Length;
         Res.Value (1 .. Res.Last) := C_Str;
         FFI.Proven_Free_String (Result.Value);
         return Res;
      end;
   end Sanitize_Filename;

end Proven.Safe_Path;
