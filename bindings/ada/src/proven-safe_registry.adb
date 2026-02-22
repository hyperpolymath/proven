--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

with Proven.FFI;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Proven.Safe_Registry is

   function Safe_Value (Ptr : chars_ptr; Len : size_t) return String is
   begin
      if Ptr = Null_Ptr or else Len = 0 then
         return "";
      end if;
      return Value (Ptr, Len);
   end Safe_Value;

   procedure Copy_To_Field
     (Source : String;
      Target : out String;
      Last   : out Natural)
   is
      Len : constant Natural := Natural'Min (Source'Length, Max_Component_Len);
   begin
      Target := (others => ' ');
      Last := Len;
      if Len > 0 then
         Target (1 .. Len) := Source (Source'First .. Source'First + Len - 1);
      end if;
   end Copy_To_Field;

   function Parse (Reference : String) return Parse_Result is
      C_Ref  : chars_ptr := New_String (Reference);
      Result : FFI.Image_Ref_Result;
   begin
      Result := FFI.Registry_Parse (C_Ref, Reference'Length);
      Free (C_Ref);

      if Result.Status /= FFI.PROVEN_OK then
         return (Success => False, Error_Code => Integer (Result.Status));
      end if;

      declare
         Ref : Image_Reference;
         Registry_Str : constant String :=
            Safe_Value (Result.Reference.Registry,
                        Result.Reference.Registry_Len);
         Repo_Str : constant String :=
            Safe_Value (Result.Reference.Repository,
                        Result.Reference.Repository_Len);
         Tag_Str : constant String :=
            Safe_Value (Result.Reference.Tag,
                        Result.Reference.Tag_Len);
         Digest_Str : constant String :=
            Safe_Value (Result.Reference.Digest,
                        Result.Reference.Digest_Len);
      begin
         Copy_To_Field (Registry_Str, Ref.Registry, Ref.Registry_Last);
         Copy_To_Field (Repo_Str, Ref.Repository, Ref.Repository_Last);
         Copy_To_Field (Tag_Str, Ref.Tag, Ref.Tag_Last);
         Copy_To_Field (Digest_Str, Ref.Digest, Ref.Digest_Last);

         --  Free C strings
         if Result.Reference.Registry /= Null_Ptr then
            FFI.Proven_Free_String (Result.Reference.Registry);
         end if;
         if Result.Reference.Repository /= Null_Ptr then
            FFI.Proven_Free_String (Result.Reference.Repository);
         end if;
         if Result.Reference.Tag /= Null_Ptr then
            FFI.Proven_Free_String (Result.Reference.Tag);
         end if;
         if Result.Reference.Digest /= Null_Ptr then
            FFI.Proven_Free_String (Result.Reference.Digest);
         end if;

         return (Success => True, Reference => Ref);
      end;
   end Parse;

end Proven.Safe_Registry;
