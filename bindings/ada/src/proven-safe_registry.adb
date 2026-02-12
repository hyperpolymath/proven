--  SPDX-License-Identifier: Apache-2.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;

package body Proven.Safe_Registry is

   ---------------------------------------------------------------------------
   --  FFI Bindings to Zig/Idris2
   ---------------------------------------------------------------------------

   type C_Image_Reference is record
      Registry      : chars_ptr;
      Registry_Len  : size_t;
      Repository    : chars_ptr;
      Repository_Len: size_t;
      Tag           : chars_ptr;
      Tag_Len       : size_t;
      Digest        : chars_ptr;
      Digest_Len    : size_t;
   end record
   with Convention => C;

   type C_Status is new int
   with Convention => C;

   type C_Image_Ref_Result is record
      Status    : C_Status;
      Reference : C_Image_Reference;
   end record
   with Convention => C;

   function proven_registry_parse
     (Input : chars_ptr; Len : size_t) return C_Image_Ref_Result
   with Import => True,
        Convention => C,
        External_Name => "proven_registry_parse";

   procedure proven_free_string (Ptr : chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "proven_free_string";

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function To_Ada_String (Ptr : chars_ptr; Len : size_t) return String is
   begin
      if Ptr = Null_Ptr then
         return "";
      end if;
      declare
         Str : constant String := Value (Ptr, Len);
      begin
         return Str;
      end;
   end To_Ada_String;

   ---------------------------------------------------------------------------
   --  Public API Implementation
   ---------------------------------------------------------------------------

   function Parse (Reference : String) return Parse_Result is
      C_Ref : constant chars_ptr := New_String (Reference);
      Result : constant C_Image_Ref_Result :=
        proven_registry_parse (C_Ref, Reference'Length);
   begin
      Free (C_Ref);

      if Result.Status /= 0 then
         return (Valid => False);
      end if;

      declare
         Ada_Ref : Image_Reference;
      begin
         Ada_Ref.Registry := To_Unbounded_String (
            To_Ada_String (Result.Reference.Registry, Result.Reference.Registry_Len));
         Ada_Ref.Repository := To_Unbounded_String (
            To_Ada_String (Result.Reference.Repository, Result.Reference.Repository_Len));
         Ada_Ref.Tag := To_Unbounded_String (
            To_Ada_String (Result.Reference.Tag, Result.Reference.Tag_Len));
         Ada_Ref.Digest := To_Unbounded_String (
            To_Ada_String (Result.Reference.Digest, Result.Reference.Digest_Len));

         --  Free C strings
         if Result.Reference.Registry /= Null_Ptr then
            proven_free_string (Result.Reference.Registry);
         end if;
         if Result.Reference.Repository /= Null_Ptr then
            proven_free_string (Result.Reference.Repository);
         end if;
         if Result.Reference.Tag /= Null_Ptr then
            proven_free_string (Result.Reference.Tag);
         end if;
         if Result.Reference.Digest /= Null_Ptr then
            proven_free_string (Result.Reference.Digest);
         end if;

         return (Valid => True, Reference => Ada_Ref);
      end;
   end Parse;

   function To_String (Ref : Image_Reference) return String is
      Result : Unbounded_String;
   begin
      --  Build: [registry/]repository[:tag][@digest]
      if Length (Ref.Registry) > 0 and then
         To_String (Ref.Registry) /= Default_Registry
      then
         Append (Result, Ref.Registry);
         Append (Result, "/");
      end if;

      Append (Result, Ref.Repository);

      if Length (Ref.Tag) > 0 and then
         To_String (Ref.Tag) /= Default_Tag
      then
         Append (Result, ":");
         Append (Result, Ref.Tag);
      end if;

      if Length (Ref.Digest) > 0 then
         Append (Result, "@");
         Append (Result, Ref.Digest);
      end if;

      return To_String (Result);
   end To_String;

   function To_Canonical (Ref : Image_Reference) return String is
      Normalized : Image_Reference := Ref;
   begin
      --  Apply defaults
      if Length (Normalized.Registry) = 0 then
         Normalized.Registry := To_Unbounded_String (Default_Registry);
      end if;
      if Length (Normalized.Tag) = 0 then
         Normalized.Tag := To_Unbounded_String (Default_Tag);
      end if;
      return To_String (Normalized);
   end To_Canonical;

   function Has_Registry (Ref : Image_Reference) return Boolean is
   begin
      return Length (Ref.Registry) > 0;
   end Has_Registry;

   function Has_Tag (Ref : Image_Reference) return Boolean is
   begin
      return Length (Ref.Tag) > 0;
   end Has_Tag;

   function Has_Digest (Ref : Image_Reference) return Boolean is
   begin
      return Length (Ref.Digest) > 0;
   end Has_Digest;

   function Get_Registry (Ref : Image_Reference) return String is
   begin
      if Length (Ref.Registry) > 0 then
         return To_String (Ref.Registry);
      else
         return Default_Registry;
      end if;
   end Get_Registry;

   function Get_Repository (Ref : Image_Reference) return String is
   begin
      return To_String (Ref.Repository);
   end Get_Repository;

   function Get_Tag (Ref : Image_Reference) return String is
   begin
      if Length (Ref.Tag) > 0 then
         return To_String (Ref.Tag);
      else
         return Default_Tag;
      end if;
   end Get_Tag;

   function Get_Digest (Ref : Image_Reference) return String is
   begin
      return To_String (Ref.Digest);
   end Get_Digest;

end Proven.Safe_Registry;
