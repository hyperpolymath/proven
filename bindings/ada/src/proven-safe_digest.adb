--  SPDX-License-Identifier: Apache-2.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Proven.Safe_Digest is

   ---------------------------------------------------------------------------
   --  FFI Bindings to Zig/Idris2
   ---------------------------------------------------------------------------

   type C_Hash_Algorithm is new unsigned_char
   with Convention => C;

   type C_Digest is record
      Algorithm : C_Hash_Algorithm;
      Value     : chars_ptr;
      Value_Len : size_t;
   end record
   with Convention => C;

   type C_Status is new int
   with Convention => C;

   type C_Digest_Result is record
      Status : C_Status;
      Digest : C_Digest;
   end record
   with Convention => C;

   type C_Bool_Result is record
      Status : C_Status;
      Value  : unsigned_char;  -- 0 = false, 1 = true
   end record
   with Convention => C;

   function proven_digest_parse
     (Input : chars_ptr; Len : size_t) return C_Digest_Result
   with Import => True,
        Convention => C,
        External_Name => "proven_digest_parse";

   function proven_digest_verify
     (Expected : access C_Digest; Actual : access C_Digest) return C_Bool_Result
   with Import => True,
        Convention => C,
        External_Name => "proven_digest_verify";

   function proven_digest_to_string
     (D : access C_Digest) return chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "proven_digest_to_string";

   procedure proven_free_string (Ptr : chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "proven_free_string";

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function To_C_Algorithm (Algo : Hash_Algorithm) return C_Hash_Algorithm is
   begin
      case Algo is
         when SHA256 => return 0;
         when SHA384 => return 1;
         when SHA512 => return 2;
         when Blake3 => return 3;
      end case;
   end To_C_Algorithm;

   function From_C_Algorithm (C_Algo : C_Hash_Algorithm) return Hash_Algorithm is
   begin
      case C_Algo is
         when 0 => return SHA256;
         when 1 => return SHA384;
         when 2 => return SHA512;
         when 3 => return Blake3;
         when others => return SHA256;  -- Default fallback
      end case;
   end From_C_Algorithm;

   function To_Ada_String (Ptr : chars_ptr; Len : size_t) return String is
   begin
      if Ptr = Null_Ptr then
         return "";
      end if;
      return Value (Ptr, Len);
   end To_Ada_String;

   ---------------------------------------------------------------------------
   --  Public API Implementation
   ---------------------------------------------------------------------------

   function Parse (Digest_String : String) return Parse_Result is
      C_Input : constant chars_ptr := New_String (Digest_String);
      Result  : constant C_Digest_Result :=
        proven_digest_parse (C_Input, Digest_String'Length);
   begin
      Free (C_Input);

      if Result.Status /= 0 then
         return (Valid => False,
                 Error_Message => To_Unbounded_String ("Parse failed"));
      end if;

      declare
         Ada_Digest : Digest;
      begin
         Ada_Digest.Algorithm := From_C_Algorithm (Result.Digest.Algorithm);
         Ada_Digest.Value := To_Unbounded_String (
            To_Ada_String (Result.Digest.Value, Result.Digest.Value_Len));

         --  Free C string
         if Result.Digest.Value /= Null_Ptr then
            proven_free_string (Result.Digest.Value);
         end if;

         return (Valid => True, Digest_Value => Ada_Digest);
      end;
   end Parse;

   function To_String (D : Digest) return String is
      Algo_Str : constant String := (case D.Algorithm is
         when SHA256 => "sha256",
         when SHA384 => "sha384",
         when SHA512 => "sha512",
         when Blake3 => "blake3");
   begin
      return Algo_Str & ":" & To_String (D.Value);
   end To_String;

   function Verify (Expected : Digest; Actual : Digest) return Boolean is
      C_Expected : aliased C_Digest;
      C_Actual   : aliased C_Digest;
      C_Expected_Value : chars_ptr;
      C_Actual_Value   : chars_ptr;
      Result     : C_Bool_Result;
   begin
      --  Convert to C structures
      C_Expected.Algorithm := To_C_Algorithm (Expected.Algorithm);
      C_Expected_Value := New_String (To_String (Expected.Value));
      C_Expected.Value := C_Expected_Value;
      C_Expected.Value_Len := Length (Expected.Value);

      C_Actual.Algorithm := To_C_Algorithm (Actual.Algorithm);
      C_Actual_Value := New_String (To_String (Actual.Value));
      C_Actual.Value := C_Actual_Value;
      C_Actual.Value_Len := Length (Actual.Value);

      --  Call FFI
      Result := proven_digest_verify (C_Expected'Access, C_Actual'Access);

      --  Cleanup
      Free (C_Expected_Value);
      Free (C_Actual_Value);

      return Result.Status = 0 and then Result.Value /= 0;
   end Verify;

   function Is_Valid (Digest_String : String) return Boolean is
      Result : constant Parse_Result := Parse (Digest_String);
   begin
      return Result.Valid;
   end Is_Valid;

   function Expected_Length (Algo : Hash_Algorithm) return Natural is
   begin
      case Algo is
         when SHA256 => return SHA256_Length;
         when SHA384 => return SHA384_Length;
         when SHA512 => return SHA512_Length;
         when Blake3 => return Blake3_Length;
      end case;
   end Expected_Length;

   function Make_Digest
     (Algo : Hash_Algorithm;
      Hex  : String) return Parse_Result
   is
      Expected : constant Natural := Expected_Length (Algo);
   begin
      --  Validate length
      if Hex'Length /= Expected then
         return (Valid => False,
                 Error_Message => To_Unbounded_String (
                    "Invalid length: expected " & Natural'Image (Expected)));
      end if;

      --  Validate all hex characters
      for C of Hex loop
         if not Is_Hexadecimal_Digit (C) then
            return (Valid => False,
                    Error_Message => To_Unbounded_String ("Non-hex character"));
         end if;
      end loop;

      --  Create digest
      return (Valid => True,
              Digest_Value => (Algorithm => Algo,
                               Value => To_Unbounded_String (To_Lower (Hex))));
   end Make_Digest;

end Proven.Safe_Digest;
