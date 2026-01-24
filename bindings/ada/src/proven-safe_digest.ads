--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe cryptographic digest operations.
--  Formally verified via Idris2 Proven.SafeDigest module.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Proven.Safe_Digest is

   --  Cryptographic hash algorithms
   type Hash_Algorithm is (SHA256, SHA384, SHA512, Blake3);

   --  Digest with algorithm and hex-encoded value
   type Digest is record
      Algorithm : Hash_Algorithm;
      Value     : Unbounded_String;  -- Hex-encoded hash
   end record;

   --  Digest parsing result
   type Parse_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Digest_Value : Digest;
         when False => Error_Message : Unbounded_String;
      end case;
   end record;

   --  Parse digest string (format: "algorithm:hexvalue")
   --
   --  Examples:
   --    "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
   --    "sha512:cf83e1357eefb8bd..."
   --    "blake3:af1349b9f5f9a1a6..."
   function Parse (Digest_String : String) return Parse_Result;

   --  Convert digest to string (algorithm:hex)
   function To_String (D : Digest) return String;

   --  Verify digest matches expected value (constant-time)
   --
   --  SECURITY: Uses constant-time comparison to prevent timing attacks
   function Verify (Expected : Digest; Actual : Digest) return Boolean;

   --  Check if digest string is valid without full parsing
   function Is_Valid (Digest_String : String) return Boolean;

   --  Get expected digest length in hex characters for algorithm
   function Expected_Length (Algo : Hash_Algorithm) return Natural;

   --  Create digest from algorithm and hex string (with validation)
   function Make_Digest
     (Algo : Hash_Algorithm;
      Hex  : String) return Parse_Result;

   --  Constants for expected lengths (hex characters)
   SHA256_Length : constant Natural := 64;   -- 32 bytes * 2
   SHA384_Length : constant Natural := 96;   -- 48 bytes * 2
   SHA512_Length : constant Natural := 128;  -- 64 bytes * 2
   Blake3_Length : constant Natural := 64;   -- 32 bytes * 2

   --  Exception for digest operations
   Digest_Error : exception;

end Proven.Safe_Digest;
