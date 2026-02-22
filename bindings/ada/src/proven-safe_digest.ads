--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Safe cryptographic digest operations -- thin FFI wrapper over libproven.
--  Digest parsing and verification done by the Idris2/Zig core.

package Proven.Safe_Digest is

   type Hash_Algorithm is (SHA256, SHA384, SHA512, Blake3);

   Max_Digest_Len : constant := 128;

   type Digest is record
      Algorithm : Hash_Algorithm;
      Value     : String (1 .. Max_Digest_Len);
      Value_Last : Natural;
   end record;

   type Parse_Result (Success : Boolean := False) is record
      case Success is
         when True  => Digest_Value : Digest;
         when False => Error_Code   : Integer;
      end case;
   end record;

   type Verify_Result (Success : Boolean := False) is record
      case Success is
         when True  => Match      : Boolean;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Parse digest string "algorithm:hexvalue" (calls proven_digest_parse).
   function Parse (Digest_String : String) return Parse_Result;

   --  Verify digest matches expected value in constant time
   --  (calls proven_digest_verify).
   function Verify (Expected, Actual : Digest) return Verify_Result;

   --  Convert digest to string "algorithm:hex".
   function To_String (D : Digest) return String;

   --  Check if digest string is valid without full parsing.
   function Is_Valid (Digest_String : String) return Boolean;

end Proven.Safe_Digest;
