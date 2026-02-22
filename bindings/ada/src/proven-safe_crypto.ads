--  SPDX-License-Identifier: MPL-2.0
--  (PMPL-1.0-or-later preferred; MPL-2.0 required for GNAT ecosystem)
--  Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

--  Cryptographic safety operations -- thin FFI wrapper over libproven.
--  Constant-time comparison and RNG provided by the Idris2/Zig core.

package Proven.Safe_Crypto is

   type Safe_Bool_Result (Success : Boolean := False) is record
      case Success is
         when True  => Value      : Boolean;
         when False => Error_Code : Integer;
      end case;
   end record;

   --  Compare two strings in constant time to prevent timing attacks
   --  (calls proven_crypto_constant_time_eq).
   function Constant_Time_Compare (A, B : String) return Safe_Bool_Result;

   --  Fill a byte buffer with cryptographically secure random bytes
   --  (calls proven_crypto_random_bytes).
   --  Returns True on success, False on failure.
   function Random_Bytes (Buffer : out String) return Boolean;

end Proven.Safe_Crypto;
