--  SPDX-License-Identifier: PMPL-1.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Cryptographic safety operations with constant-time guarantees.

package Proven.Safe_Crypto is

   --  Compare two strings in constant time to prevent timing attacks.
   function Constant_Time_Compare (A, B : String) return Boolean;

   --  Create a zeroed string of the specified length.
   function Secure_Zero (Length : Natural) return String;

end Proven.Safe_Crypto;
