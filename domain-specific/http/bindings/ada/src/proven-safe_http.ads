-------------------------------------------------------------------------------
--  Proven.Safe_HTTP - Formally Verified HTTP Utilities
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  Ada bindings for formally verified HTTP operations.
--  Verification chain: Idris2 → Zig FFI → Ada
--
--  Modules:
--    - Proven.Safe_HTTP.URL - RFC 3986 URL encoding/decoding
--
--  Future modules (from main proven library):
--    - Proven.Safe_HTTP.Auth - HTTP authentication header parsing
--    - Proven.Safe_HTTP.Headers - Safe header manipulation
-------------------------------------------------------------------------------

package Proven.Safe_HTTP
   with Pure
is

   --  Version information
   Version : constant String := "0.1.0";

   --  Library compiled with formal verification from Idris2
   Formally_Verified : constant Boolean := True;

end Proven.Safe_HTTP;
