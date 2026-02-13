-------------------------------------------------------------------------------
--  Proven.Safe_HTTP.URL - URL Encoding Operations
--  SPDX-License-Identifier: PMPL-1.0-or-later
--
--  RFC 3986 percent encoding for URL query parameters and paths.
--  Formally verified via Idris2 Proven.SafeHTTP module.
--
--  This is the Ada binding layer for the Idris2 → Zig → Ada FFI chain.
-------------------------------------------------------------------------------

package Proven.Safe_HTTP.URL
   with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  URL Encoding (Percent Encoding)
   ---------------------------------------------------------------------------

   --  URL-encode a string using RFC 3986 percent encoding
   --
   --  Unreserved characters (A-Z, a-z, 0-9, -, _, ., ~) pass through unchanged.
   --  All other characters are percent-encoded as %XX (hex).
   function URL_Encode (Input : String) return String
   with Global => null,
        Post   => URL_Encode'Result'Length >= Input'Length;

   --  URL-decode a percent-encoded string
   --
   --  Returns empty string if decoding fails (invalid encoding).
   function URL_Decode (Input : String) return String
   with Global => null;

   --  Check if character is unreserved (doesn't need encoding)
   function Is_Unreserved (C : Character) return Boolean
   with Global => null,
        Inline;

   --  Check if string is validly percent-encoded
   function Is_Valid_Encoded (Input : String) return Boolean
   with Global => null;

end Proven.Safe_HTTP.URL;
