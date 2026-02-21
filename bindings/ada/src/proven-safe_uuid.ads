--  SPDX-License-Identifier: PMPL-1.0-or-later
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Safe UUID generation and validation following RFC 4122.

with Interfaces; use Interfaces;

package Proven.Safe_UUID is

   --  UUID stored as 16 bytes.
   type UUID_Bytes is array (0 .. 15) of Unsigned_8;

   --  UUID version types.
   type UUID_Version is (V1, V2, V3, V4, V5, Nil_Version, Unknown_Version);
   --  V1: Time-based
   --  V2: DCE Security
   --  V3: Name-based (MD5)
   --  V4: Random
   --  V5: Name-based (SHA-1)
   --  Nil_Version: All zeros
   --  Unknown_Version: Unrecognized version

   --  UUID variant types.
   type UUID_Variant is (NCS, RFC_4122, Microsoft, Future);

   --  A validated UUID (128 bits).
   type UUID is record
      Bytes : UUID_Bytes;
   end record;

   --  Parse result discriminated record.
   type Parse_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : UUID;
         when False => null;
      end case;
   end record;

   --  The nil UUID (all zeros).
   Nil_UUID : constant UUID;

   --  DNS namespace UUID.
   Namespace_DNS : constant UUID;

   --  URL namespace UUID.
   Namespace_URL : constant UUID;

   --  Create UUID from bytes.
   function From_Bytes (Bytes : UUID_Bytes) return UUID;

   --  Get the bytes.
   function As_Bytes (U : UUID) return UUID_Bytes;

   --  Get the UUID version.
   function Version (U : UUID) return UUID_Version;

   --  Get the UUID variant.
   function Variant (U : UUID) return UUID_Variant;

   --  Check if this is the nil UUID.
   function Is_Nil (U : UUID) return Boolean;

   --  Format as canonical string (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx).
   function Format (U : UUID) return String;

   --  Format as URN (urn:uuid:...).
   function To_URN (U : UUID) return String;

   --  Parse UUID from canonical string format.
   function Parse (S : String) return Parse_Result;

   --  Parse UUID, raising exception on failure.
   function Parse_Or_Raise (S : String) return UUID;

   --  Generate a v4 (random) UUID from provided random bytes.
   function V4_From_Bytes (Random_Bytes : UUID_Bytes) return UUID;

   --  Check if string is valid UUID format.
   function Is_Valid (S : String) return Boolean;

   --  Compare two UUIDs for equality.
   function Equal (A, B : UUID) return Boolean;

   --  Exception raised on parse failure.
   UUID_Parse_Error : exception;

private

   Nil_UUID : constant UUID := (Bytes => (others => 0));

   Namespace_DNS : constant UUID := (Bytes => (
      16#6B#, 16#A7#, 16#B8#, 16#10#, 16#9D#, 16#AD#, 16#11#, 16#D1#,
      16#80#, 16#B4#, 16#00#, 16#C0#, 16#4F#, 16#D4#, 16#30#, 16#C8#
   ));

   Namespace_URL : constant UUID := (Bytes => (
      16#6B#, 16#A7#, 16#B8#, 16#11#, 16#9D#, 16#AD#, 16#11#, 16#D1#,
      16#80#, 16#B4#, 16#00#, 16#C0#, 16#4F#, 16#D4#, 16#30#, 16#C8#
   ));

end Proven.Safe_UUID;
