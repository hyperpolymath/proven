# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeUuid - UUID validation following RFC 4122 for Nickel
#
# Validates and parses UUIDs without throwing exceptions.

{
  # UUID v4 pattern (8-4-4-4-12 hex digits with version 4)
  uuid_v4_pattern = "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-4[0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$",

  # Generic UUID pattern (any version)
  uuid_pattern = "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$",

  # Nil UUID
  NIL = "00000000-0000-0000-0000-000000000000",

  # Check if string is a valid UUID (any version)
  is_valid
    : String -> Bool
    = fun uuid =>
      std.string.is_match uuid_pattern uuid,

  # Check if string is a valid UUID v4
  is_valid_v4
    : String -> Bool
    = fun uuid =>
      std.string.is_match uuid_v4_pattern uuid,

  # Check if UUID is nil
  is_nil
    : String -> Bool
    = fun uuid =>
      uuid == NIL,

  # Get UUID version (returns 0 if invalid)
  get_version
    : String -> Number
    = fun uuid =>
      if !(is_valid uuid) then
        0
      else
        # Version is at position 14 (index of first char after second hyphen)
        let version_char = std.string.substring 14 15 uuid in
        if std.string.is_match "[1-5]" version_char then
          std.string.to_number version_char
        else
          0,

  # Normalize UUID to lowercase
  normalize
    : String -> String
    = fun uuid =>
      std.string.lowercase uuid,

  # Parse UUID into components
  parse
    : String -> { time_low : String, time_mid : String, time_hi : String, clock_seq : String, node : String, ok : Bool }
    = fun uuid =>
      if !(is_valid uuid) then
        { time_low = "", time_mid = "", time_hi = "", clock_seq = "", node = "", ok = false }
      else
        let parts = std.string.split "-" uuid in
        {
          time_low = std.array.at 0 parts,
          time_mid = std.array.at 1 parts,
          time_hi = std.array.at 2 parts,
          clock_seq = std.array.at 3 parts,
          node = std.array.at 4 parts,
          ok = true,
        },

  # Remove hyphens from UUID
  to_hex
    : String -> String
    = fun uuid =>
      std.string.replace "-" "" uuid,

  # Format 32 hex chars as UUID
  from_hex
    : String -> { uuid : String, ok : Bool }
    = fun hex =>
      if std.string.length hex != 32 then
        { uuid = "", ok = false }
      else if !(std.string.is_match "^[0-9a-fA-F]{32}$" hex) then
        { uuid = "", ok = false }
      else
        let formatted = "%{std.string.substring 0 8 hex}-%{std.string.substring 8 12 hex}-%{std.string.substring 12 16 hex}-%{std.string.substring 16 20 hex}-%{std.string.substring 20 32 hex}" in
        { uuid = formatted, ok = true },

  # UUID contract (any version)
  UUID = std.contract.from_predicate is_valid,

  # UUID v4 contract
  UUIDv4 = std.contract.from_predicate is_valid_v4,

  # Non-nil UUID contract
  NonNilUUID = std.contract.from_predicate (fun uuid =>
    is_valid uuid && !(is_nil uuid)
  ),
}
