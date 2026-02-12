# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafePhone - Phone number validation for Nickel
#
# Provides phone number validation and formatting following
# E.164 standard. Validates structure without performing lookups.

{
  # Result type
  Result = {
    value | String,
    ok | Bool,
  },

  # Parsed phone number
  PhoneNumber = {
    country_code | String,
    national_number | String,
    e164 | String,
  },

  # E.164 constraints
  MIN_PHONE_LENGTH = 3,
  MAX_PHONE_LENGTH = 15,

  # Common country codes
  COUNTRY_CODES = {
    US = "1",
    UK = "44",
    DE = "49",
    FR = "33",
    IT = "39",
    ES = "34",
    JP = "81",
    CN = "86",
    IN = "91",
    BR = "55",
    MX = "52",
    AU = "61",
    CA = "1",
  },

  # Check if string contains only digits
  is_digits_only
    : String -> Bool
    = fun s =>
      let chars = std.string.characters s in
      std.array.all (fun c => c >= "0" && c <= "9") chars,

  # Strip non-digit characters from phone string
  strip_non_digits
    : String -> String
    = fun s =>
      let chars = std.string.characters s in
      let digits = std.array.filter (fun c => c >= "0" && c <= "9") chars in
      std.string.join "" digits,

  # Check if phone number starts with + and valid E.164 format
  is_valid_e164
    : String -> Bool
    = fun phone =>
      let trimmed = std.string.trim phone in
      if std.string.length trimmed == 0 then
        false
      else if std.string.substring 0 1 trimmed != "+" then
        false
      else
        let digits_part = std.string.substring 1 (std.string.length trimmed) trimmed in
        let digits_only = strip_non_digits digits_part in
        let len = std.string.length digits_only in
        len >= MIN_PHONE_LENGTH && len <= MAX_PHONE_LENGTH && is_digits_only digits_only,

  # Validate any phone number format
  is_valid_phone
    : String -> Bool
    = fun phone =>
      let stripped = strip_non_digits phone in
      let len = std.string.length stripped in
      len >= MIN_PHONE_LENGTH && len <= MAX_PHONE_LENGTH,

  # Convert to E.164 format (assumes country code is included or defaults to US)
  to_e164
    : String -> String -> { value : String, ok : Bool }
    = fun phone default_country =>
      let stripped = strip_non_digits phone in
      let len = std.string.length stripped in
      if len < MIN_PHONE_LENGTH || len > MAX_PHONE_LENGTH then
        { value = "", ok = false }
      else if std.string.starts_with "+" phone then
        { value = "+" ++ stripped, ok = true }
      else
        let country_code =
          if default_country == "US" || default_country == "CA" then "1"
          else if default_country == "UK" then "44"
          else if default_country == "DE" then "49"
          else if default_country == "FR" then "33"
          else if default_country == "JP" then "81"
          else if default_country == "CN" then "86"
          else if default_country == "IN" then "91"
          else if default_country == "AU" then "61"
          else "1"  # Default to US
        in
        { value = "+" ++ country_code ++ stripped, ok = true },

  # Extract country code from E.164 number
  extract_country_code
    : String -> { value : String, ok : Bool }
    = fun e164 =>
      if !(is_valid_e164 e164) then
        { value = "", ok = false }
      else
        let digits = strip_non_digits (std.string.substring 1 (std.string.length e164) e164) in
        # Try to match known country codes (simplified)
        if std.string.starts_with "1" digits then
          { value = "1", ok = true }  # NANP (US/Canada)
        else if std.string.starts_with "44" digits then
          { value = "44", ok = true }  # UK
        else if std.string.starts_with "49" digits then
          { value = "49", ok = true }  # Germany
        else if std.string.starts_with "33" digits then
          { value = "33", ok = true }  # France
        else if std.string.starts_with "81" digits then
          { value = "81", ok = true }  # Japan
        else if std.string.starts_with "86" digits then
          { value = "86", ok = true }  # China
        else if std.string.starts_with "91" digits then
          { value = "91", ok = true }  # India
        else
          # Return first 1-3 digits as country code
          { value = std.string.substring 0 (std.number.min 3 (std.string.length digits)) digits, ok = true },

  # Format phone for display (US format example)
  format_us_phone
    : String -> { value : String, ok : Bool }
    = fun phone =>
      let stripped = strip_non_digits phone in
      let len = std.string.length stripped in
      if len == 10 then
        let area = std.string.substring 0 3 stripped in
        let exchange = std.string.substring 3 6 stripped in
        let subscriber = std.string.substring 6 10 stripped in
        { value = "(" ++ area ++ ") " ++ exchange ++ "-" ++ subscriber, ok = true }
      else if len == 11 && std.string.starts_with "1" stripped then
        let area = std.string.substring 1 4 stripped in
        let exchange = std.string.substring 4 7 stripped in
        let subscriber = std.string.substring 7 11 stripped in
        { value = "+1 (" ++ area ++ ") " ++ exchange ++ "-" ++ subscriber, ok = true }
      else
        { value = stripped, ok = false },

  # Contracts
  E164Phone = std.contract.from_predicate is_valid_e164,
  Phone = std.contract.from_predicate is_valid_phone,
}
