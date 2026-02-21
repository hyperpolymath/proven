// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

//// SafePhone - Phone number validation that cannot crash.
////
//// Provides safe phone number parsing following E.164 format.

import gleam/int
import gleam/list
import gleam/string

/// Country calling codes (ITU-T E.164).
pub type CountryCode {
  Us
  Ru
  Eg
  Za
  Fr
  Es
  It
  Uk
  De
  Mx
  Br
  Au
  Jp
  Kr
  Cn
  In
  Nl
  Be
  At
  Ch
  Se
  No
  Dk
  Pl
  Pt
  Ie
  Nz
  Sg
  Hk
  Unknown
}

/// Error types for phone operations.
pub type PhoneError {
  EmptyInput
  TooShort(actual_length: Int)
  TooLong(actual_length: Int)
  UnknownCountryCode
  InvalidFormat(message: String)
}

/// A validated phone number.
pub opaque type PhoneNumber {
  PhoneNumber(country_code: CountryCode, national_number: String)
}

/// Get the numeric value of a country code.
pub fn country_code_value(country_code: CountryCode) -> Int {
  case country_code {
    Us -> 1
    Ru -> 7
    Eg -> 20
    Za -> 27
    Nl -> 31
    Be -> 32
    Fr -> 33
    Es -> 34
    It -> 39
    Ch -> 41
    At -> 43
    Uk -> 44
    Dk -> 45
    Se -> 46
    No -> 47
    Pl -> 48
    De -> 49
    Mx -> 52
    Br -> 55
    Au -> 61
    Nz -> 64
    Sg -> 65
    Jp -> 81
    Kr -> 82
    Cn -> 86
    In -> 91
    Hk -> 852
    Ie -> 353
    Pt -> 351
    Unknown -> 0
  }
}

/// Get country code from numeric value.
pub fn country_code_from_value(value: Int) -> CountryCode {
  case value {
    1 -> Us
    7 -> Ru
    20 -> Eg
    27 -> Za
    31 -> Nl
    32 -> Be
    33 -> Fr
    34 -> Es
    39 -> It
    41 -> Ch
    43 -> At
    44 -> Uk
    45 -> Dk
    46 -> Se
    47 -> No
    48 -> Pl
    49 -> De
    52 -> Mx
    55 -> Br
    61 -> Au
    64 -> Nz
    65 -> Sg
    81 -> Jp
    82 -> Kr
    86 -> Cn
    91 -> In
    351 -> Pt
    353 -> Ie
    852 -> Hk
    _ -> Unknown
  }
}

/// Get the country name for a country code.
pub fn country_name(country_code: CountryCode) -> String {
  case country_code {
    Us -> "United States/Canada"
    Ru -> "Russia"
    Eg -> "Egypt"
    Za -> "South Africa"
    Nl -> "Netherlands"
    Be -> "Belgium"
    Fr -> "France"
    Es -> "Spain"
    It -> "Italy"
    Ch -> "Switzerland"
    At -> "Austria"
    Uk -> "United Kingdom"
    Dk -> "Denmark"
    Se -> "Sweden"
    No -> "Norway"
    Pl -> "Poland"
    De -> "Germany"
    Mx -> "Mexico"
    Br -> "Brazil"
    Au -> "Australia"
    Nz -> "New Zealand"
    Sg -> "Singapore"
    Jp -> "Japan"
    Kr -> "South Korea"
    Cn -> "China"
    In -> "India"
    Pt -> "Portugal"
    Ie -> "Ireland"
    Hk -> "Hong Kong"
    Unknown -> "Unknown"
  }
}

/// Parse a phone number from string.
/// Accepts formats like: +1 555 123 4567, 15551234567, +1-555-123-4567
pub fn parse(input: String) -> Result(PhoneNumber, PhoneError) {
  let trimmed = string.trim(input)
  case string.is_empty(trimmed) {
    True -> Error(EmptyInput)
    False -> {
      // Extract only digits
      let digits = extract_digits(trimmed)
      let digit_count = string.length(digits)

      case digit_count < 7 {
        True -> Error(TooShort(actual_length: digit_count))
        False ->
          case digit_count > 15 {
            True -> Error(TooLong(actual_length: digit_count))
            False -> parse_with_country_code(digits)
          }
      }
    }
  }
}

fn extract_digits(input: String) -> String {
  input
  |> string.to_graphemes()
  |> list.filter(is_digit)
  |> string.concat()
}

fn is_digit(char: String) -> Bool {
  case char {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

fn parse_with_country_code(
  digits: String,
) -> Result(PhoneNumber, PhoneError) {
  // Try 3-digit country codes first, then 2-digit, then 1-digit
  case try_parse_country_code(digits, 3) {
    Ok(result) -> Ok(result)
    Error(_) ->
      case try_parse_country_code(digits, 2) {
        Ok(result) -> Ok(result)
        Error(_) ->
          case try_parse_country_code(digits, 1) {
            Ok(result) -> Ok(result)
            Error(_) -> Error(UnknownCountryCode)
          }
      }
  }
}

fn try_parse_country_code(
  digits: String,
  code_length: Int,
) -> Result(PhoneNumber, PhoneError) {
  let total_length = string.length(digits)
  case total_length >= code_length {
    False -> Error(InvalidFormat(message: "Not enough digits for country code"))
    True -> {
      let code_str = string.slice(digits, 0, code_length)
      let national = string.slice(digits, code_length, total_length - code_length)
      case int.parse(code_str) {
        Error(_) -> Error(InvalidFormat(message: "Invalid country code digits"))
        Ok(code_value) -> {
          let country_code = country_code_from_value(code_value)
          case country_code {
            Unknown -> Error(UnknownCountryCode)
            _ ->
              case string.length(national) >= 4 {
                False ->
                  Error(InvalidFormat(message: "National number too short"))
                True ->
                  Ok(PhoneNumber(
                    country_code: country_code,
                    national_number: national,
                  ))
              }
          }
        }
      }
    }
  }
}

/// Check if a string is a valid phone number.
pub fn is_valid(input: String) -> Bool {
  case parse(input) {
    Ok(_) -> True
    Error(_) -> False
  }
}

/// Get the country code of a phone number.
pub fn get_country_code(phone: PhoneNumber) -> CountryCode {
  phone.country_code
}

/// Get the national number (without country code).
pub fn get_national_number(phone: PhoneNumber) -> String {
  phone.national_number
}

/// Format phone number in E.164 format (+15551234567).
pub fn to_e164(phone: PhoneNumber) -> String {
  "+" <> int.to_string(country_code_value(phone.country_code)) <> phone.national_number
}

/// Format phone number in international format with spaces.
pub fn to_international(phone: PhoneNumber) -> String {
  let country_code_str = int.to_string(country_code_value(phone.country_code))
  let national = phone.national_number
  let national_length = string.length(national)

  case national_length {
    len if len <= 4 -> "+" <> country_code_str <> " " <> national
    len if len <= 7 -> {
      let part1 = string.slice(national, 0, 3)
      let part2 = string.slice(national, 3, len - 3)
      "+" <> country_code_str <> " " <> part1 <> " " <> part2
    }
    len if len <= 10 -> {
      let part1 = string.slice(national, 0, 3)
      let part2 = string.slice(national, 3, 3)
      let part3 = string.slice(national, 6, len - 6)
      "+"
      <> country_code_str
      <> " "
      <> part1
      <> " "
      <> part2
      <> " "
      <> part3
    }
    _ -> "+" <> country_code_str <> " " <> national
  }
}

/// Format phone number for display (alias for to_international).
pub fn format(phone: PhoneNumber) -> String {
  to_international(phone)
}

/// Get the total number of digits in the phone number.
pub fn digit_count(phone: PhoneNumber) -> Int {
  let country_code_digits = case country_code_value(phone.country_code) {
    v if v >= 100 -> 3
    v if v >= 10 -> 2
    _ -> 1
  }
  country_code_digits + string.length(phone.national_number)
}

/// Create a phone number from parts (validates the components).
pub fn from_parts(
  country_code: CountryCode,
  national_number: String,
) -> Result(PhoneNumber, PhoneError) {
  case country_code {
    Unknown -> Error(UnknownCountryCode)
    _ -> {
      let clean_national = extract_digits(national_number)
      let national_length = string.length(clean_national)
      case national_length < 4 {
        True -> Error(TooShort(actual_length: national_length))
        False ->
          case national_length > 14 {
            True -> Error(TooLong(actual_length: national_length))
            False ->
              Ok(PhoneNumber(
                country_code: country_code,
                national_number: clean_national,
              ))
          }
      }
    }
  }
}

/// Compare two phone numbers for equality.
pub fn equals(phone_a: PhoneNumber, phone_b: PhoneNumber) -> Bool {
  phone_a.country_code == phone_b.country_code
  && phone_a.national_number == phone_b.national_number
}
