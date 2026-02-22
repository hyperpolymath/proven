# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Safe phone number validation following E.164 standard.
# Thin wrapper over libproven FFI -- all logic lives in Idris.

import std/options
import lib_proven

type
  PhoneNumber* = object
    ## Validated phone number following E.164.
    ## Stores the parsed country code and national number as returned
    ## by libproven.
    countryCode*: uint16       ## Numeric country calling code
    nationalNumber*: uint64    ## National number as integer

  PhoneParseError* = object of CatchableError
    ## Error raised when phone parsing fails.

proc parsePhone*(input: string): Option[PhoneNumber] =
  ## Parse phone number from string via libproven.
  ## Accepts various formats: +1-555-123-4567, 15551234567, etc.
  ## Returns None if parsing fails.
  if input.len == 0:
    return none(PhoneNumber)
  let res = provenPhoneParse(unsafeAddr input[0], csize_t(input.len))
  if res.status != PROVEN_OK or not res.is_valid:
    return none(PhoneNumber)
  some(PhoneNumber(
    countryCode: res.country_code,
    nationalNumber: res.national_number
  ))

proc parsePhoneStrict*(input: string): PhoneNumber {.raises: [PhoneParseError].} =
  ## Parse phone number from string.  Raises PhoneParseError on failure.
  let parsed = parsePhone(input)
  if parsed.isNone:
    raise newException(PhoneParseError,
      "Invalid phone number: " & input)
  parsed.get

proc isValidPhone*(input: string): bool =
  ## Check if a string is a valid phone number.
  parsePhone(input).isSome

proc toE164*(phone: PhoneNumber): Option[string] =
  ## Format phone number in E.164 format (+[cc][national]) via libproven.
  ## Returns None if formatting fails.
  let res = provenPhoneFormatE164(phone.countryCode, phone.nationalNumber)
  if res.status != PROVEN_OK or res.value == nil:
    return none(string)
  result = some($res.value)
  provenFreeString(res.value)

proc `$`*(phone: PhoneNumber): string =
  ## Format phone number as E.164.  Falls back to a basic concatenation
  ## if libproven formatting fails.
  let formatted = phone.toE164()
  if formatted.isSome:
    formatted.get
  else:
    "+" & $phone.countryCode & $phone.nationalNumber

proc `==`*(a, b: PhoneNumber): bool =
  ## Check equality of two phone numbers.
  a.countryCode == b.countryCode and a.nationalNumber == b.nationalNumber
