# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Safe phone number validation following E.164 standard.

import std/[options, strutils]

type
  CountryCode* = enum
    ## Country calling codes (ITU-T E.164).
    ccUnknown = (0, "Unknown")
    ccUs = (1, "US/CA")         ## USA, Canada, Caribbean
    ccRu = (7, "RU")            ## Russia, Kazakhstan
    ccEg = (20, "EG")           ## Egypt
    ccZa = (27, "ZA")           ## South Africa
    ccGr = (30, "GR")           ## Greece
    ccNl = (31, "NL")           ## Netherlands
    ccBe = (32, "BE")           ## Belgium
    ccFr = (33, "FR")           ## France
    ccEs = (34, "ES")           ## Spain
    ccHu = (36, "HU")           ## Hungary
    ccIt = (39, "IT")           ## Italy
    ccRo = (40, "RO")           ## Romania
    ccCh = (41, "CH")           ## Switzerland
    ccAt = (43, "AT")           ## Austria
    ccUk = (44, "UK")           ## United Kingdom
    ccDk = (45, "DK")           ## Denmark
    ccSe = (46, "SE")           ## Sweden
    ccNo = (47, "NO")           ## Norway
    ccPl = (48, "PL")           ## Poland
    ccDe = (49, "DE")           ## Germany
    ccMx = (52, "MX")           ## Mexico
    ccBr = (55, "BR")           ## Brazil
    ccAu = (61, "AU")           ## Australia
    ccId = (62, "ID")           ## Indonesia
    ccPh = (63, "PH")           ## Philippines
    ccNz = (64, "NZ")           ## New Zealand
    ccSg = (65, "SG")           ## Singapore
    ccTh = (66, "TH")           ## Thailand
    ccJp = (81, "JP")           ## Japan
    ccKr = (82, "KR")           ## South Korea
    ccVn = (84, "VN")           ## Vietnam
    ccCn = (86, "CN")           ## China
    ccTr = (90, "TR")           ## Turkey
    ccIn = (91, "IN")           ## India
    ccPk = (92, "PK")           ## Pakistan
    ccAe = (971, "AE")          ## UAE
    ccSa = (966, "SA")          ## Saudi Arabia
    ccIl = (972, "IL")          ## Israel

  PhoneNumber* = object
    ## Validated phone number following E.164.
    countryCode*: CountryCode
    nationalNumber*: string

  PhoneParseError* = object of CatchableError
    ## Error raised when phone parsing fails.

proc value*(cc: CountryCode): uint16 =
  ## Get numeric value of country code.
  uint16(ord(cc))

proc fromValue*(v: uint16): CountryCode =
  ## Parse country code from numeric value.
  for cc in CountryCode:
    if ord(cc) == int(v):
      return cc
  result = ccUnknown

proc countryName*(cc: CountryCode): string =
  ## Get country name from country code.
  case cc
  of ccUs: "United States / Canada"
  of ccRu: "Russia"
  of ccEg: "Egypt"
  of ccZa: "South Africa"
  of ccGr: "Greece"
  of ccNl: "Netherlands"
  of ccBe: "Belgium"
  of ccFr: "France"
  of ccEs: "Spain"
  of ccHu: "Hungary"
  of ccIt: "Italy"
  of ccRo: "Romania"
  of ccCh: "Switzerland"
  of ccAt: "Austria"
  of ccUk: "United Kingdom"
  of ccDk: "Denmark"
  of ccSe: "Sweden"
  of ccNo: "Norway"
  of ccPl: "Poland"
  of ccDe: "Germany"
  of ccMx: "Mexico"
  of ccBr: "Brazil"
  of ccAu: "Australia"
  of ccId: "Indonesia"
  of ccPh: "Philippines"
  of ccNz: "New Zealand"
  of ccSg: "Singapore"
  of ccTh: "Thailand"
  of ccJp: "Japan"
  of ccKr: "South Korea"
  of ccVn: "Vietnam"
  of ccCn: "China"
  of ccTr: "Turkey"
  of ccIn: "India"
  of ccPk: "Pakistan"
  of ccAe: "United Arab Emirates"
  of ccSa: "Saudi Arabia"
  of ccIl: "Israel"
  of ccUnknown: "Unknown"

proc extractDigits(input: string): string =
  ## Extract only digit characters from input.
  result = newStringOfCap(input.len)
  for c in input:
    if c in '0'..'9':
      result.add c

proc parseCountryCodeFromDigits(digits: string): Option[(CountryCode, int)] =
  ## Try to parse country code from the beginning of digits.
  ## Returns (CountryCode, number of digits consumed).

  # Try 3-digit codes first (e.g., 971, 966, 972)
  if digits.len >= 3:
    try:
      let threeDigit = parseInt(digits[0..2])
      let cc = fromValue(uint16(threeDigit))
      if cc != ccUnknown:
        return some((cc, 3))
    except ValueError:
      discard

  # Try 2-digit codes (e.g., 20, 27, 33)
  if digits.len >= 2:
    try:
      let twoDigit = parseInt(digits[0..1])
      let cc = fromValue(uint16(twoDigit))
      if cc != ccUnknown:
        return some((cc, 2))
    except ValueError:
      discard

  # Try 1-digit codes (e.g., 1, 7)
  if digits.len >= 1:
    try:
      let oneDigit = parseInt(digits[0..0])
      let cc = fromValue(uint16(oneDigit))
      if cc != ccUnknown:
        return some((cc, 1))
    except ValueError:
      discard

  result = none((CountryCode, int))

proc parsePhone*(input: string): Option[PhoneNumber] =
  ## Parse phone number from string.
  ## Accepts various formats: +1-555-123-4567, 15551234567, etc.
  let trimmed = input.strip
  if trimmed.len == 0:
    return none(PhoneNumber)

  let digits = extractDigits(trimmed)

  # E.164 limits: 7-15 digits total
  if digits.len < 7 or digits.len > 15:
    return none(PhoneNumber)

  # Try to parse country code
  let ccResult = parseCountryCodeFromDigits(digits)
  if ccResult.isNone:
    return none(PhoneNumber)

  let (countryCode, ccLen) = ccResult.get

  # National number must be at least 4 digits
  let nationalNumber = digits[ccLen..^1]
  if nationalNumber.len < 4:
    return none(PhoneNumber)

  result = some(PhoneNumber(
    countryCode: countryCode,
    nationalNumber: nationalNumber
  ))

proc parsePhoneStrict*(input: string): PhoneNumber {.raises: [PhoneParseError].} =
  ## Parse phone number from string. Raises on failure.
  let parsed = parsePhone(input)
  if parsed.isNone:
    raise newException(PhoneParseError, "Invalid phone number: " & input)
  result = parsed.get

proc isValidPhone*(input: string): bool =
  ## Check if string is a valid phone number.
  parsePhone(input).isSome

proc toE164*(phone: PhoneNumber): string =
  ## Format in E.164 format (+[cc][national]).
  result = "+" & $phone.countryCode.value & phone.nationalNumber

proc `$`*(phone: PhoneNumber): string =
  ## Format phone number as E.164.
  phone.toE164

proc toInternational*(phone: PhoneNumber): string =
  ## Format with spaces for readability.
  let cc = phone.countryCode.value
  let nat = phone.nationalNumber
  let natLen = nat.len

  if natLen <= 4:
    result = "+" & $cc & " " & nat
  elif natLen <= 7:
    result = "+" & $cc & " " & nat[0..<3] & " " & nat[3..^1]
  elif natLen <= 10:
    result = "+" & $cc & " " & nat[0..<3] & " " & nat[3..<6] & " " & nat[6..^1]
  else:
    result = "+" & $cc & " " & nat

proc toNational*(phone: PhoneNumber): string =
  ## Format as national number with parentheses for area code.
  let nat = phone.nationalNumber
  let natLen = nat.len

  if natLen <= 7:
    if natLen > 3:
      result = nat[0..<3] & "-" & nat[3..^1]
    else:
      result = nat
  elif natLen <= 10:
    result = "(" & nat[0..<3] & ") " & nat[3..<6] & "-" & nat[6..^1]
  else:
    result = nat

proc digitCount*(phone: PhoneNumber): int =
  ## Get total digit count (country code + national number).
  let ccVal = phone.countryCode.value
  let ccDigits = if ccVal >= 100: 3 elif ccVal >= 10: 2 else: 1
  result = ccDigits + phone.nationalNumber.len

proc `==`*(a, b: PhoneNumber): bool =
  ## Check equality of phone numbers.
  a.countryCode == b.countryCode and a.nationalNumber == b.nationalNumber

proc hash*(phone: PhoneNumber): int =
  ## Hash function for phone numbers.
  var h = ord(phone.countryCode)
  for c in phone.nationalNumber:
    h = h * 31 + ord(c)
  result = h
