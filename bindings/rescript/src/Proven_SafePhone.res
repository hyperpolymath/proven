// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafePhone - Phone number parsing and validation that cannot crash.
 *
 * Provides safe phone number operations with country code detection and formatting.
 */

/** Country codes (ISO 3166-1 alpha-2) with dialing codes */
type countryCode =
  | US
  | CA
  | GB
  | DE
  | FR
  | IT
  | ES
  | NL
  | BE
  | CH
  | AT
  | AU
  | NZ
  | JP
  | KR
  | CN
  | HK
  | SG
  | IN
  | PK
  | BD
  | ID
  | MY
  | TH
  | VN
  | PH
  | TW
  | BR
  | MX
  | AR
  | CO
  | CL
  | PE
  | ZA
  | NG
  | EG
  | KE
  | GH
  | MA
  | AE
  | SA
  | IL
  | TR
  | RU
  | UA
  | PL
  | CZ
  | SE
  | NO
  | DK
  | FI
  | IE
  | PT
  | GR
  | Other(string)

/** Phone number type */
type phoneNumber = {
  countryCode: countryCode,
  dialingCode: string,
  nationalNumber: string,
  extension: option<string>,
  rawInput: string,
}

/** Error types for phone parsing */
type parseError =
  | InvalidFormat
  | TooShort
  | TooLong
  | InvalidCountryCode
  | InvalidCharacters

/** Get the dialing code for a country */
let getDialingCode = (country: countryCode): string => {
  switch country {
  | US | CA => "1"
  | GB => "44"
  | DE => "49"
  | FR => "33"
  | IT => "39"
  | ES => "34"
  | NL => "31"
  | BE => "32"
  | CH => "41"
  | AT => "43"
  | AU => "61"
  | NZ => "64"
  | JP => "81"
  | KR => "82"
  | CN => "86"
  | HK => "852"
  | SG => "65"
  | IN => "91"
  | PK => "92"
  | BD => "880"
  | ID => "62"
  | MY => "60"
  | TH => "66"
  | VN => "84"
  | PH => "63"
  | TW => "886"
  | BR => "55"
  | MX => "52"
  | AR => "54"
  | CO => "57"
  | CL => "56"
  | PE => "51"
  | ZA => "27"
  | NG => "234"
  | EG => "20"
  | KE => "254"
  | GH => "233"
  | MA => "212"
  | AE => "971"
  | SA => "966"
  | IL => "972"
  | TR => "90"
  | RU => "7"
  | UA => "380"
  | PL => "48"
  | CZ => "420"
  | SE => "46"
  | NO => "47"
  | DK => "45"
  | FI => "358"
  | IE => "353"
  | PT => "351"
  | GR => "30"
  | Other(_) => ""
  }
}

/** Parse country code from string */
let parseCountryCode = (code: string): result<countryCode, parseError> => {
  let upper = Js.String2.toUpperCase(Js.String2.trim(code))
  switch upper {
  | "US" => Ok(US)
  | "CA" => Ok(CA)
  | "GB" | "UK" => Ok(GB)
  | "DE" => Ok(DE)
  | "FR" => Ok(FR)
  | "IT" => Ok(IT)
  | "ES" => Ok(ES)
  | "NL" => Ok(NL)
  | "BE" => Ok(BE)
  | "CH" => Ok(CH)
  | "AT" => Ok(AT)
  | "AU" => Ok(AU)
  | "NZ" => Ok(NZ)
  | "JP" => Ok(JP)
  | "KR" => Ok(KR)
  | "CN" => Ok(CN)
  | "HK" => Ok(HK)
  | "SG" => Ok(SG)
  | "IN" => Ok(IN)
  | "PK" => Ok(PK)
  | "BD" => Ok(BD)
  | "ID" => Ok(ID)
  | "MY" => Ok(MY)
  | "TH" => Ok(TH)
  | "VN" => Ok(VN)
  | "PH" => Ok(PH)
  | "TW" => Ok(TW)
  | "BR" => Ok(BR)
  | "MX" => Ok(MX)
  | "AR" => Ok(AR)
  | "CO" => Ok(CO)
  | "CL" => Ok(CL)
  | "PE" => Ok(PE)
  | "ZA" => Ok(ZA)
  | "NG" => Ok(NG)
  | "EG" => Ok(EG)
  | "KE" => Ok(KE)
  | "GH" => Ok(GH)
  | "MA" => Ok(MA)
  | "AE" => Ok(AE)
  | "SA" => Ok(SA)
  | "IL" => Ok(IL)
  | "TR" => Ok(TR)
  | "RU" => Ok(RU)
  | "UA" => Ok(UA)
  | "PL" => Ok(PL)
  | "CZ" => Ok(CZ)
  | "SE" => Ok(SE)
  | "NO" => Ok(NO)
  | "DK" => Ok(DK)
  | "FI" => Ok(FI)
  | "IE" => Ok(IE)
  | "PT" => Ok(PT)
  | "GR" => Ok(GR)
  | s if Js.String2.length(s) == 2 => Ok(Other(s))
  | _ => Error(InvalidCountryCode)
  }
}

/** Country code to string */
let countryCodeToString = (country: countryCode): string => {
  switch country {
  | US => "US"
  | CA => "CA"
  | GB => "GB"
  | DE => "DE"
  | FR => "FR"
  | IT => "IT"
  | ES => "ES"
  | NL => "NL"
  | BE => "BE"
  | CH => "CH"
  | AT => "AT"
  | AU => "AU"
  | NZ => "NZ"
  | JP => "JP"
  | KR => "KR"
  | CN => "CN"
  | HK => "HK"
  | SG => "SG"
  | IN => "IN"
  | PK => "PK"
  | BD => "BD"
  | ID => "ID"
  | MY => "MY"
  | TH => "TH"
  | VN => "VN"
  | PH => "PH"
  | TW => "TW"
  | BR => "BR"
  | MX => "MX"
  | AR => "AR"
  | CO => "CO"
  | CL => "CL"
  | PE => "PE"
  | ZA => "ZA"
  | NG => "NG"
  | EG => "EG"
  | KE => "KE"
  | GH => "GH"
  | MA => "MA"
  | AE => "AE"
  | SA => "SA"
  | IL => "IL"
  | TR => "TR"
  | RU => "RU"
  | UA => "UA"
  | PL => "PL"
  | CZ => "CZ"
  | SE => "SE"
  | NO => "NO"
  | DK => "DK"
  | FI => "FI"
  | IE => "IE"
  | PT => "PT"
  | GR => "GR"
  | Other(s) => s
  }
}

/** Extract only digits from a string */
let extractDigits = (input: string): string => {
  Js.String2.replaceByRe(input, %re("/[^0-9]/g"), "")
}

/** Check if a string contains only valid phone characters */
let hasValidCharacters = (input: string): bool => {
  Js.Re.test_(%re("/^[0-9+\\-\\.\\(\\)\\s#*ext]+$/i"), input)
}

/** Extract extension from phone number string */
let extractExtension = (input: string): (string, option<string>) => {
  let extPatterns = [
    %re("/[,;]\\s*ext\\.?\\s*(\\d+)$/i"),
    %re("/\\s+ext\\.?\\s*(\\d+)$/i"),
    %re("/\\s+x\\.?(\\d+)$/i"),
    %re("/\\s+#(\\d+)$/"),
  ]

  let foundExtension = ref(None)
  let remainingInput = ref(input)

  Belt.Array.forEach(extPatterns, pattern => {
    if Belt.Option.isNone(foundExtension.contents) {
      switch Js.Re.exec_(pattern, input) {
      | Some(result) =>
        let matched = Js.Re.captures(result)
        switch Belt.Array.get(matched, 1) {
        | Some(extMatch) =>
          switch Js.Nullable.toOption(extMatch) {
          | Some(ext) =>
            foundExtension := Some(ext)
            remainingInput :=
              Js.String2.slice(input, ~from=0, ~to_=Js.String2.length(input) - Js.String2.length(
                  Js.Re.captures(result)[0]
                  ->Js.Nullable.toOption
                  ->Belt.Option.getWithDefault(""),
                ))
          | None => ()
          }
        | None => ()
        }
      | None => ()
      }
    }
  })

  (remainingInput.contents, foundExtension.contents)
}

/** Parse a phone number with a known country code */
let parseWithCountry = (phoneStr: string, country: countryCode): result<phoneNumber, parseError> => {
  let trimmed = Js.String2.trim(phoneStr)

  if !hasValidCharacters(trimmed) {
    Error(InvalidCharacters)
  } else {
    let (withoutExt, extension) = extractExtension(trimmed)
    let digits = extractDigits(withoutExt)
    let dialingCode = getDialingCode(country)

    // Check length
    let length = Js.String2.length(digits)
    if length < 6 {
      Error(TooShort)
    } else if length > 15 {
      Error(TooLong)
    } else {
      // Remove leading country code if present
      let nationalNumber = if Js.String2.startsWith(digits, dialingCode) {
        Js.String2.sliceToEnd(digits, ~from=Js.String2.length(dialingCode))
      } else if Js.String2.startsWith(digits, "00" ++ dialingCode) {
        Js.String2.sliceToEnd(digits, ~from=2 + Js.String2.length(dialingCode))
      } else {
        digits
      }

      Ok({
        countryCode: country,
        dialingCode: dialingCode,
        nationalNumber: nationalNumber,
        extension: extension,
        rawInput: phoneStr,
      })
    }
  }
}

/** Try to detect country from dialing code and parse */
let parse = (phoneStr: string): result<phoneNumber, parseError> => {
  let trimmed = Js.String2.trim(phoneStr)

  if !hasValidCharacters(trimmed) {
    Error(InvalidCharacters)
  } else {
    let (withoutExt, extension) = extractExtension(trimmed)
    let digits = extractDigits(withoutExt)

    let length = Js.String2.length(digits)
    if length < 6 {
      Error(TooShort)
    } else if length > 15 {
      Error(TooLong)
    } else {
      // Try to detect country code
      let startsWithPlus = Js.String2.startsWith(trimmed, "+")
      let startsWithZeros = Js.String2.startsWith(digits, "00")

      let (detectedCountry, nationalNumber) = if startsWithPlus || startsWithZeros {
        // Try matching known dialing codes (longer codes first)
        let codeStart = if startsWithZeros {
          Js.String2.sliceToEnd(digits, ~from=2)
        } else {
          digits
        }

        // Check 3-digit codes first
        let threeDigitCodes = [("880", BD), ("886", TW), ("852", HK), ("971", AE), ("966", SA), ("972", IL), ("380", UA), ("420", CZ), ("358", FI), ("353", IE), ("351", PT), ("234", NG), ("254", KE), ("233", GH), ("212", MA)]

        let found3 = Belt.Array.reduce(threeDigitCodes, None, (acc, (code, country)) => {
          switch acc {
          | Some(_) => acc
          | None =>
            if Js.String2.startsWith(codeStart, code) {
              Some((country, Js.String2.sliceToEnd(codeStart, ~from=3)))
            } else {
              None
            }
          }
        })

        switch found3 {
        | Some(result) => result
        | None =>
          // Check 2-digit codes
          let twoDigitCodes = [
            ("44", GB),
            ("49", DE),
            ("33", FR),
            ("39", IT),
            ("34", ES),
            ("31", NL),
            ("32", BE),
            ("41", CH),
            ("43", AT),
            ("61", AU),
            ("64", NZ),
            ("81", JP),
            ("82", KR),
            ("86", CN),
            ("65", SG),
            ("91", IN),
            ("92", PK),
            ("62", ID),
            ("60", MY),
            ("66", TH),
            ("84", VN),
            ("63", PH),
            ("55", BR),
            ("52", MX),
            ("54", AR),
            ("57", CO),
            ("56", CL),
            ("51", PE),
            ("27", ZA),
            ("20", EG),
            ("90", TR),
            ("48", PL),
            ("46", SE),
            ("47", NO),
            ("45", DK),
            ("30", GR),
          ]

          let found2 = Belt.Array.reduce(twoDigitCodes, None, (acc, (code, country)) => {
            switch acc {
            | Some(_) => acc
            | None =>
              if Js.String2.startsWith(codeStart, code) {
                Some((country, Js.String2.sliceToEnd(codeStart, ~from=2)))
              } else {
                None
              }
            }
          })

          switch found2 {
          | Some(result) => result
          | None =>
            // Check 1-digit codes
            if Js.String2.startsWith(codeStart, "1") {
              (US, Js.String2.sliceToEnd(codeStart, ~from=1))
            } else if Js.String2.startsWith(codeStart, "7") {
              (RU, Js.String2.sliceToEnd(codeStart, ~from=1))
            } else {
              // Unknown, assume US format
              (US, codeStart)
            }
          }
        }
      } else {
        // No country indicator, assume US
        (US, digits)
      }

      Ok({
        countryCode: detectedCountry,
        dialingCode: getDialingCode(detectedCountry),
        nationalNumber: nationalNumber,
        extension: extension,
        rawInput: phoneStr,
      })
    }
  }
}

/** Check if a phone number string is valid */
let isValid = (phoneStr: string): bool => {
  switch parse(phoneStr) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Format as E.164 (international standard) */
let formatE164 = (phone: phoneNumber): string => {
  let base = `+${phone.dialingCode}${phone.nationalNumber}`
  switch phone.extension {
  | Some(ext) => `${base};ext=${ext}`
  | None => base
  }
}

/** Format for display with country-specific formatting */
let formatNational = (phone: phoneNumber): string => {
  let digits = phone.nationalNumber
  let formatted = switch phone.countryCode {
  | US | CA =>
    // (XXX) XXX-XXXX format
    if Js.String2.length(digits) == 10 {
      `(${Js.String2.slice(digits, ~from=0, ~to_=3)}) ${Js.String2.slice(
          digits,
          ~from=3,
          ~to_=6,
        )}-${Js.String2.sliceToEnd(digits, ~from=6)}`
    } else {
      digits
    }
  | GB =>
    // XXXXX XXXXXX format (simplified)
    if Js.String2.length(digits) >= 10 {
      `${Js.String2.slice(digits, ~from=0, ~to_=5)} ${Js.String2.sliceToEnd(digits, ~from=5)}`
    } else {
      digits
    }
  | DE | FR | IT | ES =>
    // XXX XXXX XXXX format (simplified)
    if Js.String2.length(digits) >= 10 {
      `${Js.String2.slice(digits, ~from=0, ~to_=3)} ${Js.String2.slice(
          digits,
          ~from=3,
          ~to_=7,
        )} ${Js.String2.sliceToEnd(digits, ~from=7)}`
    } else {
      digits
    }
  | _ => digits
  }

  switch phone.extension {
  | Some(ext) => `${formatted} ext. ${ext}`
  | None => formatted
  }
}

/** Format for international display */
let formatInternational = (phone: phoneNumber): string => {
  let base = `+${phone.dialingCode} ${phone.nationalNumber}`
  switch phone.extension {
  | Some(ext) => `${base} ext. ${ext}`
  | None => base
  }
}

/** Get the country code */
let getCountryCode = (phone: phoneNumber): countryCode => phone.countryCode

/** Get the national number */
let getNationalNumber = (phone: phoneNumber): string => phone.nationalNumber

/** Get the extension if present */
let getExtension = (phone: phoneNumber): option<string> => phone.extension

/** Check if two phone numbers are equal */
let equal = (phoneA: phoneNumber, phoneB: phoneNumber): bool => {
  phoneA.countryCode == phoneB.countryCode &&
  phoneA.nationalNumber == phoneB.nationalNumber &&
  phoneA.extension == phoneB.extension
}
