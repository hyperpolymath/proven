// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeI18n - Internationalization utilities that cannot crash.
 *
 * Provides BCP 47 language tag parsing and validation, locale handling,
 * and RTL detection for internationalization support.
 */

/** Error types for i18n operations */
type i18nError =
  | InvalidLanguageTag
  | InvalidLocale
  | InvalidLanguageCode
  | InvalidCountryCode
  | InvalidScriptCode
  | InvalidVariant

/** Common ISO 639-1 two-letter language codes */
type languageCode =
  | En // English
  | Es // Spanish
  | Fr // French
  | De // German
  | It // Italian
  | Pt // Portuguese
  | Ru // Russian
  | Zh // Chinese
  | Ja // Japanese
  | Ko // Korean
  | Ar // Arabic
  | Hi // Hindi
  | Bn // Bengali
  | Nl // Dutch
  | Pl // Polish
  | Tr // Turkish
  | Vi // Vietnamese
  | Th // Thai
  | Sv // Swedish
  | No // Norwegian
  | Da // Danish
  | Fi // Finnish
  | Cs // Czech
  | El // Greek
  | He // Hebrew
  | Hu // Hungarian
  | Id // Indonesian
  | Ms // Malay
  | Ro // Romanian
  | Uk // Ukrainian

/** Get the language name in English */
let languageName = (code: languageCode): string => {
  switch code {
  | En => "English"
  | Es => "Spanish"
  | Fr => "French"
  | De => "German"
  | It => "Italian"
  | Pt => "Portuguese"
  | Ru => "Russian"
  | Zh => "Chinese"
  | Ja => "Japanese"
  | Ko => "Korean"
  | Ar => "Arabic"
  | Hi => "Hindi"
  | Bn => "Bengali"
  | Nl => "Dutch"
  | Pl => "Polish"
  | Tr => "Turkish"
  | Vi => "Vietnamese"
  | Th => "Thai"
  | Sv => "Swedish"
  | No => "Norwegian"
  | Da => "Danish"
  | Fi => "Finnish"
  | Cs => "Czech"
  | El => "Greek"
  | He => "Hebrew"
  | Hu => "Hungarian"
  | Id => "Indonesian"
  | Ms => "Malay"
  | Ro => "Romanian"
  | Uk => "Ukrainian"
  }
}

/** Get the two-letter code as string */
let languageCodeToString = (code: languageCode): string => {
  switch code {
  | En => "en"
  | Es => "es"
  | Fr => "fr"
  | De => "de"
  | It => "it"
  | Pt => "pt"
  | Ru => "ru"
  | Zh => "zh"
  | Ja => "ja"
  | Ko => "ko"
  | Ar => "ar"
  | Hi => "hi"
  | Bn => "bn"
  | Nl => "nl"
  | Pl => "pl"
  | Tr => "tr"
  | Vi => "vi"
  | Th => "th"
  | Sv => "sv"
  | No => "no"
  | Da => "da"
  | Fi => "fi"
  | Cs => "cs"
  | El => "el"
  | He => "he"
  | Hu => "hu"
  | Id => "id"
  | Ms => "ms"
  | Ro => "ro"
  | Uk => "uk"
  }
}

/** Check if this language uses right-to-left script by default */
let isLanguageRtl = (code: languageCode): bool => {
  switch code {
  | Ar | He => true
  | _ => false
  }
}

/** Common ISO 3166-1 alpha-2 country codes */
type countryCode =
  | US // United States
  | GB // United Kingdom
  | CA // Canada
  | AU // Australia
  | DE // Germany
  | FR // France
  | ES // Spain
  | IT // Italy
  | JP // Japan
  | CN // China
  | KR // South Korea
  | IN // India
  | BR // Brazil
  | MX // Mexico
  | RU // Russia
  | NL // Netherlands
  | SE // Sweden
  | NO // Norway
  | DK // Denmark
  | FI // Finland
  | PL // Poland
  | TR // Turkey
  | SA // Saudi Arabia
  | AE // United Arab Emirates
  | IL // Israel
  | SG // Singapore
  | HK // Hong Kong
  | TW // Taiwan
  | NZ // New Zealand
  | ZA // South Africa
  | PT // Portugal

/** Get the country name in English */
let countryName = (code: countryCode): string => {
  switch code {
  | US => "United States"
  | GB => "United Kingdom"
  | CA => "Canada"
  | AU => "Australia"
  | DE => "Germany"
  | FR => "France"
  | ES => "Spain"
  | IT => "Italy"
  | JP => "Japan"
  | CN => "China"
  | KR => "South Korea"
  | IN => "India"
  | BR => "Brazil"
  | MX => "Mexico"
  | RU => "Russia"
  | NL => "Netherlands"
  | SE => "Sweden"
  | NO => "Norway"
  | DK => "Denmark"
  | FI => "Finland"
  | PL => "Poland"
  | TR => "Turkey"
  | SA => "Saudi Arabia"
  | AE => "United Arab Emirates"
  | IL => "Israel"
  | SG => "Singapore"
  | HK => "Hong Kong"
  | TW => "Taiwan"
  | NZ => "New Zealand"
  | ZA => "South Africa"
  | PT => "Portugal"
  }
}

/** Get the two-letter code as string */
let countryCodeToString = (code: countryCode): string => {
  switch code {
  | US => "US"
  | GB => "GB"
  | CA => "CA"
  | AU => "AU"
  | DE => "DE"
  | FR => "FR"
  | ES => "ES"
  | IT => "IT"
  | JP => "JP"
  | CN => "CN"
  | KR => "KR"
  | IN => "IN"
  | BR => "BR"
  | MX => "MX"
  | RU => "RU"
  | NL => "NL"
  | SE => "SE"
  | NO => "NO"
  | DK => "DK"
  | FI => "FI"
  | PL => "PL"
  | TR => "TR"
  | SA => "SA"
  | AE => "AE"
  | IL => "IL"
  | SG => "SG"
  | HK => "HK"
  | TW => "TW"
  | NZ => "NZ"
  | ZA => "ZA"
  | PT => "PT"
  }
}

/** Common ISO 15924 script codes */
type scriptCode =
  | Arab // Arabic
  | Cyrl // Cyrillic
  | Deva // Devanagari
  | Grek // Greek
  | Hans // Simplified Han
  | Hant // Traditional Han
  | Hebr // Hebrew
  | Jpan // Japanese
  | Kore // Korean
  | Latn // Latin
  | Thai // Thai

/** Get the script name in English */
let scriptName = (code: scriptCode): string => {
  switch code {
  | Arab => "Arabic"
  | Cyrl => "Cyrillic"
  | Deva => "Devanagari"
  | Grek => "Greek"
  | Hans => "Simplified Chinese"
  | Hant => "Traditional Chinese"
  | Hebr => "Hebrew"
  | Jpan => "Japanese"
  | Kore => "Korean"
  | Latn => "Latin"
  | Thai => "Thai"
  }
}

/** Get the four-letter code as string */
let scriptCodeToString = (code: scriptCode): string => {
  switch code {
  | Arab => "Arab"
  | Cyrl => "Cyrl"
  | Deva => "Deva"
  | Grek => "Grek"
  | Hans => "Hans"
  | Hant => "Hant"
  | Hebr => "Hebr"
  | Jpan => "Jpan"
  | Kore => "Kore"
  | Latn => "Latn"
  | Thai => "Thai"
  }
}

/** Check if this script is right-to-left */
let isScriptRtl = (code: scriptCode): bool => {
  switch code {
  | Arab | Hebr => true
  | _ => false
  }
}

/** A parsed BCP 47 language tag */
type languageTag = {
  language: string,
  script: option<string>,
  region: option<string>,
  variants: option<array<string>>,
  privateUse: option<string>,
}

/** A locale combining language, country, and optional variant */
type locale = {
  language: languageCode,
  country: option<countryCode>,
  script: option<scriptCode>,
  variant: option<string>,
}

/** Parse a two-letter language code */
let parseLanguageCode = (str: string): option<languageCode> => {
  let lower = Js.String2.toLowerCase(str)
  switch lower {
  | "en" => Some(En)
  | "es" => Some(Es)
  | "fr" => Some(Fr)
  | "de" => Some(De)
  | "it" => Some(It)
  | "pt" => Some(Pt)
  | "ru" => Some(Ru)
  | "zh" => Some(Zh)
  | "ja" => Some(Ja)
  | "ko" => Some(Ko)
  | "ar" => Some(Ar)
  | "hi" => Some(Hi)
  | "bn" => Some(Bn)
  | "nl" => Some(Nl)
  | "pl" => Some(Pl)
  | "tr" => Some(Tr)
  | "vi" => Some(Vi)
  | "th" => Some(Th)
  | "sv" => Some(Sv)
  | "no" => Some(No)
  | "da" => Some(Da)
  | "fi" => Some(Fi)
  | "cs" => Some(Cs)
  | "el" => Some(El)
  | "he" => Some(He)
  | "hu" => Some(Hu)
  | "id" => Some(Id)
  | "ms" => Some(Ms)
  | "ro" => Some(Ro)
  | "uk" => Some(Uk)
  | _ => None
  }
}

/** Parse a two-letter country code */
let parseCountryCode = (str: string): option<countryCode> => {
  let upper = Js.String2.toUpperCase(str)
  switch upper {
  | "US" => Some(US)
  | "GB" => Some(GB)
  | "CA" => Some(CA)
  | "AU" => Some(AU)
  | "DE" => Some(DE)
  | "FR" => Some(FR)
  | "ES" => Some(ES)
  | "IT" => Some(IT)
  | "JP" => Some(JP)
  | "CN" => Some(CN)
  | "KR" => Some(KR)
  | "IN" => Some(IN)
  | "BR" => Some(BR)
  | "MX" => Some(MX)
  | "RU" => Some(RU)
  | "NL" => Some(NL)
  | "SE" => Some(SE)
  | "NO" => Some(NO)
  | "DK" => Some(DK)
  | "FI" => Some(FI)
  | "PL" => Some(PL)
  | "TR" => Some(TR)
  | "SA" => Some(SA)
  | "AE" => Some(AE)
  | "IL" => Some(IL)
  | "SG" => Some(SG)
  | "HK" => Some(HK)
  | "TW" => Some(TW)
  | "NZ" => Some(NZ)
  | "ZA" => Some(ZA)
  | _ => None
  }
}

/** Parse a four-letter script code */
let parseScriptCode = (str: string): option<scriptCode> => {
  let capitalized =
    Js.String2.toUpperCase(Js.String2.slice(str, ~from=0, ~to_=1)) ++
    Js.String2.toLowerCase(Js.String2.sliceToEnd(str, ~from=1))
  switch capitalized {
  | "Arab" => Some(Arab)
  | "Cyrl" => Some(Cyrl)
  | "Deva" => Some(Deva)
  | "Grek" => Some(Grek)
  | "Hans" => Some(Hans)
  | "Hant" => Some(Hant)
  | "Hebr" => Some(Hebr)
  | "Jpan" => Some(Jpan)
  | "Kore" => Some(Kore)
  | "Latn" => Some(Latn)
  | "Thai" => Some(Thai)
  | _ => None
  }
}

/** Check if a character is alphabetic */
let isAlpha = (c: string): bool => {
  (c >= "a" && c <= "z") || (c >= "A" && c <= "Z")
}

/** Check if a character is a digit */
let isDigit = (c: string): bool => {
  c >= "0" && c <= "9"
}

/** Check if a string is all alphabetic */
let isAllAlpha = (str: string): bool => {
  let len = Js.String2.length(str)
  if len == 0 {
    false
  } else {
    let valid = ref(true)
    for i in 0 to len - 1 {
      if !isAlpha(Js.String2.charAt(str, i)) {
        valid := false
      }
    }
    valid.contents
  }
}

/** Check if a string is all digits */
let isAllDigits = (str: string): bool => {
  let len = Js.String2.length(str)
  if len == 0 {
    false
  } else {
    let valid = ref(true)
    for i in 0 to len - 1 {
      if !isDigit(Js.String2.charAt(str, i)) {
        valid := false
      }
    }
    valid.contents
  }
}

/** Parse a BCP 47 language tag */
let parseLanguageTag = (tag: string): result<languageTag, i18nError> => {
  let len = Js.String2.length(tag)
  if len == 0 {
    Error(InvalidLanguageTag)
  } else {
    let parts = Js.String2.split(tag, "-")
    let partsLen = Belt.Array.length(parts)

    if partsLen == 0 {
      Error(InvalidLanguageTag)
    } else {
      let language = Belt.Array.getUnsafe(parts, 0)
      let langLen = Js.String2.length(language)

      if langLen < 2 || langLen > 3 || !isAllAlpha(language) {
        Error(InvalidLanguageCode)
      } else {
        let result: languageTag = {
          language: Js.String2.toLowerCase(language),
          script: None,
          region: None,
          variants: None,
          privateUse: None,
        }

        // Parse remaining subtags
        let idx = ref(1)
        let scriptRef = ref(None)
        let regionRef = ref(None)

        while idx.contents < partsLen {
          let subtag = Belt.Array.getUnsafe(parts, idx.contents)
          let subtagLen = Js.String2.length(subtag)

          // Script (4 letters, alphabetic)
          if subtagLen == 4 && Belt.Option.isNone(scriptRef.contents) && isAllAlpha(subtag) {
            scriptRef :=
              Some(
                Js.String2.toUpperCase(Js.String2.slice(subtag, ~from=0, ~to_=1)) ++
                Js.String2.toLowerCase(Js.String2.sliceToEnd(subtag, ~from=1)),
              )
          } else if Belt.Option.isNone(regionRef.contents) {
            // Region (2 letters or 3 digits)
            if subtagLen == 2 && isAllAlpha(subtag) {
              regionRef := Some(Js.String2.toUpperCase(subtag))
            } else if subtagLen == 3 && isAllDigits(subtag) {
              regionRef := Some(subtag)
            }
          }

          idx := idx.contents + 1
        }

        Ok({
          ...result,
          script: scriptRef.contents,
          region: regionRef.contents,
        })
      }
    }
  }
}

/** Parse a POSIX locale string (e.g., "en_US", "en_US.UTF-8") */
let parseLocale = (str: string): result<locale, i18nError> => {
  if Js.String2.length(str) == 0 {
    Error(InvalidLocale)
  } else {
    // Strip encoding suffix if present
    let localeStr = switch Js.String2.indexOf(str, ".") {
    | -1 => str
    | pos => Js.String2.slice(str, ~from=0, ~to_=pos)
    }

    // Split by underscore
    let parts = Js.String2.split(localeStr, "_")

    if Belt.Array.length(parts) == 0 {
      Error(InvalidLocale)
    } else {
      let langStr = Belt.Array.getUnsafe(parts, 0)
      switch parseLanguageCode(langStr) {
      | None => Error(InvalidLanguageCode)
      | Some(language) =>
        let country = if Belt.Array.length(parts) > 1 {
          parseCountryCode(Belt.Array.getUnsafe(parts, 1))
        } else {
          None
        }

        Ok({
          language: language,
          country: country,
          script: None,
          variant: None,
        })
      }
    }
  }
}

/** Format locale as BCP 47 language tag */
let localeToLanguageTag = (locale: locale): string => {
  let parts = [languageCodeToString(locale.language)]

  switch locale.script {
  | Some(s) => {
      let _ = Js.Array2.push(parts, scriptCodeToString(s))
    }
  | None => ()
  }

  switch locale.country {
  | Some(c) => {
      let _ = Js.Array2.push(parts, countryCodeToString(c))
    }
  | None => ()
  }

  Js.Array2.joinWith(parts, "-")
}

/** Format as POSIX locale string (e.g., "en_US") */
let localeToPosix = (locale: locale): string => {
  switch locale.country {
  | Some(c) => languageCodeToString(locale.language) ++ "_" ++ countryCodeToString(c)
  | None => languageCodeToString(locale.language)
  }
}

/** Check if this locale uses right-to-left script */
let isLocaleRtl = (locale: locale): bool => {
  switch locale.script {
  | Some(s) => isScriptRtl(s)
  | None => isLanguageRtl(locale.language)
  }
}

/** Check if a string is a valid BCP 47 language tag */
let isValidLanguageTag = (tag: string): bool => {
  switch parseLanguageTag(tag) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Check if a string is a valid POSIX locale */
let isValidLocale = (str: string): bool => {
  switch parseLocale(str) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Check if a string is a valid two-letter language code */
let isValidLanguageCode = (str: string): bool => {
  Belt.Option.isSome(parseLanguageCode(str))
}

/** Check if a string is a valid two-letter country code */
let isValidCountryCode = (str: string): bool => {
  Belt.Option.isSome(parseCountryCode(str))
}

/** Common locale constants */
module Locales = {
  let enUS: locale = {language: En, country: Some(US), script: None, variant: None}
  let enGB: locale = {language: En, country: Some(GB), script: None, variant: None}
  let enAU: locale = {language: En, country: Some(AU), script: None, variant: None}
  let enCA: locale = {language: En, country: Some(CA), script: None, variant: None}
  let esES: locale = {language: Es, country: Some(ES), script: None, variant: None}
  let esMX: locale = {language: Es, country: Some(MX), script: None, variant: None}
  let frFR: locale = {language: Fr, country: Some(FR), script: None, variant: None}
  let frCA: locale = {language: Fr, country: Some(CA), script: None, variant: None}
  let deDE: locale = {language: De, country: Some(DE), script: None, variant: None}
  let itIT: locale = {language: It, country: Some(IT), script: None, variant: None}
  let ptBR: locale = {language: Pt, country: Some(BR), script: None, variant: None}
  let ptPT: locale = {language: Pt, country: Some(PT), script: None, variant: None}
  let ruRU: locale = {language: Ru, country: Some(RU), script: None, variant: None}
  let zhCN: locale = {language: Zh, country: Some(CN), script: None, variant: None}
  let zhTW: locale = {language: Zh, country: Some(TW), script: None, variant: None}
  let jaJP: locale = {language: Ja, country: Some(JP), script: None, variant: None}
  let koKR: locale = {language: Ko, country: Some(KR), script: None, variant: None}
  let arSA: locale = {language: Ar, country: Some(SA), script: None, variant: None}
  let heIL: locale = {language: He, country: Some(IL), script: None, variant: None}
  let hiIN: locale = {language: Hi, country: Some(IN), script: None, variant: None}
}
