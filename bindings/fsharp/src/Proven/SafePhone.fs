// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe phone number parsing, validation, and formatting following ITU-T E.164.
module SafePhone =
    open System
    open System.Text.RegularExpressions

    /// Phone number type classification.
    type PhoneNumberType =
        | Mobile
        | FixedLine
        | TollFree
        | PremiumRate
        | SharedCost
        | VoIP
        | PersonalNumber
        | Pager
        | UAN   // Universal Access Number
        | Unknown

    /// ITU country calling codes.
    type CountryCallingCode =
        | CC1     // USA, Canada, Caribbean
        | CC7     // Russia, Kazakhstan
        | CC20    // Egypt
        | CC27    // South Africa
        | CC30    // Greece
        | CC31    // Netherlands
        | CC32    // Belgium
        | CC33    // France
        | CC34    // Spain
        | CC36    // Hungary
        | CC39    // Italy
        | CC40    // Romania
        | CC41    // Switzerland
        | CC43    // Austria
        | CC44    // UK
        | CC45    // Denmark
        | CC46    // Sweden
        | CC47    // Norway
        | CC48    // Poland
        | CC49    // Germany
        | CC51    // Peru
        | CC52    // Mexico
        | CC53    // Cuba
        | CC54    // Argentina
        | CC55    // Brazil
        | CC56    // Chile
        | CC57    // Colombia
        | CC58    // Venezuela
        | CC60    // Malaysia
        | CC61    // Australia
        | CC62    // Indonesia
        | CC63    // Philippines
        | CC64    // New Zealand
        | CC65    // Singapore
        | CC66    // Thailand
        | CC81    // Japan
        | CC82    // South Korea
        | CC84    // Vietnam
        | CC86    // China
        | CC90    // Turkey
        | CC91    // India
        | CC92    // Pakistan
        | CC93    // Afghanistan
        | CC94    // Sri Lanka
        | CC95    // Myanmar
        | CC98    // Iran
        | CC212   // Morocco
        | CC213   // Algeria
        | CC216   // Tunisia
        | CC218   // Libya
        | CC234   // Nigeria
        | CC254   // Kenya
        | CC255   // Tanzania
        | CC256   // Uganda
        | CC351   // Portugal
        | CC352   // Luxembourg
        | CC353   // Ireland
        | CC354   // Iceland
        | CC358   // Finland
        | CC380   // Ukraine
        | CC381   // Serbia
        | CC385   // Croatia
        | CC386   // Slovenia
        | CC420   // Czech Republic
        | CC421   // Slovakia
        | CC852   // Hong Kong
        | CC853   // Macau
        | CC855   // Cambodia
        | CC856   // Laos
        | CC880   // Bangladesh
        | CC886   // Taiwan
        | CC960   // Maldives
        | CC961   // Lebanon
        | CC962   // Jordan
        | CC963   // Syria
        | CC964   // Iraq
        | CC965   // Kuwait
        | CC966   // Saudi Arabia
        | CC967   // Yemen
        | CC968   // Oman
        | CC971   // UAE
        | CC972   // Israel
        | CC973   // Bahrain
        | CC974   // Qatar
        | CC975   // Bhutan
        | CC976   // Mongolia
        | CC977   // Nepal
        | CC992   // Tajikistan
        | CC993   // Turkmenistan
        | CC994   // Azerbaijan
        | CC995   // Georgia
        | CC996   // Kyrgyzstan
        | CC998   // Uzbekistan
        | CCUnknown

    /// Phone number parsing errors.
    type PhoneError =
        | InvalidCharacter of char
        | TooShort of int
        | TooLong of int
        | InvalidCountryCode of string
        | InvalidNationalNumber of string
        | EmptyInput

    /// A validated phone number.
    type PhoneNumber = {
        CountryCode: CountryCallingCode
        NationalNumber: string
    }

    /// Get the numeric value of a country calling code.
    let getCountryCodeValue (code: CountryCallingCode) : int =
        match code with
        | CC1 -> 1 | CC7 -> 7 | CC20 -> 20 | CC27 -> 27 | CC30 -> 30
        | CC31 -> 31 | CC32 -> 32 | CC33 -> 33 | CC34 -> 34 | CC36 -> 36
        | CC39 -> 39 | CC40 -> 40 | CC41 -> 41 | CC43 -> 43 | CC44 -> 44
        | CC45 -> 45 | CC46 -> 46 | CC47 -> 47 | CC48 -> 48 | CC49 -> 49
        | CC51 -> 51 | CC52 -> 52 | CC53 -> 53 | CC54 -> 54 | CC55 -> 55
        | CC56 -> 56 | CC57 -> 57 | CC58 -> 58 | CC60 -> 60 | CC61 -> 61
        | CC62 -> 62 | CC63 -> 63 | CC64 -> 64 | CC65 -> 65 | CC66 -> 66
        | CC81 -> 81 | CC82 -> 82 | CC84 -> 84 | CC86 -> 86 | CC90 -> 90
        | CC91 -> 91 | CC92 -> 92 | CC93 -> 93 | CC94 -> 94 | CC95 -> 95
        | CC98 -> 98 | CC212 -> 212 | CC213 -> 213 | CC216 -> 216 | CC218 -> 218
        | CC234 -> 234 | CC254 -> 254 | CC255 -> 255 | CC256 -> 256
        | CC351 -> 351 | CC352 -> 352 | CC353 -> 353 | CC354 -> 354 | CC358 -> 358
        | CC380 -> 380 | CC381 -> 381 | CC385 -> 385 | CC386 -> 386
        | CC420 -> 420 | CC421 -> 421 | CC852 -> 852 | CC853 -> 853
        | CC855 -> 855 | CC856 -> 856 | CC880 -> 880 | CC886 -> 886
        | CC960 -> 960 | CC961 -> 961 | CC962 -> 962 | CC963 -> 963
        | CC964 -> 964 | CC965 -> 965 | CC966 -> 966 | CC967 -> 967
        | CC968 -> 968 | CC971 -> 971 | CC972 -> 972 | CC973 -> 973
        | CC974 -> 974 | CC975 -> 975 | CC976 -> 976 | CC977 -> 977
        | CC992 -> 992 | CC993 -> 993 | CC994 -> 994 | CC995 -> 995
        | CC996 -> 996 | CC998 -> 998 | CCUnknown -> 0

    /// Format country calling code as string.
    let formatCountryCode (code: CountryCallingCode) : string =
        sprintf "+%d" (getCountryCodeValue code)

    /// Get country name for a calling code.
    let getCountryName (code: CountryCallingCode) : string =
        match code with
        | CC1 -> "United States/Canada"
        | CC7 -> "Russia/Kazakhstan"
        | CC20 -> "Egypt"
        | CC27 -> "South Africa"
        | CC30 -> "Greece"
        | CC31 -> "Netherlands"
        | CC32 -> "Belgium"
        | CC33 -> "France"
        | CC34 -> "Spain"
        | CC36 -> "Hungary"
        | CC39 -> "Italy"
        | CC40 -> "Romania"
        | CC41 -> "Switzerland"
        | CC43 -> "Austria"
        | CC44 -> "United Kingdom"
        | CC45 -> "Denmark"
        | CC46 -> "Sweden"
        | CC47 -> "Norway"
        | CC48 -> "Poland"
        | CC49 -> "Germany"
        | CC51 -> "Peru"
        | CC52 -> "Mexico"
        | CC53 -> "Cuba"
        | CC54 -> "Argentina"
        | CC55 -> "Brazil"
        | CC56 -> "Chile"
        | CC57 -> "Colombia"
        | CC58 -> "Venezuela"
        | CC60 -> "Malaysia"
        | CC61 -> "Australia"
        | CC62 -> "Indonesia"
        | CC63 -> "Philippines"
        | CC64 -> "New Zealand"
        | CC65 -> "Singapore"
        | CC66 -> "Thailand"
        | CC81 -> "Japan"
        | CC82 -> "South Korea"
        | CC84 -> "Vietnam"
        | CC86 -> "China"
        | CC90 -> "Turkey"
        | CC91 -> "India"
        | CC92 -> "Pakistan"
        | CC93 -> "Afghanistan"
        | CC94 -> "Sri Lanka"
        | CC95 -> "Myanmar"
        | CC98 -> "Iran"
        | CC212 -> "Morocco"
        | CC213 -> "Algeria"
        | CC216 -> "Tunisia"
        | CC218 -> "Libya"
        | CC234 -> "Nigeria"
        | CC254 -> "Kenya"
        | CC255 -> "Tanzania"
        | CC256 -> "Uganda"
        | CC351 -> "Portugal"
        | CC352 -> "Luxembourg"
        | CC353 -> "Ireland"
        | CC354 -> "Iceland"
        | CC358 -> "Finland"
        | CC380 -> "Ukraine"
        | CC381 -> "Serbia"
        | CC385 -> "Croatia"
        | CC386 -> "Slovenia"
        | CC420 -> "Czech Republic"
        | CC421 -> "Slovakia"
        | CC852 -> "Hong Kong"
        | CC853 -> "Macau"
        | CC855 -> "Cambodia"
        | CC856 -> "Laos"
        | CC880 -> "Bangladesh"
        | CC886 -> "Taiwan"
        | CC960 -> "Maldives"
        | CC961 -> "Lebanon"
        | CC962 -> "Jordan"
        | CC963 -> "Syria"
        | CC964 -> "Iraq"
        | CC965 -> "Kuwait"
        | CC966 -> "Saudi Arabia"
        | CC967 -> "Yemen"
        | CC968 -> "Oman"
        | CC971 -> "UAE"
        | CC972 -> "Israel"
        | CC973 -> "Bahrain"
        | CC974 -> "Qatar"
        | CC975 -> "Bhutan"
        | CC976 -> "Mongolia"
        | CC977 -> "Nepal"
        | CC992 -> "Tajikistan"
        | CC993 -> "Turkmenistan"
        | CC994 -> "Azerbaijan"
        | CC995 -> "Georgia"
        | CC996 -> "Kyrgyzstan"
        | CC998 -> "Uzbekistan"
        | CCUnknown -> "Unknown"

    let private isDigitChar (c: char) : bool = c >= '0' && c <= '9'

    let private extractDigits (s: string) : string =
        String(s.ToCharArray() |> Array.filter isDigitChar)

    let private parseCountryCodeFromDigits (digits: string) : (CountryCallingCode * string) option =
        let tryParseCode (len: int) =
            if digits.Length >= len then
                let prefix = digits.Substring(0, len)
                let rest = digits.Substring(len)
                match Int32.TryParse(prefix) with
                | true, n ->
                    let code =
                        match n with
                        | 1 -> Some CC1 | 7 -> Some CC7 | 20 -> Some CC20 | 27 -> Some CC27
                        | 30 -> Some CC30 | 31 -> Some CC31 | 32 -> Some CC32 | 33 -> Some CC33
                        | 34 -> Some CC34 | 36 -> Some CC36 | 39 -> Some CC39 | 40 -> Some CC40
                        | 41 -> Some CC41 | 43 -> Some CC43 | 44 -> Some CC44 | 45 -> Some CC45
                        | 46 -> Some CC46 | 47 -> Some CC47 | 48 -> Some CC48 | 49 -> Some CC49
                        | 51 -> Some CC51 | 52 -> Some CC52 | 53 -> Some CC53 | 54 -> Some CC54
                        | 55 -> Some CC55 | 56 -> Some CC56 | 57 -> Some CC57 | 58 -> Some CC58
                        | 60 -> Some CC60 | 61 -> Some CC61 | 62 -> Some CC62 | 63 -> Some CC63
                        | 64 -> Some CC64 | 65 -> Some CC65 | 66 -> Some CC66 | 81 -> Some CC81
                        | 82 -> Some CC82 | 84 -> Some CC84 | 86 -> Some CC86 | 90 -> Some CC90
                        | 91 -> Some CC91 | 92 -> Some CC92 | 93 -> Some CC93 | 94 -> Some CC94
                        | 95 -> Some CC95 | 98 -> Some CC98
                        | 212 -> Some CC212 | 213 -> Some CC213 | 216 -> Some CC216 | 218 -> Some CC218
                        | 234 -> Some CC234 | 254 -> Some CC254 | 255 -> Some CC255 | 256 -> Some CC256
                        | 351 -> Some CC351 | 352 -> Some CC352 | 353 -> Some CC353 | 354 -> Some CC354
                        | 358 -> Some CC358 | 380 -> Some CC380 | 381 -> Some CC381 | 385 -> Some CC385
                        | 386 -> Some CC386 | 420 -> Some CC420 | 421 -> Some CC421 | 852 -> Some CC852
                        | 853 -> Some CC853 | 855 -> Some CC855 | 856 -> Some CC856 | 880 -> Some CC880
                        | 886 -> Some CC886 | 960 -> Some CC960 | 961 -> Some CC961 | 962 -> Some CC962
                        | 963 -> Some CC963 | 964 -> Some CC964 | 965 -> Some CC965 | 966 -> Some CC966
                        | 967 -> Some CC967 | 968 -> Some CC968 | 971 -> Some CC971 | 972 -> Some CC972
                        | 973 -> Some CC973 | 974 -> Some CC974 | 975 -> Some CC975 | 976 -> Some CC976
                        | 977 -> Some CC977 | 992 -> Some CC992 | 993 -> Some CC993 | 994 -> Some CC994
                        | 995 -> Some CC995 | 996 -> Some CC996 | 998 -> Some CC998
                        | _ -> None
                    code |> Option.map (fun c -> (c, rest))
                | false, _ -> None
            else None

        // Try 3-digit codes first, then 2-digit, then 1-digit
        tryParseCode 3
        |> Option.orElseWith (fun () -> tryParseCode 2)
        |> Option.orElseWith (fun () -> tryParseCode 1)

    /// Parse phone number from string (E.164 format with + prefix).
    let parse (input: string) : Result<PhoneNumber, PhoneError> =
        if String.IsNullOrWhiteSpace(input) then
            Error EmptyInput
        else
            let trimmed = input.Trim()
            let startsWithPlus = trimmed.StartsWith("+")
            let rawDigits =
                if startsWithPlus then
                    extractDigits (trimmed.Substring(1))
                else
                    extractDigits trimmed
            let len = rawDigits.Length

            if len < 7 then
                Error(TooShort len)
            elif len > 15 then
                Error(TooLong len)
            else
                match parseCountryCodeFromDigits rawDigits with
                | Some(code, national) ->
                    if national.Length < 4 then
                        Error(InvalidNationalNumber national)
                    else
                        Ok { CountryCode = code; NationalNumber = national }
                | None ->
                    Error(InvalidCountryCode(rawDigits.Substring(0, min 3 rawDigits.Length)))

    /// Parse phone number, returning Option.
    let tryParse (input: string) : PhoneNumber option =
        match parse input with
        | Ok phone -> Some phone
        | Error _ -> None

    /// Check if string is a valid phone number.
    let isValid (input: string) : bool =
        (tryParse input).IsSome

    /// Format phone number in E.164 format (+CCNNNN...).
    let formatE164 (phone: PhoneNumber) : string =
        sprintf "+%d%s" (getCountryCodeValue phone.CountryCode) phone.NationalNumber

    /// Format phone number with spaces (e.g., +1 555 123 4567).
    let formatInternational (phone: PhoneNumber) : string =
        let national = phone.NationalNumber
        let len = national.Length
        let formattedNational =
            if len <= 4 then national
            elif len <= 7 then sprintf "%s %s" (national.Substring(0, 3)) (national.Substring(3))
            elif len <= 10 then sprintf "%s %s %s" (national.Substring(0, 3)) (national.Substring(3, 3)) (national.Substring(6))
            else national
        sprintf "+%d %s" (getCountryCodeValue phone.CountryCode) formattedNational

    /// Format for display (same as international).
    let formatDisplay (phone: PhoneNumber) : string =
        formatInternational phone

    /// Get the country code from a phone number.
    let getCountryCode (phone: PhoneNumber) : CountryCallingCode =
        phone.CountryCode

    /// Get the national number from a phone number.
    let getNationalNumber (phone: PhoneNumber) : string =
        phone.NationalNumber

    /// Get the full number as digits only.
    let getDigits (phone: PhoneNumber) : string =
        sprintf "%d%s" (getCountryCodeValue phone.CountryCode) phone.NationalNumber

    /// Get total length of phone number (country code + national).
    let getLength (phone: PhoneNumber) : int =
        (getDigits phone).Length

    /// Compare two phone numbers for equality.
    let equals (a: PhoneNumber) (b: PhoneNumber) : bool =
        getCountryCodeValue a.CountryCode = getCountryCodeValue b.CountryCode &&
        a.NationalNumber = b.NationalNumber

    /// Obfuscate phone number for display (e.g., +1 555 *** 4567).
    let obfuscate (phone: PhoneNumber) : string =
        let national = phone.NationalNumber
        let len = national.Length
        let obfuscated =
            if len <= 4 then "****"
            elif len <= 7 then sprintf "%s****" (national.Substring(0, 3))
            else sprintf "%s****%s" (national.Substring(0, 3)) (national.Substring(len - 4))
        sprintf "+%d %s" (getCountryCodeValue phone.CountryCode) obfuscated
