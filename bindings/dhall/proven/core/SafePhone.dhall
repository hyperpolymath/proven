-- SPDX-License-Identifier: PMPL-1.0
-- SPDX-FileCopyrightText: 2025 Hyperpolymath

{-
SafePhone - Safe phone number handling

Provides E.164 compliant phone number types and validation markers.
All phone numbers are stored in international format.

Part of the Proven safety primitives library.
-}

let Prelude = https://prelude.dhall-lang.org/v23.0.0/package.dhall
    sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

let Natural/lessThan = Prelude.Natural.lessThan
let Natural/greaterThan = Prelude.Natural.greaterThan

-- Country calling codes
let CountryCode = {
    code : Natural,
    name : Text,
    iso2 : Text
}

-- Common country codes
let CountryCodes = {
    us = { code = 1, name = "United States", iso2 = "US" },
    ca = { code = 1, name = "Canada", iso2 = "CA" },
    uk = { code = 44, name = "United Kingdom", iso2 = "GB" },
    de = { code = 49, name = "Germany", iso2 = "DE" },
    fr = { code = 33, name = "France", iso2 = "FR" },
    jp = { code = 81, name = "Japan", iso2 = "JP" },
    cn = { code = 86, name = "China", iso2 = "CN" },
    in_ = { code = 91, name = "India", iso2 = "IN" },
    au = { code = 61, name = "Australia", iso2 = "AU" },
    br = { code = 55, name = "Brazil", iso2 = "BR" },
    mx = { code = 52, name = "Mexico", iso2 = "MX" },
    kr = { code = 82, name = "South Korea", iso2 = "KR" },
    ru = { code = 7, name = "Russia", iso2 = "RU" },
    it = { code = 39, name = "Italy", iso2 = "IT" },
    es = { code = 34, name = "Spain", iso2 = "ES" },
    nl = { code = 31, name = "Netherlands", iso2 = "NL" },
    se = { code = 46, name = "Sweden", iso2 = "SE" },
    ch = { code = 41, name = "Switzerland", iso2 = "CH" },
    sg = { code = 65, name = "Singapore", iso2 = "SG" },
    hk = { code = 852, name = "Hong Kong", iso2 = "HK" }
}

-- E.164 phone number (international format)
let PhoneNumber = {
    countryCode : Natural,
    nationalNumber : Text,
    extension : Optional Text
}

-- Phone result
let PhoneResult = { value : PhoneNumber, ok : Bool }

-- Create success result
let ok
    : PhoneNumber -> PhoneResult
    = \(p : PhoneNumber) -> { value = p, ok = True }

-- Create error result
let err
    : PhoneResult
    = { value = { countryCode = 0, nationalNumber = "", extension = None Text }, ok = False }

-- Create phone number
let mkPhoneNumber
    : Natural -> Text -> PhoneNumber
    = \(cc : Natural) -> \(nn : Text) ->
        { countryCode = cc, nationalNumber = nn, extension = None Text }

-- Create phone number with extension
let mkPhoneNumberWithExt
    : Natural -> Text -> Text -> PhoneNumber
    = \(cc : Natural) -> \(nn : Text) -> \(ext : Text) ->
        { countryCode = cc, nationalNumber = nn, extension = Some ext }

-- Format as E.164 (e.g., +14155551234)
let formatE164
    : PhoneNumber -> Text
    = \(p : PhoneNumber) ->
        "+" ++ Natural/show p.countryCode ++ p.nationalNumber

-- Format for display (basic - e.g., +1 415 555 1234)
let formatDisplay
    : PhoneNumber -> Text
    = \(p : PhoneNumber) ->
        let ext = merge { None = "", Some = \(e : Text) -> " ext. " ++ e } p.extension
        in "+" ++ Natural/show p.countryCode ++ " " ++ p.nationalNumber ++ ext

-- Format as tel: URI
let formatTelURI
    : PhoneNumber -> Text
    = \(p : PhoneNumber) ->
        let ext = merge { None = "", Some = \(e : Text) -> ";ext=" ++ e } p.extension
        in "tel:+" ++ Natural/show p.countryCode ++ p.nationalNumber ++ ext

-- Phone number type
let PhoneType = < Mobile | Landline | Toll_free | Premium | VoIP | Unknown >

-- Phone number with type
let TypedPhoneNumber = {
    number : PhoneNumber,
    phoneType : PhoneType,
    verified : Bool
}

-- Create typed phone number
let mkTypedPhoneNumber
    : PhoneNumber -> PhoneType -> TypedPhoneNumber
    = \(num : PhoneNumber) -> \(t : PhoneType) ->
        { number = num, phoneType = t, verified = False }

-- Mark phone as verified
let markVerified
    : TypedPhoneNumber -> TypedPhoneNumber
    = \(p : TypedPhoneNumber) ->
        { number = p.number, phoneType = p.phoneType, verified = True }

-- SMS capable phone
let SMSCapable = { number : PhoneNumber }

-- Create SMS-capable phone marker
let mkSMSCapable
    : PhoneNumber -> SMSCapable
    = \(p : PhoneNumber) ->
        { number = p }

-- Phone validation result
let ValidationResult = {
    valid : Bool,
    countryCode : Optional Natural,
    phoneType : Optional PhoneType,
    carrier : Optional Text
}

-- Default validation (unknown)
let unknownValidation
    : ValidationResult
    = {
        valid = False,
        countryCode = None Natural,
        phoneType = None PhoneType,
        carrier = None Text
    }

-- Phone region configuration
let PhoneRegion = {
    countryCode : CountryCode,
    nationalPrefix : Text,
    internationalPrefix : Text,
    trunkPrefix : Optional Text
}

-- Create phone region
let mkPhoneRegion
    : CountryCode -> Text -> Text -> PhoneRegion
    = \(cc : CountryCode) -> \(natPfx : Text) -> \(intPfx : Text) ->
        { countryCode = cc, nationalPrefix = natPfx, internationalPrefix = intPfx, trunkPrefix = None Text }

-- Emergency numbers
let EmergencyNumbers = {
    us = "911",
    eu = "112",
    uk = "999",
    au = "000",
    jp = "110"
}

-- Check if number looks like emergency
let isEmergencyLike
    : Text -> Bool
    = \(n : Text) ->
        -- Cannot do complex checks in Dhall
        False

-- Phone contact
let PhoneContact = {
    name : Text,
    phone : PhoneNumber,
    label : Optional Text
}

-- Create phone contact
let mkPhoneContact
    : Text -> PhoneNumber -> PhoneContact
    = \(name : Text) -> \(phone : PhoneNumber) ->
        { name = name, phone = phone, label = None Text }

-- Phone list
let PhoneList = { phones : List PhoneNumber }

-- Create phone list
let mkPhoneList
    : List PhoneNumber -> PhoneList
    = \(phones : List PhoneNumber) ->
        { phones = phones }

in {
    -- Types
    CountryCode,
    PhoneNumber,
    PhoneResult,
    PhoneType,
    TypedPhoneNumber,
    SMSCapable,
    ValidationResult,
    PhoneRegion,
    PhoneContact,
    PhoneList,

    -- Constructors
    ok,
    err,
    mkPhoneNumber,
    mkPhoneNumberWithExt,
    mkTypedPhoneNumber,
    markVerified,
    mkSMSCapable,
    mkPhoneRegion,
    mkPhoneContact,
    mkPhoneList,

    -- Formatting
    formatE164,
    formatDisplay,
    formatTelURI,

    -- Utilities
    isEmergencyLike,
    unknownValidation,

    -- Constants
    CountryCodes,
    EmergencyNumbers
}
