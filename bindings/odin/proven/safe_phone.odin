// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import "core:strings"
import "core:strconv"
import "core:fmt"

// ITU-T E.164 country calling codes.
// This is a subset of the most commonly used country codes.
Country_Code :: enum u16 {
    UNKNOWN = 0,

    // North America (NANP)
    US = 1,      // United States
    CA = 1001,   // Canada (same +1 but distinguished by area code)

    // Europe
    GB = 44,     // United Kingdom
    FR = 33,     // France
    DE = 49,     // Germany
    IT = 39,     // Italy
    ES = 34,     // Spain
    PT = 351,    // Portugal
    NL = 31,     // Netherlands
    BE = 32,     // Belgium
    AT = 43,     // Austria
    CH = 41,     // Switzerland
    SE = 46,     // Sweden
    NO = 47,     // Norway
    DK = 45,     // Denmark
    FI = 358,    // Finland
    IE = 353,    // Ireland
    PL = 48,     // Poland
    CZ = 420,    // Czech Republic
    HU = 36,     // Hungary
    RO = 40,     // Romania
    GR = 30,     // Greece
    UA = 380,    // Ukraine
    RU = 7,      // Russia
    TR = 90,     // Turkey

    // Asia
    CN = 86,     // China
    JP = 81,     // Japan
    KR = 82,     // South Korea
    IN = 91,     // India
    ID = 62,     // Indonesia
    MY = 60,     // Malaysia
    SG = 65,     // Singapore
    TH = 66,     // Thailand
    VN = 84,     // Vietnam
    PH = 63,     // Philippines
    PK = 92,     // Pakistan
    BD = 880,    // Bangladesh
    HK = 852,    // Hong Kong
    TW = 886,    // Taiwan

    // Middle East
    IL = 972,    // Israel
    SA = 966,    // Saudi Arabia
    AE = 971,    // United Arab Emirates
    EG = 20,     // Egypt

    // Africa
    ZA = 27,     // South Africa
    NG = 234,    // Nigeria
    KE = 254,    // Kenya
    MA = 212,    // Morocco

    // Oceania
    AU = 61,     // Australia
    NZ = 64,     // New Zealand

    // South America
    BR = 55,     // Brazil
    AR = 54,     // Argentina
    MX = 52,     // Mexico
    CO = 57,     // Colombia
    CL = 56,     // Chile
    PE = 51,     // Peru
    VE = 58,     // Venezuela
}

// Phone number type classification.
Phone_Type :: enum {
    Unknown,
    Mobile,
    Fixed_Line,
    Toll_Free,
    Premium_Rate,
    Shared_Cost,
    VOIP,
    Personal_Number,
    Pager,
    Emergency,
}

// Phone number structure.
Phone_Number :: struct {
    country_code:   Country_Code, // Country calling code
    national_number: string,      // National significant number (digits only)
    extension:      string,       // Optional extension
    raw_input:      string,       // Original input for reference
}

// Get the numeric calling code for a country.
country_calling_code :: proc(country: Country_Code) -> int {
    switch country {
    case .US: return 1
    case .CA: return 1  // Same as US
    case .GB: return 44
    case .FR: return 33
    case .DE: return 49
    case .IT: return 39
    case .ES: return 34
    case .PT: return 351
    case .NL: return 31
    case .BE: return 32
    case .AT: return 43
    case .CH: return 41
    case .SE: return 46
    case .NO: return 47
    case .DK: return 45
    case .FI: return 358
    case .IE: return 353
    case .PL: return 48
    case .CZ: return 420
    case .HU: return 36
    case .RO: return 40
    case .GR: return 30
    case .UA: return 380
    case .RU: return 7
    case .TR: return 90
    case .CN: return 86
    case .JP: return 81
    case .KR: return 82
    case .IN: return 91
    case .ID: return 62
    case .MY: return 60
    case .SG: return 65
    case .TH: return 66
    case .VN: return 84
    case .PH: return 63
    case .PK: return 92
    case .BD: return 880
    case .HK: return 852
    case .TW: return 886
    case .IL: return 972
    case .SA: return 966
    case .AE: return 971
    case .EG: return 20
    case .ZA: return 27
    case .NG: return 234
    case .KE: return 254
    case .MA: return 212
    case .AU: return 61
    case .NZ: return 64
    case .BR: return 55
    case .AR: return 54
    case .MX: return 52
    case .CO: return 57
    case .CL: return 56
    case .PE: return 51
    case .VE: return 58
    case: return 0
    }
}

// Get the ISO 3166-1 alpha-2 code for a country.
country_iso_code :: proc(country: Country_Code) -> string {
    switch country {
    case .US: return "US"
    case .CA: return "CA"
    case .GB: return "GB"
    case .FR: return "FR"
    case .DE: return "DE"
    case .IT: return "IT"
    case .ES: return "ES"
    case .PT: return "PT"
    case .NL: return "NL"
    case .BE: return "BE"
    case .AT: return "AT"
    case .CH: return "CH"
    case .SE: return "SE"
    case .NO: return "NO"
    case .DK: return "DK"
    case .FI: return "FI"
    case .IE: return "IE"
    case .PL: return "PL"
    case .CZ: return "CZ"
    case .HU: return "HU"
    case .RO: return "RO"
    case .GR: return "GR"
    case .UA: return "UA"
    case .RU: return "RU"
    case .TR: return "TR"
    case .CN: return "CN"
    case .JP: return "JP"
    case .KR: return "KR"
    case .IN: return "IN"
    case .ID: return "ID"
    case .MY: return "MY"
    case .SG: return "SG"
    case .TH: return "TH"
    case .VN: return "VN"
    case .PH: return "PH"
    case .PK: return "PK"
    case .BD: return "BD"
    case .HK: return "HK"
    case .TW: return "TW"
    case .IL: return "IL"
    case .SA: return "SA"
    case .AE: return "AE"
    case .EG: return "EG"
    case .ZA: return "ZA"
    case .NG: return "NG"
    case .KE: return "KE"
    case .MA: return "MA"
    case .AU: return "AU"
    case .NZ: return "NZ"
    case .BR: return "BR"
    case .AR: return "AR"
    case .MX: return "MX"
    case .CO: return "CO"
    case .CL: return "CL"
    case .PE: return "PE"
    case .VE: return "VE"
    case: return "??"
    }
}

// Parse country code from ISO 3166-1 alpha-2 code.
parse_country_code :: proc(iso_code: string) -> (country: Country_Code, ok: bool) {
    upper := strings.to_upper(iso_code)
    switch upper {
    case "US": return .US, true
    case "CA": return .CA, true
    case "GB": return .GB, true
    case "FR": return .FR, true
    case "DE": return .DE, true
    case "IT": return .IT, true
    case "ES": return .ES, true
    case "PT": return .PT, true
    case "NL": return .NL, true
    case "BE": return .BE, true
    case "AT": return .AT, true
    case "CH": return .CH, true
    case "SE": return .SE, true
    case "NO": return .NO, true
    case "DK": return .DK, true
    case "FI": return .FI, true
    case "IE": return .IE, true
    case "PL": return .PL, true
    case "CZ": return .CZ, true
    case "HU": return .HU, true
    case "RO": return .RO, true
    case "GR": return .GR, true
    case "UA": return .UA, true
    case "RU": return .RU, true
    case "TR": return .TR, true
    case "CN": return .CN, true
    case "JP": return .JP, true
    case "KR": return .KR, true
    case "IN": return .IN, true
    case "ID": return .ID, true
    case "MY": return .MY, true
    case "SG": return .SG, true
    case "TH": return .TH, true
    case "VN": return .VN, true
    case "PH": return .PH, true
    case "PK": return .PK, true
    case "BD": return .BD, true
    case "HK": return .HK, true
    case "TW": return .TW, true
    case "IL": return .IL, true
    case "SA": return .SA, true
    case "AE": return .AE, true
    case "EG": return .EG, true
    case "ZA": return .ZA, true
    case "NG": return .NG, true
    case "KE": return .KE, true
    case "MA": return .MA, true
    case "AU": return .AU, true
    case "NZ": return .NZ, true
    case "BR": return .BR, true
    case "AR": return .AR, true
    case "MX": return .MX, true
    case "CO": return .CO, true
    case "CL": return .CL, true
    case "PE": return .PE, true
    case "VE": return .VE, true
    case: return .UNKNOWN, false
    }
}

// Extract digits only from a string.
@(private)
extract_digits :: proc(input: string, allocator := context.allocator) -> string {
    builder := strings.builder_make(allocator)
    for c in input {
        if c >= '0' && c <= '9' {
            strings.write_rune(&builder, c)
        }
    }
    return strings.to_string(builder)
}

// Parse a phone number string.
// Accepts formats like: +1-555-123-4567, (555) 123-4567, +44 20 7946 0958
parse_phone :: proc(input: string, default_country := Country_Code.US, allocator := context.allocator) -> (phone: Phone_Number, ok: bool) {
    s := strings.trim_space(input)
    if len(s) == 0 {
        return {}, false
    }

    // Extract extension if present
    extension := ""
    ext_markers := []string{"ext", "ext.", "x", "extension"}
    lower_s := strings.to_lower(s)
    for marker in ext_markers {
        idx := strings.index(lower_s, marker)
        if idx >= 0 {
            ext_start := idx + len(marker)
            extension = extract_digits(s[ext_start:], allocator)
            s = strings.trim_space(s[:idx])
            break
        }
    }

    // Check for international format
    country := default_country
    has_plus := len(s) > 0 && s[0] == '+'

    digits := extract_digits(s, allocator)
    if len(digits) == 0 {
        return {}, false
    }

    national_number := digits

    if has_plus {
        // Try to parse country code from digits
        // Check 1-digit codes first (1, 7), then 2-digit, then 3-digit
        parsed_country, remaining, cc_ok := try_parse_country_code(digits)
        if cc_ok {
            country = parsed_country
            national_number = remaining
        }
    } else if len(digits) == 10 && (default_country == .US || default_country == .CA) {
        // NANP number without country code
        national_number = digits
    } else if len(digits) == 11 && digits[0] == '1' && (default_country == .US || default_country == .CA) {
        // NANP number with leading 1
        national_number = digits[1:]
    }

    // Basic validation: national number should have reasonable length
    if len(national_number) < 4 || len(national_number) > 15 {
        return {}, false
    }

    return Phone_Number{
        country_code = country,
        national_number = national_number,
        extension = extension,
        raw_input = input,
    }, true
}

// Try to parse country calling code from the beginning of digits.
@(private)
try_parse_country_code :: proc(digits: string) -> (country: Country_Code, remaining: string, ok: bool) {
    if len(digits) < 2 {
        return .UNKNOWN, digits, false
    }

    // 1-digit country codes
    if digits[0] == '1' {
        return .US, digits[1:], true  // Could be US or CA
    }
    if digits[0] == '7' {
        return .RU, digits[1:], true
    }

    // 2-digit country codes
    if len(digits) >= 2 {
        code2, _ := strconv.parse_int(digits[:2])
        switch code2 {
        case 20: return .EG, digits[2:], true
        case 27: return .ZA, digits[2:], true
        case 30: return .GR, digits[2:], true
        case 31: return .NL, digits[2:], true
        case 32: return .BE, digits[2:], true
        case 33: return .FR, digits[2:], true
        case 34: return .ES, digits[2:], true
        case 36: return .HU, digits[2:], true
        case 39: return .IT, digits[2:], true
        case 40: return .RO, digits[2:], true
        case 41: return .CH, digits[2:], true
        case 43: return .AT, digits[2:], true
        case 44: return .GB, digits[2:], true
        case 45: return .DK, digits[2:], true
        case 46: return .SE, digits[2:], true
        case 47: return .NO, digits[2:], true
        case 48: return .PL, digits[2:], true
        case 49: return .DE, digits[2:], true
        case 51: return .PE, digits[2:], true
        case 52: return .MX, digits[2:], true
        case 54: return .AR, digits[2:], true
        case 55: return .BR, digits[2:], true
        case 56: return .CL, digits[2:], true
        case 57: return .CO, digits[2:], true
        case 58: return .VE, digits[2:], true
        case 60: return .MY, digits[2:], true
        case 61: return .AU, digits[2:], true
        case 62: return .ID, digits[2:], true
        case 63: return .PH, digits[2:], true
        case 64: return .NZ, digits[2:], true
        case 65: return .SG, digits[2:], true
        case 66: return .TH, digits[2:], true
        case 81: return .JP, digits[2:], true
        case 82: return .KR, digits[2:], true
        case 84: return .VN, digits[2:], true
        case 86: return .CN, digits[2:], true
        case 90: return .TR, digits[2:], true
        case 91: return .IN, digits[2:], true
        case 92: return .PK, digits[2:], true
        }
    }

    // 3-digit country codes
    if len(digits) >= 3 {
        code3, _ := strconv.parse_int(digits[:3])
        switch code3 {
        case 212: return .MA, digits[3:], true
        case 234: return .NG, digits[3:], true
        case 254: return .KE, digits[3:], true
        case 351: return .PT, digits[3:], true
        case 353: return .IE, digits[3:], true
        case 358: return .FI, digits[3:], true
        case 380: return .UA, digits[3:], true
        case 420: return .CZ, digits[3:], true
        case 852: return .HK, digits[3:], true
        case 880: return .BD, digits[3:], true
        case 886: return .TW, digits[3:], true
        case 966: return .SA, digits[3:], true
        case 971: return .AE, digits[3:], true
        case 972: return .IL, digits[3:], true
        }
    }

    return .UNKNOWN, digits, false
}

// Format phone number in E.164 format (+15551234567).
format_phone_e164 :: proc(phone: Phone_Number, allocator := context.allocator) -> string {
    calling_code := country_calling_code(phone.country_code)
    return fmt.aprintf("+%d%s", calling_code, phone.national_number)
}

// Format phone number in international format (+1 555-123-4567).
format_phone_international :: proc(phone: Phone_Number, allocator := context.allocator) -> string {
    calling_code := country_calling_code(phone.country_code)
    formatted_national := format_national_number(phone.national_number, phone.country_code, allocator)

    if len(phone.extension) > 0 {
        return fmt.aprintf("+%d %s ext. %s", calling_code, formatted_national, phone.extension)
    }
    return fmt.aprintf("+%d %s", calling_code, formatted_national)
}

// Format phone number in national format ((555) 123-4567 for US).
format_phone_national :: proc(phone: Phone_Number, allocator := context.allocator) -> string {
    formatted := format_national_number(phone.national_number, phone.country_code, allocator)

    if len(phone.extension) > 0 {
        return fmt.aprintf("%s ext. %s", formatted, phone.extension)
    }
    return formatted
}

// Format the national significant number according to country conventions.
@(private)
format_national_number :: proc(digits: string, country: Country_Code, allocator := context.allocator) -> string {
    // NANP format: (XXX) XXX-XXXX
    if (country == .US || country == .CA) && len(digits) == 10 {
        return fmt.aprintf("(%s) %s-%s", digits[0:3], digits[3:6], digits[6:10])
    }

    // UK format: XXXX XXX XXXX or similar
    if country == .GB && len(digits) >= 10 {
        if digits[0] == '7' { // Mobile
            return fmt.aprintf("%s %s %s", digits[0:4], digits[4:7], digits[7:])
        }
        if digits[0] == '2' { // London
            return fmt.aprintf("%s %s %s", digits[0:2], digits[2:6], digits[6:])
        }
    }

    // Japan format: XX-XXXX-XXXX
    if country == .JP && len(digits) >= 10 {
        return fmt.aprintf("%s-%s-%s", digits[0:2], digits[2:6], digits[6:])
    }

    // Default: group in 3s
    builder := strings.builder_make(allocator)
    for i := 0; i < len(digits); i += 1 {
        if i > 0 && i % 3 == 0 {
            strings.write_byte(&builder, ' ')
        }
        strings.write_byte(&builder, digits[i])
    }
    return strings.to_string(builder)
}

// Validate phone number length for country.
is_valid_phone_length :: proc(phone: Phone_Number) -> bool {
    len_nn := len(phone.national_number)

    switch phone.country_code {
    case .US, .CA:
        return len_nn == 10
    case .GB:
        return len_nn >= 10 && len_nn <= 11
    case .DE, .FR, .IT, .ES:
        return len_nn >= 9 && len_nn <= 11
    case .JP:
        return len_nn >= 9 && len_nn <= 10
    case .CN:
        return len_nn == 11
    case .AU:
        return len_nn == 9
    case .IN:
        return len_nn == 10
    case:
        // E.164 allows up to 15 digits total
        return len_nn >= 4 && len_nn <= 14
    }
}

// Check if phone number appears to be mobile.
is_mobile_phone :: proc(phone: Phone_Number) -> bool {
    if len(phone.national_number) == 0 {
        return false
    }

    first := phone.national_number[0]

    switch phone.country_code {
    case .US, .CA:
        // In NANP, can't reliably distinguish mobile from fixed
        return false
    case .GB:
        return first == '7'
    case .DE:
        return first == '1' && len(phone.national_number) >= 2 &&
               (phone.national_number[1] == '5' || phone.national_number[1] == '6' || phone.national_number[1] == '7')
    case .FR:
        return first == '6' || first == '7'
    case .AU:
        return first == '4'
    case .JP:
        return first == '7' || first == '8' || first == '9'
    case .CN:
        return first == '1'
    case .IN:
        return first == '6' || first == '7' || first == '8' || first == '9'
    case:
        return false
    }
}

// Compare two phone numbers (ignores formatting).
phones_equal :: proc(a, b: Phone_Number) -> bool {
    if a.country_code != b.country_code {
        return false
    }
    return a.national_number == b.national_number && a.extension == b.extension
}

// Mask phone number for display (e.g., +1 ***-***-4567).
mask_phone :: proc(phone: Phone_Number, allocator := context.allocator) -> string {
    calling_code := country_calling_code(phone.country_code)
    len_nn := len(phone.national_number)

    if len_nn <= 4 {
        return fmt.aprintf("+%d ****", calling_code)
    }

    visible := phone.national_number[len_nn - 4:]
    masked_len := len_nn - 4

    builder := strings.builder_make(allocator)
    fmt.sbprintf(&builder, "+%d ", calling_code)

    for _ in 0..<masked_len {
        strings.write_byte(&builder, '*')
    }
    strings.write_string(&builder, visible)

    return strings.to_string(builder)
}

// Get phone number as digits only (no country code).
phone_digits :: proc(phone: Phone_Number) -> string {
    return phone.national_number
}

// Get full phone number as digits (with country code).
phone_full_digits :: proc(phone: Phone_Number, allocator := context.allocator) -> string {
    calling_code := country_calling_code(phone.country_code)
    return fmt.aprintf("%d%s", calling_code, phone.national_number)
}
