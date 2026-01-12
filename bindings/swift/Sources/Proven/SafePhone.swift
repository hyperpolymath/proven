// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Safe phone number validation and formatting following ITU-T E.164.
public enum SafePhone {
    // MARK: - Phone Number Type

    /// Phone number type classification.
    public enum PhoneNumberType: Equatable, Hashable, CustomStringConvertible {
        case mobile
        case fixedLine
        case tollFree
        case premiumRate
        case sharedCost
        case voip
        case personalNumber
        case pager
        case uan  // Universal Access Number
        case unknown

        public var description: String {
            switch self {
            case .mobile: return "Mobile"
            case .fixedLine: return "Fixed Line"
            case .tollFree: return "Toll Free"
            case .premiumRate: return "Premium Rate"
            case .sharedCost: return "Shared Cost"
            case .voip: return "VoIP"
            case .personalNumber: return "Personal Number"
            case .pager: return "Pager"
            case .uan: return "UAN"
            case .unknown: return "Unknown"
            }
        }
    }

    // MARK: - Country Calling Code

    /// ITU country calling codes (common ones).
    public enum CountryCode: Int, Equatable, Hashable, CaseIterable, CustomStringConvertible {
        case cc1 = 1       // USA, Canada, Caribbean
        case cc7 = 7       // Russia, Kazakhstan
        case cc20 = 20     // Egypt
        case cc27 = 27     // South Africa
        case cc30 = 30     // Greece
        case cc31 = 31     // Netherlands
        case cc32 = 32     // Belgium
        case cc33 = 33     // France
        case cc34 = 34     // Spain
        case cc36 = 36     // Hungary
        case cc39 = 39     // Italy
        case cc40 = 40     // Romania
        case cc41 = 41     // Switzerland
        case cc43 = 43     // Austria
        case cc44 = 44     // UK
        case cc45 = 45     // Denmark
        case cc46 = 46     // Sweden
        case cc47 = 47     // Norway
        case cc48 = 48     // Poland
        case cc49 = 49     // Germany
        case cc51 = 51     // Peru
        case cc52 = 52     // Mexico
        case cc53 = 53     // Cuba
        case cc54 = 54     // Argentina
        case cc55 = 55     // Brazil
        case cc56 = 56     // Chile
        case cc57 = 57     // Colombia
        case cc58 = 58     // Venezuela
        case cc60 = 60     // Malaysia
        case cc61 = 61     // Australia
        case cc62 = 62     // Indonesia
        case cc63 = 63     // Philippines
        case cc64 = 64     // New Zealand
        case cc65 = 65     // Singapore
        case cc66 = 66     // Thailand
        case cc81 = 81     // Japan
        case cc82 = 82     // South Korea
        case cc84 = 84     // Vietnam
        case cc86 = 86     // China
        case cc90 = 90     // Turkey
        case cc91 = 91     // India
        case cc92 = 92     // Pakistan
        case cc93 = 93     // Afghanistan
        case cc94 = 94     // Sri Lanka
        case cc95 = 95     // Myanmar
        case cc98 = 98     // Iran
        case cc212 = 212   // Morocco
        case cc213 = 213   // Algeria
        case cc216 = 216   // Tunisia
        case cc218 = 218   // Libya
        case cc220 = 220   // Gambia
        case cc221 = 221   // Senegal
        case cc234 = 234   // Nigeria
        case cc254 = 254   // Kenya
        case cc255 = 255   // Tanzania
        case cc256 = 256   // Uganda
        case cc260 = 260   // Zambia
        case cc263 = 263   // Zimbabwe
        case cc351 = 351   // Portugal
        case cc352 = 352   // Luxembourg
        case cc353 = 353   // Ireland
        case cc354 = 354   // Iceland
        case cc358 = 358   // Finland
        case cc380 = 380   // Ukraine
        case cc381 = 381   // Serbia
        case cc385 = 385   // Croatia
        case cc386 = 386   // Slovenia
        case cc420 = 420   // Czech Republic
        case cc421 = 421   // Slovakia
        case cc852 = 852   // Hong Kong
        case cc853 = 853   // Macau
        case cc855 = 855   // Cambodia
        case cc856 = 856   // Laos
        case cc880 = 880   // Bangladesh
        case cc886 = 886   // Taiwan
        case cc960 = 960   // Maldives
        case cc961 = 961   // Lebanon
        case cc962 = 962   // Jordan
        case cc963 = 963   // Syria
        case cc964 = 964   // Iraq
        case cc965 = 965   // Kuwait
        case cc966 = 966   // Saudi Arabia
        case cc967 = 967   // Yemen
        case cc968 = 968   // Oman
        case cc971 = 971   // UAE
        case cc972 = 972   // Israel
        case cc973 = 973   // Bahrain
        case cc974 = 974   // Qatar
        case cc975 = 975   // Bhutan
        case cc976 = 976   // Mongolia
        case cc977 = 977   // Nepal
        case cc992 = 992   // Tajikistan
        case cc993 = 993   // Turkmenistan
        case cc994 = 994   // Azerbaijan
        case cc995 = 995   // Georgia
        case cc996 = 996   // Kyrgyzstan
        case cc998 = 998   // Uzbekistan

        public var description: String {
            "+\(rawValue)"
        }

        /// Get the numeric value of the country code.
        public var value: Int {
            rawValue
        }

        /// Get the number of digits in this country code.
        public var digitCount: Int {
            if rawValue >= 100 { return 3 }
            if rawValue >= 10 { return 2 }
            return 1
        }
    }

    // MARK: - Phone Errors

    /// Phone number parsing and validation errors.
    public enum PhoneError: Error, Equatable, CustomStringConvertible {
        case invalidCharacter(Character)
        case tooShort(Int)
        case tooLong(Int)
        case invalidCountryCode(String)
        case invalidNationalNumber(String)
        case emptyInput

        public var description: String {
            switch self {
            case .invalidCharacter(let char):
                return "Invalid character: \(char)"
            case .tooShort(let length):
                return "Phone number too short: \(length) digits"
            case .tooLong(let length):
                return "Phone number too long: \(length) digits"
            case .invalidCountryCode(let code):
                return "Invalid country code: \(code)"
            case .invalidNationalNumber(let number):
                return "Invalid national number: \(number)"
            case .emptyInput:
                return "Empty input"
            }
        }
    }

    // MARK: - Phone Number Structure

    /// Validated phone number in E.164 format.
    public struct PhoneNumber: Equatable, Hashable, CustomStringConvertible {
        /// The country calling code.
        public let countryCode: CountryCode

        /// The national number (digits only).
        public let nationalNumber: String

        /// Create a phone number from country code and national number.
        public init(countryCode: CountryCode, nationalNumber: String) {
            self.countryCode = countryCode
            self.nationalNumber = nationalNumber
        }

        public var description: String {
            formatE164()
        }

        /// Get the full number as digits only.
        public var digits: String {
            "\(countryCode.value)\(nationalNumber)"
        }

        /// Get total length of phone number (country code + national).
        public var length: Int {
            digits.count
        }
    }

    // MARK: - Phone Parsing

    /// Check if character is a digit.
    private static func isDigitChar(_ char: Character) -> Bool {
        char >= "0" && char <= "9"
    }

    /// Extract only digits from string.
    private static func extractDigits(_ string: String) -> String {
        String(string.filter { isDigitChar($0) })
    }

    /// Parse country code from digit string.
    private static func parseCountryCode(_ digits: String) -> (CountryCode, String)? {
        // Try 3-digit codes first, then 2-digit, then 1-digit
        for length in [3, 2, 1] {
            guard digits.count >= length else { continue }
            let prefix = String(digits.prefix(length))
            if let codeValue = Int(prefix),
               let countryCode = CountryCode(rawValue: codeValue) {
                let rest = String(digits.dropFirst(length))
                return (countryCode, rest)
            }
        }
        return nil
    }

    /// Parse phone number from string (E.164 format with +).
    public static func parse(_ input: String) -> Result<PhoneNumber, PhoneError> {
        let trimmed = input.trimmingCharacters(in: .whitespacesAndNewlines)

        guard !trimmed.isEmpty else {
            return .failure(.emptyInput)
        }

        let digits: String
        if trimmed.hasPrefix("+") {
            digits = extractDigits(String(trimmed.dropFirst()))
        } else {
            digits = extractDigits(trimmed)
        }

        guard digits.count >= 7 else {
            return .failure(.tooShort(digits.count))
        }

        guard digits.count <= 15 else {
            return .failure(.tooLong(digits.count))
        }

        guard let (countryCode, nationalNumber) = parseCountryCode(digits) else {
            let codePrefix = String(digits.prefix(3))
            return .failure(.invalidCountryCode(codePrefix))
        }

        guard nationalNumber.count >= 4 else {
            return .failure(.invalidNationalNumber(nationalNumber))
        }

        return .success(PhoneNumber(countryCode: countryCode, nationalNumber: nationalNumber))
    }

    /// Parse phone number, returning nil on failure.
    public static func parse(_ input: String) -> PhoneNumber? {
        switch parse(input) as Result<PhoneNumber, PhoneError> {
        case .success(let phone): return phone
        case .failure: return nil
        }
    }

    // MARK: - Phone Formatting

    /// Format phone number in E.164 format (+CCNNNN...).
    public static func formatE164(_ phone: PhoneNumber) -> String {
        "+\(phone.countryCode.value)\(phone.nationalNumber)"
    }

    /// Format phone number with spaces (e.g., +1 555 123 4567).
    public static func formatInternational(_ phone: PhoneNumber) -> String {
        let national = phone.nationalNumber
        let formatted = formatNational(national)
        return "+\(phone.countryCode.value) \(formatted)"
    }

    /// Format national number with spaces.
    private static func formatNational(_ national: String) -> String {
        let length = national.count
        if length <= 4 {
            return national
        } else if length <= 7 {
            let index = national.index(national.startIndex, offsetBy: 3)
            return "\(national[..<index]) \(national[index...])"
        } else if length <= 10 {
            let index1 = national.index(national.startIndex, offsetBy: 3)
            let index2 = national.index(national.startIndex, offsetBy: 6)
            return "\(national[..<index1]) \(national[index1..<index2]) \(national[index2...])"
        } else {
            return national
        }
    }

    /// Format for display (simplified international format).
    public static func formatDisplay(_ phone: PhoneNumber) -> String {
        formatInternational(phone)
    }

    // MARK: - Phone Validation

    /// Check if string is valid phone number.
    public static func isValid(_ input: String) -> Bool {
        switch parse(input) as Result<PhoneNumber, PhoneError> {
        case .success: return true
        case .failure: return false
        }
    }

    /// Get the country code from a phone number.
    public static func getCountryCode(_ phone: PhoneNumber) -> CountryCode {
        phone.countryCode
    }

    /// Get the national number from a phone number.
    public static func getNationalNumber(_ phone: PhoneNumber) -> String {
        phone.nationalNumber
    }

    /// Get the full number as digits only.
    public static func getDigits(_ phone: PhoneNumber) -> String {
        phone.digits
    }

    /// Get total length of phone number.
    public static func phoneLength(_ phone: PhoneNumber) -> Int {
        phone.length
    }
}

// MARK: - PhoneNumber Extension Methods

extension SafePhone.PhoneNumber {
    /// Format this phone number in E.164 format.
    public func formatE164() -> String {
        SafePhone.formatE164(self)
    }

    /// Format this phone number with spaces.
    public func formatInternational() -> String {
        SafePhone.formatInternational(self)
    }

    /// Format this phone number for display.
    public func formatDisplay() -> String {
        SafePhone.formatDisplay(self)
    }
}
