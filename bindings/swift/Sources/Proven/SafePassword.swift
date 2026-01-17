// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Password strength level.
public enum PasswordStrength: Int, Comparable {
    case veryWeak = 0
    case weak = 1
    case fair = 2
    case strong = 3
    case veryStrong = 4

    public static func < (lhs: PasswordStrength, rhs: PasswordStrength) -> Bool {
        lhs.rawValue < rhs.rawValue
    }

    public var description: String {
        switch self {
        case .veryWeak: return "Very Weak"
        case .weak: return "Weak"
        case .fair: return "Fair"
        case .strong: return "Strong"
        case .veryStrong: return "Very Strong"
        }
    }
}

/// Password validation result.
public struct PasswordValidation {
    public let isValid: Bool
    public let strength: PasswordStrength
    public let score: Int
    public let feedback: [String]
    public let entropy: Double
}

/// Password policy configuration.
public struct PasswordPolicy {
    public let minLength: Int
    public let maxLength: Int
    public let requireUppercase: Bool
    public let requireLowercase: Bool
    public let requireNumbers: Bool
    public let requireSpecial: Bool
    public let minStrength: PasswordStrength
    public let disallowCommon: Bool
    public let maxRepeating: Int

    public init(
        minLength: Int = 8,
        maxLength: Int = 128,
        requireUppercase: Bool = true,
        requireLowercase: Bool = true,
        requireNumbers: Bool = true,
        requireSpecial: Bool = false,
        minStrength: PasswordStrength = .fair,
        disallowCommon: Bool = true,
        maxRepeating: Int = 3
    ) {
        self.minLength = max(1, minLength)
        self.maxLength = max(minLength, maxLength)
        self.requireUppercase = requireUppercase
        self.requireLowercase = requireLowercase
        self.requireNumbers = requireNumbers
        self.requireSpecial = requireSpecial
        self.minStrength = minStrength
        self.disallowCommon = disallowCommon
        self.maxRepeating = max(1, maxRepeating)
    }

    /// Default policy.
    public static let `default` = PasswordPolicy()

    /// Strict policy.
    public static let strict = PasswordPolicy(
        minLength: 12,
        requireSpecial: true,
        minStrength: .strong
    )

    /// Relaxed policy.
    public static let relaxed = PasswordPolicy(
        minLength: 6,
        requireUppercase: false,
        requireNumbers: false,
        minStrength: .weak
    )
}

/// Password utilities.
public enum SafePassword {
    // Common weak passwords (subset)
    private static let commonPasswords: Set<String> = [
        "password", "123456", "12345678", "qwerty", "abc123",
        "monkey", "1234567", "letmein", "trustno1", "dragon",
        "baseball", "iloveyou", "master", "sunshine", "ashley",
        "football", "shadow", "123123", "654321", "superman",
        "qazwsx", "michael", "password1", "password123"
    ]

    /// Validate password against policy.
    public static func validate(_ password: String, policy: PasswordPolicy = .default) -> PasswordValidation {
        var feedback: [String] = []
        var score = 0

        // Length checks
        if password.count < policy.minLength {
            feedback.append("Password must be at least \(policy.minLength) characters")
        } else {
            score += min(password.count, 20)
        }

        if password.count > policy.maxLength {
            feedback.append("Password must be at most \(policy.maxLength) characters")
        }

        // Character type checks
        let hasUppercase = password.contains(where: { $0.isUppercase })
        let hasLowercase = password.contains(where: { $0.isLowercase })
        let hasNumbers = password.contains(where: { $0.isNumber })
        let hasSpecial = password.contains(where: { !$0.isLetter && !$0.isNumber })

        if policy.requireUppercase && !hasUppercase {
            feedback.append("Password must contain uppercase letters")
        } else if hasUppercase {
            score += 10
        }

        if policy.requireLowercase && !hasLowercase {
            feedback.append("Password must contain lowercase letters")
        } else if hasLowercase {
            score += 10
        }

        if policy.requireNumbers && !hasNumbers {
            feedback.append("Password must contain numbers")
        } else if hasNumbers {
            score += 10
        }

        if policy.requireSpecial && !hasSpecial {
            feedback.append("Password must contain special characters")
        } else if hasSpecial {
            score += 15
        }

        // Common password check
        if policy.disallowCommon && commonPasswords.contains(password.lowercased()) {
            feedback.append("Password is too common")
            score = max(0, score - 30)
        }

        // Repeating character check
        if hasRepeatingChars(password, maxCount: policy.maxRepeating) {
            feedback.append("Password has too many repeating characters")
            score = max(0, score - 10)
        }

        // Calculate entropy
        let entropy = calculateEntropy(password)
        score += Int(entropy / 4)

        // Determine strength
        let strength: PasswordStrength
        switch score {
        case 0..<20: strength = .veryWeak
        case 20..<40: strength = .weak
        case 40..<60: strength = .fair
        case 60..<80: strength = .strong
        default: strength = .veryStrong
        }

        if strength < policy.minStrength {
            feedback.append("Password strength must be at least \(policy.minStrength.description)")
        }

        let isValid = feedback.isEmpty && password.count >= policy.minLength && password.count <= policy.maxLength

        return PasswordValidation(
            isValid: isValid,
            strength: strength,
            score: min(100, score),
            feedback: feedback,
            entropy: entropy
        )
    }

    /// Calculate password entropy.
    public static func calculateEntropy(_ password: String) -> Double {
        guard !password.isEmpty else { return 0 }

        var charsetSize = 0

        if password.contains(where: { $0.isLowercase }) {
            charsetSize += 26
        }
        if password.contains(where: { $0.isUppercase }) {
            charsetSize += 26
        }
        if password.contains(where: { $0.isNumber }) {
            charsetSize += 10
        }
        if password.contains(where: { !$0.isLetter && !$0.isNumber }) {
            charsetSize += 32
        }

        guard charsetSize > 0 else { return 0 }

        return Double(password.count) * log2(Double(charsetSize))
    }

    /// Check for repeating characters.
    private static func hasRepeatingChars(_ password: String, maxCount: Int) -> Bool {
        var count = 1
        var lastChar: Character?

        for char in password {
            if char == lastChar {
                count += 1
                if count > maxCount {
                    return true
                }
            } else {
                count = 1
                lastChar = char
            }
        }

        return false
    }

    /// Generate a random password.
    public static func generate(
        length: Int = 16,
        includeUppercase: Bool = true,
        includeLowercase: Bool = true,
        includeNumbers: Bool = true,
        includeSpecial: Bool = true,
        excludeAmbiguous: Bool = true
    ) -> String {
        var charset = ""

        let lowercase = excludeAmbiguous ? "abcdefghjkmnpqrstuvwxyz" : "abcdefghijklmnopqrstuvwxyz"
        let uppercase = excludeAmbiguous ? "ABCDEFGHJKMNPQRSTUVWXYZ" : "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let numbers = excludeAmbiguous ? "23456789" : "0123456789"
        let special = "!@#$%^&*()_+-=[]{}|;:,.<>?"

        if includeLowercase { charset += lowercase }
        if includeUppercase { charset += uppercase }
        if includeNumbers { charset += numbers }
        if includeSpecial { charset += special }

        guard !charset.isEmpty else { return "" }

        var password = ""
        let charArray = Array(charset)

        for _ in 0..<max(1, length) {
            let randomIndex = Int.random(in: 0..<charArray.count)
            password.append(charArray[randomIndex])
        }

        return password
    }

    /// Generate a passphrase.
    public static func generatePassphrase(
        wordCount: Int = 4,
        separator: String = "-",
        capitalize: Bool = true
    ) -> String {
        // Simple word list (in production, use a larger list)
        let words = [
            "apple", "banana", "cherry", "dragon", "eagle", "forest",
            "garden", "harbor", "island", "jungle", "knight", "lemon",
            "meadow", "nectar", "orange", "palace", "quartz", "river",
            "sunset", "tiger", "umbrella", "valley", "winter", "yellow",
            "zenith", "anchor", "breeze", "castle", "dolphin", "ember"
        ]

        var passphrase: [String] = []

        for _ in 0..<max(1, wordCount) {
            var word = words.randomElement() ?? "word"
            if capitalize {
                word = word.prefix(1).uppercased() + word.dropFirst()
            }
            passphrase.append(word)
        }

        return passphrase.joined(separator: separator)
    }

    /// Check password strength (simplified).
    public static func checkStrength(_ password: String) -> PasswordStrength {
        validate(password, policy: PasswordPolicy(
            minLength: 1,
            requireUppercase: false,
            requireLowercase: false,
            requireNumbers: false,
            requireSpecial: false,
            minStrength: .veryWeak,
            disallowCommon: false
        )).strength
    }
}
