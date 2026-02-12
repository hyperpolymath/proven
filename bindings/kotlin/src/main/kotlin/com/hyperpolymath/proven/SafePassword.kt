// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

import kotlin.math.log2
import kotlin.random.Random

/**
 * Password strength levels.
 */
enum class PasswordStrength {
    VERY_WEAK,
    WEAK,
    FAIR,
    STRONG,
    VERY_STRONG
}

/**
 * Password validation result.
 */
data class PasswordValidation(
    val isValid: Boolean,
    val strength: PasswordStrength,
    val entropy: Double,
    val issues: List<String>
)

/**
 * Password policy configuration.
 */
data class PasswordPolicy(
    val minLength: Int = 8,
    val maxLength: Int = 128,
    val requireUppercase: Boolean = true,
    val requireLowercase: Boolean = true,
    val requireDigits: Boolean = true,
    val requireSpecial: Boolean = false,
    val minUppercase: Int = 1,
    val minLowercase: Int = 1,
    val minDigits: Int = 1,
    val minSpecial: Int = 0,
    val allowedSpecialChars: String = "!@#$%^&*()_+-=[]{}|;':\",./<>?",
    val disallowCommonPasswords: Boolean = true,
    val disallowRepeatingChars: Int = 3,
    val disallowSequentialChars: Int = 3
) {
    companion object {
        val MINIMAL = PasswordPolicy(
            minLength = 6,
            requireUppercase = false,
            requireLowercase = true,
            requireDigits = false,
            requireSpecial = false
        )

        val STANDARD = PasswordPolicy()

        val STRICT = PasswordPolicy(
            minLength = 12,
            requireUppercase = true,
            requireLowercase = true,
            requireDigits = true,
            requireSpecial = true,
            minUppercase = 2,
            minLowercase = 2,
            minDigits = 2,
            minSpecial = 1
        )

        val PASSPHRASE = PasswordPolicy(
            minLength = 20,
            maxLength = 256,
            requireUppercase = false,
            requireLowercase = true,
            requireDigits = false,
            requireSpecial = false
        )
    }
}

/**
 * Common passwords list (subset for demonstration).
 */
private val COMMON_PASSWORDS = setOf(
    "password", "123456", "123456789", "12345678", "12345",
    "1234567", "1234567890", "qwerty", "abc123", "111111",
    "password1", "iloveyou", "1q2w3e4r", "000000", "qwerty123",
    "zaq12wsx", "dragon", "sunshine", "princess", "letmein",
    "monkey", "shadow", "master", "666666", "qwertyuiop",
    "123321", "mustang", "michael", "football", "password123"
)

/**
 * Password utilities.
 */
object SafePassword {
    private val UPPERCASE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    private val LOWERCASE = "abcdefghijklmnopqrstuvwxyz"
    private val DIGITS = "0123456789"
    private val SPECIAL = "!@#$%^&*()_+-=[]{}|;':\",./<>?"

    /**
     * Validate password against policy.
     */
    fun validate(password: String, policy: PasswordPolicy = PasswordPolicy.STANDARD): PasswordValidation {
        val issues = mutableListOf<String>()

        // Length checks
        if (password.length < policy.minLength) {
            issues.add("Password must be at least ${policy.minLength} characters")
        }
        if (password.length > policy.maxLength) {
            issues.add("Password must be at most ${policy.maxLength} characters")
        }

        // Character class counts
        val uppercaseCount = password.count { it.isUpperCase() }
        val lowercaseCount = password.count { it.isLowerCase() }
        val digitCount = password.count { it.isDigit() }
        val specialCount = password.count { it in policy.allowedSpecialChars }

        if (policy.requireUppercase && uppercaseCount < policy.minUppercase) {
            issues.add("Password must contain at least ${policy.minUppercase} uppercase letter(s)")
        }
        if (policy.requireLowercase && lowercaseCount < policy.minLowercase) {
            issues.add("Password must contain at least ${policy.minLowercase} lowercase letter(s)")
        }
        if (policy.requireDigits && digitCount < policy.minDigits) {
            issues.add("Password must contain at least ${policy.minDigits} digit(s)")
        }
        if (policy.requireSpecial && specialCount < policy.minSpecial) {
            issues.add("Password must contain at least ${policy.minSpecial} special character(s)")
        }

        // Common password check
        if (policy.disallowCommonPasswords && password.lowercase() in COMMON_PASSWORDS) {
            issues.add("Password is too common")
        }

        // Repeating characters check
        if (policy.disallowRepeatingChars > 0) {
            val repeatPattern = "(.)\\1{${policy.disallowRepeatingChars - 1},}".toRegex()
            if (repeatPattern.containsMatchIn(password)) {
                issues.add("Password contains too many repeating characters")
            }
        }

        // Sequential characters check
        if (policy.disallowSequentialChars > 0 && hasSequentialChars(password, policy.disallowSequentialChars)) {
            issues.add("Password contains sequential characters")
        }

        val entropy = calculateEntropy(password)
        val strength = calculateStrength(password, entropy)

        return PasswordValidation(
            isValid = issues.isEmpty(),
            strength = strength,
            entropy = entropy,
            issues = issues
        )
    }

    /**
     * Calculate password entropy in bits.
     */
    fun calculateEntropy(password: String): Double {
        if (password.isEmpty()) return 0.0

        val hasUpper = password.any { it.isUpperCase() }
        val hasLower = password.any { it.isLowerCase() }
        val hasDigit = password.any { it.isDigit() }
        val hasSpecial = password.any { !it.isLetterOrDigit() }

        var poolSize = 0
        if (hasUpper) poolSize += 26
        if (hasLower) poolSize += 26
        if (hasDigit) poolSize += 10
        if (hasSpecial) poolSize += 32

        if (poolSize == 0) return 0.0

        return password.length * log2(poolSize.toDouble())
    }

    /**
     * Calculate password strength based on various factors.
     */
    fun calculateStrength(password: String, entropy: Double = calculateEntropy(password)): PasswordStrength {
        // Check for common patterns that weaken the password
        val lowerPassword = password.lowercase()
        val hasCommonPattern = COMMON_PASSWORDS.any { lowerPassword.contains(it) }

        val adjustedEntropy = if (hasCommonPattern) entropy * 0.5 else entropy

        return when {
            adjustedEntropy < 28 -> PasswordStrength.VERY_WEAK
            adjustedEntropy < 36 -> PasswordStrength.WEAK
            adjustedEntropy < 60 -> PasswordStrength.FAIR
            adjustedEntropy < 80 -> PasswordStrength.STRONG
            else -> PasswordStrength.VERY_STRONG
        }
    }

    /**
     * Generate a random password.
     */
    fun generate(
        length: Int = 16,
        includeUppercase: Boolean = true,
        includeLowercase: Boolean = true,
        includeDigits: Boolean = true,
        includeSpecial: Boolean = true,
        excludeAmbiguous: Boolean = true
    ): String {
        val ambiguousChars = "0O1lI"

        var charPool = ""
        if (includeUppercase) charPool += UPPERCASE
        if (includeLowercase) charPool += LOWERCASE
        if (includeDigits) charPool += DIGITS
        if (includeSpecial) charPool += SPECIAL

        if (excludeAmbiguous) {
            charPool = charPool.filter { it !in ambiguousChars }
        }

        if (charPool.isEmpty()) return ""

        val password = StringBuilder()

        // Ensure at least one character from each required category
        val requiredChars = mutableListOf<Char>()
        if (includeUppercase) {
            val pool = if (excludeAmbiguous) UPPERCASE.filter { it !in ambiguousChars } else UPPERCASE
            requiredChars.add(pool.random())
        }
        if (includeLowercase) {
            val pool = if (excludeAmbiguous) LOWERCASE.filter { it !in ambiguousChars } else LOWERCASE
            requiredChars.add(pool.random())
        }
        if (includeDigits) {
            val pool = if (excludeAmbiguous) DIGITS.filter { it !in ambiguousChars } else DIGITS
            requiredChars.add(pool.random())
        }
        if (includeSpecial) {
            requiredChars.add(SPECIAL.random())
        }

        // Add required characters
        password.append(requiredChars.joinToString(""))

        // Fill remaining length with random characters
        val remaining = length - password.length
        if (remaining > 0) {
            repeat(remaining) {
                password.append(charPool.random())
            }
        }

        // Shuffle the password
        return password.toString().toList().shuffled().joinToString("")
    }

    /**
     * Generate a passphrase from words.
     */
    fun generatePassphrase(
        wordCount: Int = 4,
        separator: String = "-",
        capitalize: Boolean = true
    ): String {
        // Simple word list (in production, use a larger dictionary)
        val words = listOf(
            "apple", "banana", "cherry", "dragon", "eagle", "forest", "garden", "harbor",
            "island", "jungle", "kingdom", "lemon", "mountain", "nectar", "ocean", "planet",
            "quantum", "river", "sunset", "thunder", "umbrella", "valley", "whisper", "xenon",
            "yellow", "zenith", "anchor", "bridge", "castle", "diamond", "ember", "falcon",
            "glacier", "horizon", "ivory", "jasmine", "knight", "lantern", "marble", "nebula"
        )

        val selectedWords = (1..wordCount).map { words.random() }
        val processed = if (capitalize) selectedWords.map { it.replaceFirstChar { c -> c.uppercase() } } else selectedWords

        return processed.joinToString(separator)
    }

    /**
     * Check if password contains sequential characters.
     */
    private fun hasSequentialChars(password: String, maxSequential: Int): Boolean {
        if (password.length < maxSequential) return false

        val sequences = listOf(
            "abcdefghijklmnopqrstuvwxyz",
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
            "0123456789",
            "qwertyuiop",
            "asdfghjkl",
            "zxcvbnm"
        )

        val lowerPassword = password.lowercase()

        for (seq in sequences) {
            for (i in 0..(seq.length - maxSequential)) {
                val subseq = seq.substring(i, i + maxSequential)
                if (lowerPassword.contains(subseq) || lowerPassword.contains(subseq.reversed())) {
                    return true
                }
            }
        }

        return false
    }

    /**
     * Check if two passwords are similar.
     */
    fun areSimilar(password1: String, password2: String, threshold: Double = 0.7): Boolean {
        val distance = levenshteinDistance(password1, password2)
        val maxLength = maxOf(password1.length, password2.length)
        if (maxLength == 0) return true

        val similarity = 1.0 - (distance.toDouble() / maxLength)
        return similarity >= threshold
    }

    /**
     * Calculate Levenshtein distance between two strings.
     */
    private fun levenshteinDistance(s1: String, s2: String): Int {
        val dp = Array(s1.length + 1) { IntArray(s2.length + 1) }

        for (i in 0..s1.length) dp[i][0] = i
        for (j in 0..s2.length) dp[0][j] = j

        for (i in 1..s1.length) {
            for (j in 1..s2.length) {
                val cost = if (s1[i - 1] == s2[j - 1]) 0 else 1
                dp[i][j] = minOf(
                    dp[i - 1][j] + 1,
                    dp[i][j - 1] + 1,
                    dp[i - 1][j - 1] + cost
                )
            }
        }

        return dp[s1.length][s2.length]
    }

    /**
     * Mask password for display (e.g., "pass****").
     */
    fun mask(password: String, visibleChars: Int = 4, maskChar: Char = '*'): String {
        if (password.length <= visibleChars) {
            return maskChar.toString().repeat(password.length)
        }
        return password.take(visibleChars) + maskChar.toString().repeat(password.length - visibleChars)
    }
}
