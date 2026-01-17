// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"math"
	"strings"
	"unicode"
)

// PasswordStrength represents password strength levels.
type PasswordStrength int

const (
	// StrengthWeak indicates a weak password.
	StrengthWeak PasswordStrength = iota
	// StrengthFair indicates a fair password.
	StrengthFair
	// StrengthGood indicates a good password.
	StrengthGood
	// StrengthStrong indicates a strong password.
	StrengthStrong
	// StrengthVeryStrong indicates a very strong password.
	StrengthVeryStrong
)

// String returns string representation of strength.
func (s PasswordStrength) String() string {
	switch s {
	case StrengthWeak:
		return "weak"
	case StrengthFair:
		return "fair"
	case StrengthGood:
		return "good"
	case StrengthStrong:
		return "strong"
	case StrengthVeryStrong:
		return "very_strong"
	default:
		return "unknown"
	}
}

// PasswordAnalysis contains password analysis results.
type PasswordAnalysis struct {
	Length       int
	HasLower     bool
	HasUpper     bool
	HasDigit     bool
	HasSpecial   bool
	Entropy      float64
	Strength     PasswordStrength
	CharsetSize  int
	UniqueChars  int
}

// AnalyzePassword analyzes a password's strength.
func AnalyzePassword(password string) PasswordAnalysis {
	analysis := PasswordAnalysis{
		Length: len(password),
	}

	uniqueChars := make(map[rune]bool)
	charsetSize := 0

	for _, r := range password {
		uniqueChars[r] = true

		if unicode.IsLower(r) {
			analysis.HasLower = true
		} else if unicode.IsUpper(r) {
			analysis.HasUpper = true
		} else if unicode.IsDigit(r) {
			analysis.HasDigit = true
		} else {
			analysis.HasSpecial = true
		}
	}

	analysis.UniqueChars = len(uniqueChars)

	// Calculate charset size
	if analysis.HasLower {
		charsetSize += 26
	}
	if analysis.HasUpper {
		charsetSize += 26
	}
	if analysis.HasDigit {
		charsetSize += 10
	}
	if analysis.HasSpecial {
		charsetSize += 32
	}
	analysis.CharsetSize = charsetSize

	// Calculate entropy
	if charsetSize > 0 && analysis.Length > 0 {
		analysis.Entropy = float64(analysis.Length) * math.Log2(float64(charsetSize))
	}

	// Determine strength
	analysis.Strength = calculateStrength(analysis)

	return analysis
}

func calculateStrength(a PasswordAnalysis) PasswordStrength {
	if a.Length < 8 {
		return StrengthWeak
	}

	score := 0

	if a.Length >= 8 {
		score++
	}
	if a.Length >= 12 {
		score++
	}
	if a.Length >= 16 {
		score++
	}
	if a.HasLower && a.HasUpper {
		score++
	}
	if a.HasDigit {
		score++
	}
	if a.HasSpecial {
		score++
	}
	if a.Entropy >= 60 {
		score++
	}

	switch {
	case score <= 2:
		return StrengthWeak
	case score <= 3:
		return StrengthFair
	case score <= 4:
		return StrengthGood
	case score <= 5:
		return StrengthStrong
	default:
		return StrengthVeryStrong
	}
}

// PasswordPolicy defines password requirements.
type PasswordPolicy struct {
	MinLength      int
	MaxLength      int
	RequireLower   bool
	RequireUpper   bool
	RequireDigit   bool
	RequireSpecial bool
	MinEntropy     float64
}

// DefaultPasswordPolicy returns a sensible default policy.
func DefaultPasswordPolicy() PasswordPolicy {
	return PasswordPolicy{
		MinLength:      8,
		MaxLength:      128,
		RequireLower:   true,
		RequireUpper:   true,
		RequireDigit:   true,
		RequireSpecial: false,
		MinEntropy:     40,
	}
}

// StrictPasswordPolicy returns a strict policy.
func StrictPasswordPolicy() PasswordPolicy {
	return PasswordPolicy{
		MinLength:      12,
		MaxLength:      128,
		RequireLower:   true,
		RequireUpper:   true,
		RequireDigit:   true,
		RequireSpecial: true,
		MinEntropy:     60,
	}
}

// ValidatePassword validates a password against a policy.
func ValidatePassword(password string, policy PasswordPolicy) []string {
	var errors []string
	analysis := AnalyzePassword(password)

	if analysis.Length < policy.MinLength {
		errors = append(errors, "password too short")
	}
	if policy.MaxLength > 0 && analysis.Length > policy.MaxLength {
		errors = append(errors, "password too long")
	}
	if policy.RequireLower && !analysis.HasLower {
		errors = append(errors, "missing lowercase letter")
	}
	if policy.RequireUpper && !analysis.HasUpper {
		errors = append(errors, "missing uppercase letter")
	}
	if policy.RequireDigit && !analysis.HasDigit {
		errors = append(errors, "missing digit")
	}
	if policy.RequireSpecial && !analysis.HasSpecial {
		errors = append(errors, "missing special character")
	}
	if analysis.Entropy < policy.MinEntropy {
		errors = append(errors, "insufficient entropy")
	}

	return errors
}

// IsValidPassword checks if password meets default policy.
func IsValidPassword(password string) bool {
	errors := ValidatePassword(password, DefaultPasswordPolicy())
	return len(errors) == 0
}

// CommonPasswords is a small set of common passwords to reject.
var CommonPasswords = []string{
	"password", "123456", "12345678", "qwerty", "abc123",
	"password1", "admin", "letmein", "welcome", "monkey",
}

// IsCommonPassword checks if password is in common passwords list.
func IsCommonPassword(password string) bool {
	lower := strings.ToLower(password)
	for _, common := range CommonPasswords {
		if lower == common {
			return true
		}
	}
	return false
}
