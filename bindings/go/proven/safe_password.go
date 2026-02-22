// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafePassword provides password validation via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// Password strength level constants.
const (
	PasswordVeryWeak   = 0
	PasswordWeak       = 1
	PasswordFair       = 2
	PasswordStrong     = 3
	PasswordVeryStrong = 4
)

// PasswordValidation holds the results of password strength analysis.
type PasswordValidation struct {
	Strength     int32
	HasLowercase bool
	HasUppercase bool
	HasDigit     bool
	HasSpecial   bool
	Length       int
}

// PasswordValidate analyzes a password's strength.
func PasswordValidate(password string) PasswordValidation {
	cs, length := cString(password)
	defer unsafeFree(cs)
	result := C.proven_password_validate(cs, length)
	return PasswordValidation{
		Strength:     int32(result.strength),
		HasLowercase: bool(result.has_lowercase),
		HasUppercase: bool(result.has_uppercase),
		HasDigit:     bool(result.has_digit),
		HasSpecial:   bool(result.has_special),
		Length:       int(result.length),
	}
}

// PasswordIsCommon checks whether a password is in the common passwords list.
func PasswordIsCommon(password string) bool {
	cs, length := cString(password)
	defer unsafeFree(cs)
	return bool(C.proven_password_is_common(cs, length))
}
