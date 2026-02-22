// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeEmail provides email validation via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// IsValidEmail checks whether an email address is valid (RFC 5321 simplified).
func IsValidEmail(email string) (bool, error) {
	cs, length := cString(email)
	defer unsafeFree(cs)
	result := C.proven_email_is_valid(cs, length)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}
