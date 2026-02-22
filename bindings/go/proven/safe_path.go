// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafePath provides filesystem operations that prevent directory traversal attacks.
// All computation is performed in Idris 2 via the Proven FFI.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// HasTraversal checks whether a path attempts directory traversal.
func HasTraversal(path string) (bool, error) {
	cs, length := cString(path)
	defer unsafeFree(cs)
	result := C.proven_path_has_traversal(cs, length)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// SanitizeFilename sanitizes a filename by removing dangerous characters.
func SanitizeFilename(filename string) (string, error) {
	cs, length := cString(filename)
	defer unsafeFree(cs)
	return goStringResult(C.proven_path_sanitize_filename(cs, length))
}
