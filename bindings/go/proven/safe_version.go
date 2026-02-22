// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeVersion provides semantic versioning operations via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// SemVer represents a parsed semantic version.
type SemVer struct {
	Major      uint32
	Minor      uint32
	Patch      uint32
	Prerelease string
}

// VersionParse parses a semantic version string (e.g., "1.2.3-beta").
func VersionParse(input string) (*SemVer, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)

	result := C.proven_version_parse(cs, length)
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}

	sv := &SemVer{
		Major: uint32(result.version.major),
		Minor: uint32(result.version.minor),
		Patch: uint32(result.version.patch),
	}

	if result.version.prerelease != nil && result.version.prerelease_len > 0 {
		sv.Prerelease = C.GoStringN(result.version.prerelease, C.int(result.version.prerelease_len))
	}

	// Free the C-allocated version data.
	C.proven_version_free(&result.version)

	return sv, nil
}

// VersionCompare compares two semantic versions.
// Returns -1 if a < b, 0 if a == b, 1 if a > b.
func VersionCompare(a, b *SemVer) int32 {
	cA := C.SemanticVersion{
		major: C.uint32_t(a.Major),
		minor: C.uint32_t(a.Minor),
		patch: C.uint32_t(a.Patch),
	}
	if a.Prerelease != "" {
		preCS := C.CString(a.Prerelease)
		defer unsafeFree(preCS)
		cA.prerelease = preCS
		cA.prerelease_len = C.size_t(len(a.Prerelease))
	}

	cB := C.SemanticVersion{
		major: C.uint32_t(b.Major),
		minor: C.uint32_t(b.Minor),
		patch: C.uint32_t(b.Patch),
	}
	if b.Prerelease != "" {
		preCS := C.CString(b.Prerelease)
		defer unsafeFree(preCS)
		cB.prerelease = preCS
		cB.prerelease_len = C.size_t(len(b.Prerelease))
	}

	return int32(C.proven_version_compare(cA, cB))
}
