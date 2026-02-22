// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeDigest provides cryptographic digest operations via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
// #include <stdlib.h>
import "C"

// Hash algorithm constants.
const (
	HashSHA256 = 0
	HashSHA384 = 1
	HashSHA512 = 2
	HashBlake3 = 3
)

// ProvenDigest represents a parsed cryptographic digest.
type ProvenDigest struct {
	Algorithm uint8
	Value     string
}

// DigestParse parses a digest string (e.g., "sha256:abc123...").
func DigestParse(input string) (*ProvenDigest, error) {
	cs, length := cString(input)
	defer unsafeFree(cs)

	result := C.proven_digest_parse(cs, length)
	if int(result.status) != StatusOK {
		return nil, newError(int(result.status))
	}

	d := &ProvenDigest{
		Algorithm: uint8(result.digest.algorithm),
	}
	if result.digest.value != nil {
		d.Value = C.GoStringN(result.digest.value, C.int(result.digest.value_len))
	}

	return d, nil
}

// DigestVerify performs a constant-time comparison of two digests.
func DigestVerify(expected, actual *ProvenDigest) (bool, error) {
	cExpected := C.Digest{algorithm: C.uint8_t(expected.Algorithm)}
	cActual := C.Digest{algorithm: C.uint8_t(actual.Algorithm)}

	var expectedValueCS, actualValueCS *C.char

	if expected.Value != "" {
		expectedValueCS = C.CString(expected.Value)
		defer unsafeFree(expectedValueCS)
		cExpected.value = expectedValueCS
		cExpected.value_len = C.size_t(len(expected.Value))
	}
	if actual.Value != "" {
		actualValueCS = C.CString(actual.Value)
		defer unsafeFree(actualValueCS)
		cActual.value = actualValueCS
		cActual.value_len = C.size_t(len(actual.Value))
	}

	result := C.proven_digest_verify(&cExpected, &cActual)
	if int(result.status) != StatusOK {
		return false, newError(int(result.status))
	}
	return bool(result.value), nil
}

// DigestToString converts a digest to its string representation (algorithm:hex).
func DigestToString(d *ProvenDigest) (string, error) {
	cDigest := C.Digest{algorithm: C.uint8_t(d.Algorithm)}

	if d.Value != "" {
		valueCS := C.CString(d.Value)
		defer unsafeFree(valueCS)
		cDigest.value = valueCS
		cDigest.value_len = C.size_t(len(d.Value))
	}

	return goStringResult(C.proven_digest_to_string(&cDigest))
}
