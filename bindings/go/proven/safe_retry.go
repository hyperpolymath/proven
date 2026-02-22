// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

// SafeRetry provides exponential backoff retry logic via the Proven FFI.
// All computation is performed in Idris 2 with formal verification.

package proven

// #include <stdint.h>
// #include <stdbool.h>
import "C"

// RetryConf holds the configuration for retry behavior.
type RetryConf struct {
	MaxAttempts  uint32
	BaseDelayMs  uint64
	MaxDelayMs   uint64
	Multiplier   float64
}

// RetryDelay calculates the delay in milliseconds for a given attempt number.
// Includes jitter via the Proven FFI.
func RetryDelay(config RetryConf, attempt uint32) uint64 {
	cConfig := C.RetryConfig{
		max_attempts:  C.uint32_t(config.MaxAttempts),
		base_delay_ms: C.uint64_t(config.BaseDelayMs),
		max_delay_ms:  C.uint64_t(config.MaxDelayMs),
		multiplier:    C.double(config.Multiplier),
	}
	return uint64(C.proven_retry_delay(cConfig, C.uint32_t(attempt)))
}

// RetryShouldRetry checks whether a retry should be attempted for the given
// attempt number based on the configuration.
func RetryShouldRetry(config RetryConf, attempt uint32) bool {
	cConfig := C.RetryConfig{
		max_attempts:  C.uint32_t(config.MaxAttempts),
		base_delay_ms: C.uint64_t(config.BaseDelayMs),
		max_delay_ms:  C.uint64_t(config.MaxDelayMs),
		multiplier:    C.double(config.Multiplier),
	}
	return bool(C.proven_retry_should_retry(cConfig, C.uint32_t(attempt)))
}
