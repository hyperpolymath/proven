// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"math"
	"math/rand"
)

// RetryStrategy defines how delays are calculated between retries.
type RetryStrategy int

const (
	RetryFixed RetryStrategy = iota
	RetryLinear
	RetryExponential
)

// String returns the string representation of the retry strategy.
func (s RetryStrategy) String() string {
	switch s {
	case RetryFixed:
		return "fixed"
	case RetryLinear:
		return "linear"
	case RetryExponential:
		return "exponential"
	default:
		return "unknown"
	}
}

// RetryConfig configures retry behavior.
type RetryConfig struct {
	MaxAttempts   int
	InitialDelay  int64 // Milliseconds
	MaxDelay      int64 // Milliseconds
	Strategy      RetryStrategy
	JitterFactor  float64 // 0.0 to 1.0
	BackoffFactor float64 // For exponential/linear
}

// DefaultRetryConfig returns sensible defaults.
func DefaultRetryConfig() RetryConfig {
	return RetryConfig{
		MaxAttempts:   3,
		InitialDelay:  1000,
		MaxDelay:      30000,
		Strategy:      RetryExponential,
		JitterFactor:  0.1,
		BackoffFactor: 2.0,
	}
}

// Retry manages retry logic with configurable backoff.
type Retry struct {
	config   RetryConfig
	attempt  int
	rng      *rand.Rand
}

// NewRetry creates a retry handler with the given config.
func NewRetry(config RetryConfig) *Retry {
	if config.MaxAttempts < 1 {
		config.MaxAttempts = 1
	}
	if config.InitialDelay < 0 {
		config.InitialDelay = 0
	}
	if config.MaxDelay < config.InitialDelay {
		config.MaxDelay = config.InitialDelay
	}
	if config.JitterFactor < 0 {
		config.JitterFactor = 0
	}
	if config.JitterFactor > 1 {
		config.JitterFactor = 1
	}
	if config.BackoffFactor < 1 {
		config.BackoffFactor = 1
	}

	return &Retry{
		config: config,
		rng:    rand.New(rand.NewSource(rand.Int63())),
	}
}

// ShouldRetry returns true if more attempts are available.
func (r *Retry) ShouldRetry() bool {
	return r.attempt < r.config.MaxAttempts
}

// NextDelay returns the delay before the next attempt in milliseconds.
func (r *Retry) NextDelay() int64 {
	delay := r.calculateDelay()
	delay = r.applyJitter(delay)

	if delay > r.config.MaxDelay {
		delay = r.config.MaxDelay
	}

	return delay
}

// RecordAttempt records an attempt and returns whether more attempts are available.
func (r *Retry) RecordAttempt() bool {
	r.attempt++
	return r.ShouldRetry()
}

// Attempt returns the current attempt number (1-indexed).
func (r *Retry) Attempt() int {
	return r.attempt
}

// RemainingAttempts returns the number of remaining attempts.
func (r *Retry) RemainingAttempts() int {
	remaining := r.config.MaxAttempts - r.attempt
	if remaining < 0 {
		return 0
	}
	return remaining
}

// Reset resets the retry counter.
func (r *Retry) Reset() {
	r.attempt = 0
}

func (r *Retry) calculateDelay() int64 {
	switch r.config.Strategy {
	case RetryFixed:
		return r.config.InitialDelay
	case RetryLinear:
		return r.config.InitialDelay + int64(float64(r.attempt)*r.config.BackoffFactor*float64(r.config.InitialDelay))
	case RetryExponential:
		return int64(float64(r.config.InitialDelay) * math.Pow(r.config.BackoffFactor, float64(r.attempt)))
	default:
		return r.config.InitialDelay
	}
}

func (r *Retry) applyJitter(delay int64) int64 {
	if r.config.JitterFactor == 0 {
		return delay
	}

	jitterRange := float64(delay) * r.config.JitterFactor
	jitter := (r.rng.Float64() * 2 - 1) * jitterRange
	result := float64(delay) + jitter

	if result < 0 {
		return 0
	}
	return int64(result)
}

// RetryResult represents the result of a retry operation.
type RetryResult[T any] struct {
	Value    T
	Attempts int
	Success  bool
	Errors   []error
}

// RetryWithBackoff executes a function with retry logic.
func RetryWithBackoff[T any](config RetryConfig, fn func() (T, error), sleep func(int64)) RetryResult[T] {
	retry := NewRetry(config)
	var result RetryResult[T]
	result.Errors = make([]error, 0)

	for {
		value, err := fn()
		result.Attempts++

		if err == nil {
			result.Value = value
			result.Success = true
			return result
		}

		result.Errors = append(result.Errors, err)

		if !retry.RecordAttempt() {
			return result
		}

		delay := retry.NextDelay()
		sleep(delay)
	}
}

// RetryableError marks an error as retryable.
type RetryableError struct {
	Err       error
	Retryable bool
}

func (e RetryableError) Error() string {
	return e.Err.Error()
}

// NewRetryableError creates a retryable error.
func NewRetryableError(err error) RetryableError {
	return RetryableError{Err: err, Retryable: true}
}

// NewNonRetryableError creates a non-retryable error.
func NewNonRetryableError(err error) RetryableError {
	return RetryableError{Err: err, Retryable: false}
}

// IsRetryable checks if an error should be retried.
func IsRetryable(err error) bool {
	if re, ok := err.(RetryableError); ok {
		return re.Retryable
	}
	return true // Default to retryable
}

// RetryWithBackoffSelective retries only on retryable errors.
func RetryWithBackoffSelective[T any](config RetryConfig, fn func() (T, error), sleep func(int64)) RetryResult[T] {
	retry := NewRetry(config)
	var result RetryResult[T]
	result.Errors = make([]error, 0)

	for {
		value, err := fn()
		result.Attempts++

		if err == nil {
			result.Value = value
			result.Success = true
			return result
		}

		result.Errors = append(result.Errors, err)

		if !IsRetryable(err) {
			return result
		}

		if !retry.RecordAttempt() {
			return result
		}

		delay := retry.NextDelay()
		sleep(delay)
	}
}
