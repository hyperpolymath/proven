// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"sync"
)

// CircuitState represents the state of a circuit breaker.
type CircuitState int

const (
	CircuitClosed CircuitState = iota
	CircuitOpen
	CircuitHalfOpen
)

// String returns the string representation of the circuit state.
func (s CircuitState) String() string {
	switch s {
	case CircuitClosed:
		return "closed"
	case CircuitOpen:
		return "open"
	case CircuitHalfOpen:
		return "half_open"
	default:
		return "unknown"
	}
}

// CircuitBreakerConfig configures a circuit breaker.
type CircuitBreakerConfig struct {
	FailureThreshold    int   // Failures before opening
	SuccessThreshold    int   // Successes in half-open before closing
	TimeoutMs           int64 // Time before half-open
	HalfOpenMaxRequests int   // Max requests in half-open state
}

// DefaultCircuitBreakerConfig returns sensible defaults.
func DefaultCircuitBreakerConfig() CircuitBreakerConfig {
	return CircuitBreakerConfig{
		FailureThreshold:    5,
		SuccessThreshold:    2,
		TimeoutMs:           30000,
		HalfOpenMaxRequests: 1,
	}
}

// CircuitBreaker implements the circuit breaker pattern.
type CircuitBreaker struct {
	config             CircuitBreakerConfig
	state              CircuitState
	failureCount       int
	successCount       int
	halfOpenRequests   int
	lastFailureTime    int64
	mu                 sync.Mutex
	getNow             func() int64
	onStateChange      func(from, to CircuitState)
}

// NewCircuitBreaker creates a circuit breaker with the given config.
func NewCircuitBreaker(config CircuitBreakerConfig, getNow func() int64) *CircuitBreaker {
	return &CircuitBreaker{
		config: config,
		state:  CircuitClosed,
		getNow: getNow,
	}
}

// OnStateChange sets a callback for state changes.
func (cb *CircuitBreaker) OnStateChange(handler func(from, to CircuitState)) {
	cb.mu.Lock()
	defer cb.mu.Unlock()
	cb.onStateChange = handler
}

// State returns the current circuit state.
func (cb *CircuitBreaker) State() CircuitState {
	cb.mu.Lock()
	defer cb.mu.Unlock()
	cb.checkStateTransition()
	return cb.state
}

// AllowRequest checks if a request should be allowed.
func (cb *CircuitBreaker) AllowRequest() bool {
	cb.mu.Lock()
	defer cb.mu.Unlock()

	cb.checkStateTransition()

	switch cb.state {
	case CircuitClosed:
		return true
	case CircuitOpen:
		return false
	case CircuitHalfOpen:
		if cb.halfOpenRequests < cb.config.HalfOpenMaxRequests {
			cb.halfOpenRequests++
			return true
		}
		return false
	}
	return false
}

// RecordSuccess records a successful request.
func (cb *CircuitBreaker) RecordSuccess() {
	cb.mu.Lock()
	defer cb.mu.Unlock()

	switch cb.state {
	case CircuitClosed:
		cb.failureCount = 0
	case CircuitHalfOpen:
		cb.successCount++
		if cb.successCount >= cb.config.SuccessThreshold {
			cb.transition(CircuitClosed)
		}
	}
}

// RecordFailure records a failed request.
func (cb *CircuitBreaker) RecordFailure() {
	cb.mu.Lock()
	defer cb.mu.Unlock()

	cb.lastFailureTime = cb.getNow()

	switch cb.state {
	case CircuitClosed:
		cb.failureCount++
		if cb.failureCount >= cb.config.FailureThreshold {
			cb.transition(CircuitOpen)
		}
	case CircuitHalfOpen:
		cb.transition(CircuitOpen)
	}
}

// Reset resets the circuit breaker to closed state.
func (cb *CircuitBreaker) Reset() {
	cb.mu.Lock()
	defer cb.mu.Unlock()
	cb.transition(CircuitClosed)
}

// FailureCount returns the current failure count.
func (cb *CircuitBreaker) FailureCount() int {
	cb.mu.Lock()
	defer cb.mu.Unlock()
	return cb.failureCount
}

// TimeUntilHalfOpen returns milliseconds until the circuit may transition to half-open.
func (cb *CircuitBreaker) TimeUntilHalfOpen() int64 {
	cb.mu.Lock()
	defer cb.mu.Unlock()

	if cb.state != CircuitOpen {
		return 0
	}

	elapsed := cb.getNow() - cb.lastFailureTime
	remaining := cb.config.TimeoutMs - elapsed
	if remaining < 0 {
		return 0
	}
	return remaining
}

func (cb *CircuitBreaker) checkStateTransition() {
	if cb.state == CircuitOpen {
		elapsed := cb.getNow() - cb.lastFailureTime
		if elapsed >= cb.config.TimeoutMs {
			cb.transition(CircuitHalfOpen)
		}
	}
}

func (cb *CircuitBreaker) transition(to CircuitState) {
	from := cb.state
	cb.state = to

	// Reset counters
	cb.failureCount = 0
	cb.successCount = 0
	cb.halfOpenRequests = 0

	if cb.onStateChange != nil && from != to {
		cb.onStateChange(from, to)
	}
}

// CircuitBreakerStats contains circuit breaker statistics.
type CircuitBreakerStats struct {
	State             CircuitState
	FailureCount      int
	SuccessCount      int
	TotalRequests     int
	RejectedRequests  int
	TimeInCurrentState int64
}

// CircuitBreakerGroup manages multiple circuit breakers by key.
type CircuitBreakerGroup struct {
	config   CircuitBreakerConfig
	breakers map[string]*CircuitBreaker
	mu       sync.RWMutex
	getNow   func() int64
}

// NewCircuitBreakerGroup creates a circuit breaker group.
func NewCircuitBreakerGroup(config CircuitBreakerConfig, getNow func() int64) *CircuitBreakerGroup {
	return &CircuitBreakerGroup{
		config:   config,
		breakers: make(map[string]*CircuitBreaker),
		getNow:   getNow,
	}
}

// Get returns the circuit breaker for the given key, creating if needed.
func (g *CircuitBreakerGroup) Get(key string) *CircuitBreaker {
	g.mu.RLock()
	cb, exists := g.breakers[key]
	g.mu.RUnlock()

	if exists {
		return cb
	}

	g.mu.Lock()
	defer g.mu.Unlock()

	// Double-check after acquiring write lock
	if cb, exists = g.breakers[key]; exists {
		return cb
	}

	cb = NewCircuitBreaker(g.config, g.getNow)
	g.breakers[key] = cb
	return cb
}

// Remove removes a circuit breaker by key.
func (g *CircuitBreakerGroup) Remove(key string) {
	g.mu.Lock()
	defer g.mu.Unlock()
	delete(g.breakers, key)
}

// Keys returns all keys with circuit breakers.
func (g *CircuitBreakerGroup) Keys() []string {
	g.mu.RLock()
	defer g.mu.RUnlock()

	keys := make([]string, 0, len(g.breakers))
	for k := range g.breakers {
		keys = append(keys, k)
	}
	return keys
}

// ResetAll resets all circuit breakers.
func (g *CircuitBreakerGroup) ResetAll() {
	g.mu.RLock()
	defer g.mu.RUnlock()

	for _, cb := range g.breakers {
		cb.Reset()
	}
}
