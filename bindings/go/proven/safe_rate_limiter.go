// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"sync"
)

// TokenBucket implements the token bucket rate limiting algorithm.
type TokenBucket struct {
	capacity       float64
	tokens         float64
	refillRate     float64 // Tokens per millisecond
	lastRefillTime int64
	mu             sync.Mutex
	getNow         func() int64
}

// NewTokenBucket creates a token bucket rate limiter.
func NewTokenBucket(capacity float64, refillRatePerSecond float64, getNow func() int64) *TokenBucket {
	return &TokenBucket{
		capacity:       capacity,
		tokens:         capacity,
		refillRate:     refillRatePerSecond / 1000.0,
		lastRefillTime: getNow(),
		getNow:         getNow,
	}
}

// TryConsume attempts to consume tokens, returns true if successful.
func (tb *TokenBucket) TryConsume(tokens float64) bool {
	tb.mu.Lock()
	defer tb.mu.Unlock()

	tb.refill()

	if tb.tokens >= tokens {
		tb.tokens -= tokens
		return true
	}
	return false
}

// AvailableTokens returns the current number of available tokens.
func (tb *TokenBucket) AvailableTokens() float64 {
	tb.mu.Lock()
	defer tb.mu.Unlock()
	tb.refill()
	return tb.tokens
}

// TimeUntilTokens returns milliseconds until the requested tokens are available.
func (tb *TokenBucket) TimeUntilTokens(tokens float64) int64 {
	tb.mu.Lock()
	defer tb.mu.Unlock()
	tb.refill()

	if tb.tokens >= tokens {
		return 0
	}

	needed := tokens - tb.tokens
	return int64(needed / tb.refillRate)
}

func (tb *TokenBucket) refill() {
	now := tb.getNow()
	elapsed := now - tb.lastRefillTime
	tb.tokens += float64(elapsed) * tb.refillRate
	if tb.tokens > tb.capacity {
		tb.tokens = tb.capacity
	}
	tb.lastRefillTime = now
}

// SlidingWindowLimiter implements sliding window rate limiting.
type SlidingWindowLimiter struct {
	maxRequests int
	windowMs    int64
	requests    []int64
	mu          sync.Mutex
	getNow      func() int64
}

// NewSlidingWindowLimiter creates a sliding window rate limiter.
func NewSlidingWindowLimiter(maxRequests int, windowMs int64, getNow func() int64) *SlidingWindowLimiter {
	return &SlidingWindowLimiter{
		maxRequests: maxRequests,
		windowMs:    windowMs,
		requests:    make([]int64, 0),
		getNow:      getNow,
	}
}

// TryAcquire attempts to acquire a request slot.
func (sw *SlidingWindowLimiter) TryAcquire() bool {
	sw.mu.Lock()
	defer sw.mu.Unlock()

	now := sw.getNow()
	sw.pruneOld(now)

	if len(sw.requests) >= sw.maxRequests {
		return false
	}

	sw.requests = append(sw.requests, now)
	return true
}

// RemainingRequests returns how many requests are available.
func (sw *SlidingWindowLimiter) RemainingRequests() int {
	sw.mu.Lock()
	defer sw.mu.Unlock()

	sw.pruneOld(sw.getNow())
	remaining := sw.maxRequests - len(sw.requests)
	if remaining < 0 {
		return 0
	}
	return remaining
}

// TimeUntilReset returns milliseconds until the oldest request expires.
func (sw *SlidingWindowLimiter) TimeUntilReset() int64 {
	sw.mu.Lock()
	defer sw.mu.Unlock()

	now := sw.getNow()
	sw.pruneOld(now)

	if len(sw.requests) == 0 {
		return 0
	}

	return (sw.requests[0] + sw.windowMs) - now
}

func (sw *SlidingWindowLimiter) pruneOld(now int64) {
	cutoff := now - sw.windowMs
	newRequests := make([]int64, 0, len(sw.requests))
	for _, t := range sw.requests {
		if t > cutoff {
			newRequests = append(newRequests, t)
		}
	}
	sw.requests = newRequests
}

// FixedWindowLimiter implements fixed window rate limiting.
type FixedWindowLimiter struct {
	maxRequests int
	windowMs    int64
	requests    int
	windowStart int64
	mu          sync.Mutex
	getNow      func() int64
}

// NewFixedWindowLimiter creates a fixed window rate limiter.
func NewFixedWindowLimiter(maxRequests int, windowMs int64, getNow func() int64) *FixedWindowLimiter {
	return &FixedWindowLimiter{
		maxRequests: maxRequests,
		windowMs:    windowMs,
		windowStart: getNow(),
		getNow:      getNow,
	}
}

// TryAcquire attempts to acquire a request slot.
func (fw *FixedWindowLimiter) TryAcquire() bool {
	fw.mu.Lock()
	defer fw.mu.Unlock()

	now := fw.getNow()
	fw.maybeResetWindow(now)

	if fw.requests >= fw.maxRequests {
		return false
	}

	fw.requests++
	return true
}

// RemainingRequests returns how many requests are available.
func (fw *FixedWindowLimiter) RemainingRequests() int {
	fw.mu.Lock()
	defer fw.mu.Unlock()

	fw.maybeResetWindow(fw.getNow())
	remaining := fw.maxRequests - fw.requests
	if remaining < 0 {
		return 0
	}
	return remaining
}

// TimeUntilReset returns milliseconds until the window resets.
func (fw *FixedWindowLimiter) TimeUntilReset() int64 {
	fw.mu.Lock()
	defer fw.mu.Unlock()

	now := fw.getNow()
	return (fw.windowStart + fw.windowMs) - now
}

func (fw *FixedWindowLimiter) maybeResetWindow(now int64) {
	if now >= fw.windowStart+fw.windowMs {
		fw.windowStart = now
		fw.requests = 0
	}
}

// RateLimiterType represents types of rate limiters.
type RateLimiterType int

const (
	TokenBucketType RateLimiterType = iota
	SlidingWindowType
	FixedWindowType
)

// RateLimiter is a generic interface for rate limiters.
type RateLimiter interface {
	TryAcquire() bool
}

// LeakyBucket implements the leaky bucket rate limiting algorithm.
type LeakyBucket struct {
	capacity      int
	leakRate      float64 // Items per millisecond
	queue         int
	lastLeakTime  int64
	mu            sync.Mutex
	getNow        func() int64
}

// NewLeakyBucket creates a leaky bucket rate limiter.
func NewLeakyBucket(capacity int, leakRatePerSecond float64, getNow func() int64) *LeakyBucket {
	return &LeakyBucket{
		capacity:     capacity,
		leakRate:     leakRatePerSecond / 1000.0,
		lastLeakTime: getNow(),
		getNow:       getNow,
	}
}

// TryAdd attempts to add a request to the bucket.
func (lb *LeakyBucket) TryAdd() bool {
	lb.mu.Lock()
	defer lb.mu.Unlock()

	lb.leak()

	if lb.queue >= lb.capacity {
		return false
	}

	lb.queue++
	return true
}

// QueueSize returns the current queue size.
func (lb *LeakyBucket) QueueSize() int {
	lb.mu.Lock()
	defer lb.mu.Unlock()
	lb.leak()
	return lb.queue
}

func (lb *LeakyBucket) leak() {
	now := lb.getNow()
	elapsed := now - lb.lastLeakTime
	leaked := int(float64(elapsed) * lb.leakRate)

	lb.queue -= leaked
	if lb.queue < 0 {
		lb.queue = 0
	}
	lb.lastLeakTime = now
}
