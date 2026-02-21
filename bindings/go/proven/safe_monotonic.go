// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"sync"
	"sync/atomic"
)

// MonotonicCounter is an atomically increasing counter that never decreases.
type MonotonicCounter struct {
	value int64
}

// NewMonotonicCounter creates a monotonic counter starting at 0.
func NewMonotonicCounter() *MonotonicCounter {
	return &MonotonicCounter{value: 0}
}

// NewMonotonicCounterFrom creates a monotonic counter with initial value.
func NewMonotonicCounterFrom(initial int64) *MonotonicCounter {
	return &MonotonicCounter{value: initial}
}

// Increment atomically increments and returns the new value.
func (c *MonotonicCounter) Increment() int64 {
	return atomic.AddInt64(&c.value, 1)
}

// IncrementBy atomically increments by n and returns the new value.
// If n is negative, the counter is unchanged.
func (c *MonotonicCounter) IncrementBy(n int64) int64 {
	if n <= 0 {
		return atomic.LoadInt64(&c.value)
	}
	return atomic.AddInt64(&c.value, n)
}

// Value returns the current value.
func (c *MonotonicCounter) Value() int64 {
	return atomic.LoadInt64(&c.value)
}

// TrySet sets the value only if newValue > current value.
// Returns true if the value was updated.
func (c *MonotonicCounter) TrySet(newValue int64) bool {
	for {
		current := atomic.LoadInt64(&c.value)
		if newValue <= current {
			return false
		}
		if atomic.CompareAndSwapInt64(&c.value, current, newValue) {
			return true
		}
	}
}

// MonotonicTimestamp ensures timestamps never go backwards.
type MonotonicTimestamp struct {
	lastValue int64
	mu        sync.Mutex
	getNow    func() int64
}

// NewMonotonicTimestamp creates a monotonic timestamp provider.
func NewMonotonicTimestamp(getNow func() int64) *MonotonicTimestamp {
	return &MonotonicTimestamp{
		getNow: getNow,
	}
}

// Now returns the current time, ensuring it never goes backwards.
func (m *MonotonicTimestamp) Now() int64 {
	m.mu.Lock()
	defer m.mu.Unlock()

	current := m.getNow()
	if current <= m.lastValue {
		m.lastValue++
		return m.lastValue
	}
	m.lastValue = current
	return current
}

// Last returns the last timestamp that was returned.
func (m *MonotonicTimestamp) Last() int64 {
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.lastValue
}

// MonotonicID generates monotonically increasing unique IDs.
type MonotonicID struct {
	counter   *MonotonicCounter
	timestamp *MonotonicTimestamp
}

// NewMonotonicID creates a monotonic ID generator.
func NewMonotonicID(getNow func() int64) *MonotonicID {
	return &MonotonicID{
		counter:   NewMonotonicCounter(),
		timestamp: NewMonotonicTimestamp(getNow),
	}
}

// NextID returns the next monotonically increasing ID.
// Format: timestamp in ms << 20 | counter (20 bits for sequence)
func (m *MonotonicID) NextID() int64 {
	ts := m.timestamp.Now()
	seq := m.counter.Increment() & 0xFFFFF // 20 bits
	return (ts << 20) | seq
}

// MonotonicSequence tracks a sequence of values, ensuring monotonicity.
type MonotonicSequence struct {
	values   []int64
	maxValue int64
	mu       sync.Mutex
}

// NewMonotonicSequence creates a monotonic sequence tracker.
func NewMonotonicSequence() *MonotonicSequence {
	return &MonotonicSequence{
		values: make([]int64, 0),
	}
}

// TryAppend adds a value only if it's greater than all previous values.
func (s *MonotonicSequence) TryAppend(value int64) bool {
	s.mu.Lock()
	defer s.mu.Unlock()

	if len(s.values) > 0 && value <= s.maxValue {
		return false
	}

	s.values = append(s.values, value)
	s.maxValue = value
	return true
}

// Values returns a copy of all values.
func (s *MonotonicSequence) Values() []int64 {
	s.mu.Lock()
	defer s.mu.Unlock()

	result := make([]int64, len(s.values))
	copy(result, s.values)
	return result
}

// Len returns the number of values.
func (s *MonotonicSequence) Len() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return len(s.values)
}

// Max returns the maximum (most recent) value.
func (s *MonotonicSequence) Max() (int64, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()

	if len(s.values) == 0 {
		return 0, false
	}
	return s.maxValue, true
}

// Clear resets the sequence.
func (s *MonotonicSequence) Clear() {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.values = make([]int64, 0)
	s.maxValue = 0
}

// MonotonicVersion represents a monotonically increasing version.
type MonotonicVersion struct {
	major int64
	minor int64
	patch int64
	mu    sync.Mutex
}

// NewMonotonicVersion creates a version starting at 0.0.0.
func NewMonotonicVersion() *MonotonicVersion {
	return &MonotonicVersion{}
}

// NewMonotonicVersionFrom creates a version with initial values.
func NewMonotonicVersionFrom(major, minor, patch int64) *MonotonicVersion {
	return &MonotonicVersion{
		major: major,
		minor: minor,
		patch: patch,
	}
}

// BumpMajor increments major, resets minor and patch.
func (v *MonotonicVersion) BumpMajor() (int64, int64, int64) {
	v.mu.Lock()
	defer v.mu.Unlock()
	v.major++
	v.minor = 0
	v.patch = 0
	return v.major, v.minor, v.patch
}

// BumpMinor increments minor, resets patch.
func (v *MonotonicVersion) BumpMinor() (int64, int64, int64) {
	v.mu.Lock()
	defer v.mu.Unlock()
	v.minor++
	v.patch = 0
	return v.major, v.minor, v.patch
}

// BumpPatch increments patch.
func (v *MonotonicVersion) BumpPatch() (int64, int64, int64) {
	v.mu.Lock()
	defer v.mu.Unlock()
	v.patch++
	return v.major, v.minor, v.patch
}

// Version returns the current version.
func (v *MonotonicVersion) Version() (int64, int64, int64) {
	v.mu.Lock()
	defer v.mu.Unlock()
	return v.major, v.minor, v.patch
}

// TrySet sets the version only if newVersion > current version.
func (v *MonotonicVersion) TrySet(major, minor, patch int64) bool {
	v.mu.Lock()
	defer v.mu.Unlock()

	if major < v.major {
		return false
	}
	if major == v.major && minor < v.minor {
		return false
	}
	if major == v.major && minor == v.minor && patch <= v.patch {
		return false
	}

	v.major = major
	v.minor = minor
	v.patch = patch
	return true
}

// HighWaterMark tracks the highest seen value.
type HighWaterMark struct {
	value int64
}

// NewHighWaterMark creates a high water mark tracker.
func NewHighWaterMark() *HighWaterMark {
	return &HighWaterMark{}
}

// Update updates the mark if the new value is higher.
// Returns true if the value was updated.
func (h *HighWaterMark) Update(value int64) bool {
	for {
		current := atomic.LoadInt64(&h.value)
		if value <= current {
			return false
		}
		if atomic.CompareAndSwapInt64(&h.value, current, value) {
			return true
		}
	}
}

// Value returns the current high water mark.
func (h *HighWaterMark) Value() int64 {
	return atomic.LoadInt64(&h.value)
}
