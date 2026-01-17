// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

// BoundedBuffer is a bounded stack-like buffer.
type BoundedBuffer[T any] struct {
	data     []T
	capacity int
}

// NewBoundedBuffer creates a bounded buffer.
func NewBoundedBuffer[T any](capacity int) *BoundedBuffer[T] {
	if capacity < 1 {
		capacity = 1
	}
	return &BoundedBuffer[T]{
		data:     make([]T, 0, capacity),
		capacity: capacity,
	}
}

// Len returns current length.
func (b *BoundedBuffer[T]) Len() int {
	return len(b.data)
}

// Cap returns capacity.
func (b *BoundedBuffer[T]) Cap() int {
	return b.capacity
}

// IsFull checks if buffer is full.
func (b *BoundedBuffer[T]) IsFull() bool {
	return len(b.data) >= b.capacity
}

// IsEmpty checks if buffer is empty.
func (b *BoundedBuffer[T]) IsEmpty() bool {
	return len(b.data) == 0
}

// Push adds an item (fails if full).
func (b *BoundedBuffer[T]) Push(item T) bool {
	if b.IsFull() {
		return false
	}
	b.data = append(b.data, item)
	return true
}

// Pop removes and returns the last item.
func (b *BoundedBuffer[T]) Pop() (T, bool) {
	var zero T
	if b.IsEmpty() {
		return zero, false
	}
	item := b.data[len(b.data)-1]
	b.data = b.data[:len(b.data)-1]
	return item, true
}

// Peek returns the last item without removing.
func (b *BoundedBuffer[T]) Peek() (T, bool) {
	var zero T
	if b.IsEmpty() {
		return zero, false
	}
	return b.data[len(b.data)-1], true
}

// Get returns item at index.
func (b *BoundedBuffer[T]) Get(index int) (T, bool) {
	var zero T
	if index < 0 || index >= len(b.data) {
		return zero, false
	}
	return b.data[index], true
}

// Clear clears the buffer.
func (b *BoundedBuffer[T]) Clear() {
	b.data = b.data[:0]
}

// ToSlice returns a copy of the data.
func (b *BoundedBuffer[T]) ToSlice() []T {
	result := make([]T, len(b.data))
	copy(result, b.data)
	return result
}

// Remaining returns remaining capacity.
func (b *BoundedBuffer[T]) Remaining() int {
	return b.capacity - len(b.data)
}

// RingBuffer is a circular buffer.
type RingBuffer[T any] struct {
	data     []T
	capacity int
	head     int
	tail     int
	length   int
}

// NewRingBuffer creates a ring buffer.
func NewRingBuffer[T any](capacity int) *RingBuffer[T] {
	if capacity < 1 {
		capacity = 1
	}
	return &RingBuffer[T]{
		data:     make([]T, capacity),
		capacity: capacity,
	}
}

// Len returns current length.
func (r *RingBuffer[T]) Len() int {
	return r.length
}

// Cap returns capacity.
func (r *RingBuffer[T]) Cap() int {
	return r.capacity
}

// IsFull checks if buffer is full.
func (r *RingBuffer[T]) IsFull() bool {
	return r.length >= r.capacity
}

// IsEmpty checks if buffer is empty.
func (r *RingBuffer[T]) IsEmpty() bool {
	return r.length == 0
}

// Push adds an item (overwrites oldest if full).
func (r *RingBuffer[T]) Push(item T) (T, bool) {
	var overwritten T
	var wasOverwritten bool

	if r.IsFull() {
		overwritten = r.data[r.head]
		wasOverwritten = true
		r.head = (r.head + 1) % r.capacity
	} else {
		r.length++
	}

	r.data[r.tail] = item
	r.tail = (r.tail + 1) % r.capacity

	return overwritten, wasOverwritten
}

// Pop removes and returns the oldest item (FIFO).
func (r *RingBuffer[T]) Pop() (T, bool) {
	var zero T
	if r.IsEmpty() {
		return zero, false
	}

	item := r.data[r.head]
	r.head = (r.head + 1) % r.capacity
	r.length--

	return item, true
}

// Peek returns the oldest item without removing.
func (r *RingBuffer[T]) Peek() (T, bool) {
	var zero T
	if r.IsEmpty() {
		return zero, false
	}
	return r.data[r.head], true
}

// PeekNewest returns the newest item without removing.
func (r *RingBuffer[T]) PeekNewest() (T, bool) {
	var zero T
	if r.IsEmpty() {
		return zero, false
	}
	index := (r.tail - 1 + r.capacity) % r.capacity
	return r.data[index], true
}

// Get returns item at logical index (0 = oldest).
func (r *RingBuffer[T]) Get(index int) (T, bool) {
	var zero T
	if index < 0 || index >= r.length {
		return zero, false
	}
	actualIndex := (r.head + index) % r.capacity
	return r.data[actualIndex], true
}

// Clear clears the buffer.
func (r *RingBuffer[T]) Clear() {
	r.head = 0
	r.tail = 0
	r.length = 0
}

// ToSlice returns items in order (oldest first).
func (r *RingBuffer[T]) ToSlice() []T {
	result := make([]T, r.length)
	for i := 0; i < r.length; i++ {
		actualIndex := (r.head + i) % r.capacity
		result[i] = r.data[actualIndex]
	}
	return result
}
