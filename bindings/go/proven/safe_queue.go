// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

// BoundedQueue is a bounded FIFO queue.
type BoundedQueue[T any] struct {
	data     []T
	capacity int
}

// NewBoundedQueue creates a bounded queue.
func NewBoundedQueue[T any](capacity int) *BoundedQueue[T] {
	if capacity < 1 {
		capacity = 1
	}
	return &BoundedQueue[T]{
		data:     make([]T, 0, capacity),
		capacity: capacity,
	}
}

// Len returns current length.
func (q *BoundedQueue[T]) Len() int {
	return len(q.data)
}

// Cap returns capacity.
func (q *BoundedQueue[T]) Cap() int {
	return q.capacity
}

// IsFull checks if queue is full.
func (q *BoundedQueue[T]) IsFull() bool {
	return len(q.data) >= q.capacity
}

// IsEmpty checks if queue is empty.
func (q *BoundedQueue[T]) IsEmpty() bool {
	return len(q.data) == 0
}

// Enqueue adds an item (fails if full).
func (q *BoundedQueue[T]) Enqueue(item T) bool {
	if q.IsFull() {
		return false
	}
	q.data = append(q.data, item)
	return true
}

// Dequeue removes and returns the first item.
func (q *BoundedQueue[T]) Dequeue() (T, bool) {
	var zero T
	if q.IsEmpty() {
		return zero, false
	}
	item := q.data[0]
	q.data = q.data[1:]
	return item, true
}

// Peek returns the first item without removing.
func (q *BoundedQueue[T]) Peek() (T, bool) {
	var zero T
	if q.IsEmpty() {
		return zero, false
	}
	return q.data[0], true
}

// Clear clears the queue.
func (q *BoundedQueue[T]) Clear() {
	q.data = q.data[:0]
}

// ToSlice returns a copy of the data.
func (q *BoundedQueue[T]) ToSlice() []T {
	result := make([]T, len(q.data))
	copy(result, q.data)
	return result
}

// PriorityQueue is a min-heap priority queue.
type PriorityQueue[T any] struct {
	heap       []T
	comparator func(a, b T) int
	capacity   int
}

// NewPriorityQueue creates a priority queue.
func NewPriorityQueue[T any](capacity int, comparator func(a, b T) int) *PriorityQueue[T] {
	if capacity < 1 {
		capacity = 1
	}
	return &PriorityQueue[T]{
		heap:       make([]T, 0, capacity),
		comparator: comparator,
		capacity:   capacity,
	}
}

// Len returns current length.
func (pq *PriorityQueue[T]) Len() int {
	return len(pq.heap)
}

// Cap returns capacity.
func (pq *PriorityQueue[T]) Cap() int {
	return pq.capacity
}

// IsFull checks if queue is full.
func (pq *PriorityQueue[T]) IsFull() bool {
	return len(pq.heap) >= pq.capacity
}

// IsEmpty checks if queue is empty.
func (pq *PriorityQueue[T]) IsEmpty() bool {
	return len(pq.heap) == 0
}

// Push adds an item (fails if full).
func (pq *PriorityQueue[T]) Push(item T) bool {
	if pq.IsFull() {
		return false
	}

	pq.heap = append(pq.heap, item)
	pq.bubbleUp(len(pq.heap) - 1)
	return true
}

// Pop removes and returns the highest priority item.
func (pq *PriorityQueue[T]) Pop() (T, bool) {
	var zero T
	if pq.IsEmpty() {
		return zero, false
	}

	top := pq.heap[0]
	last := len(pq.heap) - 1
	pq.heap[0] = pq.heap[last]
	pq.heap = pq.heap[:last]

	if len(pq.heap) > 0 {
		pq.bubbleDown(0)
	}

	return top, true
}

// Peek returns the highest priority item without removing.
func (pq *PriorityQueue[T]) Peek() (T, bool) {
	var zero T
	if pq.IsEmpty() {
		return zero, false
	}
	return pq.heap[0], true
}

// Clear clears the queue.
func (pq *PriorityQueue[T]) Clear() {
	pq.heap = pq.heap[:0]
}

// ToSlice returns items (not in priority order).
func (pq *PriorityQueue[T]) ToSlice() []T {
	result := make([]T, len(pq.heap))
	copy(result, pq.heap)
	return result
}

func (pq *PriorityQueue[T]) bubbleUp(index int) {
	for index > 0 {
		parent := (index - 1) / 2
		if pq.comparator(pq.heap[index], pq.heap[parent]) >= 0 {
			break
		}
		pq.heap[index], pq.heap[parent] = pq.heap[parent], pq.heap[index]
		index = parent
	}
}

func (pq *PriorityQueue[T]) bubbleDown(index int) {
	length := len(pq.heap)
	for {
		left := 2*index + 1
		right := 2*index + 2
		smallest := index

		if left < length && pq.comparator(pq.heap[left], pq.heap[smallest]) < 0 {
			smallest = left
		}
		if right < length && pq.comparator(pq.heap[right], pq.heap[smallest]) < 0 {
			smallest = right
		}

		if smallest == index {
			break
		}

		pq.heap[index], pq.heap[smallest] = pq.heap[smallest], pq.heap[index]
		index = smallest
	}
}

// IntPriorityQueue creates a min-heap for integers.
func IntPriorityQueue(capacity int) *PriorityQueue[int] {
	return NewPriorityQueue[int](capacity, func(a, b int) int {
		if a < b {
			return -1
		}
		if a > b {
			return 1
		}
		return 0
	})
}

// MaxIntPriorityQueue creates a max-heap for integers.
func MaxIntPriorityQueue(capacity int) *PriorityQueue[int] {
	return NewPriorityQueue[int](capacity, func(a, b int) int {
		if a > b {
			return -1
		}
		if a < b {
			return 1
		}
		return 0
	})
}
