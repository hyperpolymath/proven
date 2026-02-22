// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Bounded FIFO queue delegated to libproven FFI.
///
/// Queue operations with overflow protection via the formally verified
/// Idris 2 core.

import CProven

/// Bounded queue backed by libproven.
/// This class manages the lifecycle of a C-allocated queue.
public final class SafeQueue {
    private let queue: UnsafeMutablePointer<ProvenBoundedQueue>

    /// Create a bounded queue with the given capacity.
    /// Returns nil if allocation fails.
    public init?(capacity: Int) {
        guard let ptr = proven_queue_create(capacity) else {
            return nil
        }
        self.queue = ptr
    }

    deinit {
        proven_queue_free(queue)
    }

    /// Push a value onto the queue.
    /// Returns true if successful, false if queue is full.
    @discardableResult
    public func push(_ value: Int64) -> Bool {
        proven_queue_push(queue, value)
    }

    /// Pop a value from the queue (FIFO).
    public func pop() -> Result<Int64, ProvenError> {
        let result = proven_queue_pop(queue)
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Get the current number of elements.
    public var count: Int {
        proven_queue_size(queue)
    }
}
