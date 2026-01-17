// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeResource - Resource handle management with RAII semantics.
 *
 * Provides type-safe resource handles that ensure proper cleanup.
 * Resources are tracked and cannot be double-freed or used after release.
 */

/** Error types for resource operations */
type resourceError =
  | ResourceAlreadyReleased
  | ResourceNotFound
  | ResourceLimitExceeded
  | InvalidHandle
  | AllocationFailed
  | AcquisitionFailed

/** Generation counter to detect stale handles */
type generation = int

/** A type-safe handle that references a managed resource */
type handle = {
  index: int,
  generation: generation,
}

/** Sentinel value for invalid handles */
let invalidHandle: handle = {
  index: -1,
  generation: 0,
}

/** Check if a handle is valid (not the sentinel) */
let isValidHandle = (h: handle): bool => {
  h.index >= 0
}

/** A slot in the resource pool */
type slot<'a> = {
  mutable resource: option<'a>,
  mutable generation: generation,
  mutable isActive: bool,
}

/** A resource pool that manages handles to resources */
type resourcePool<'a> = {
  slots: array<slot<'a>>,
  mutable freeList: array<int>,
  mutable freeCount: int,
  mutable activeCount: int,
  capacity: int,
}

/** Initialize an empty resource pool */
let createResourcePool = (capacity: int): resourcePool<'a> => {
  let cap = max(1, capacity)
  let slots = Belt.Array.makeBy(cap, _ => {
    resource: None,
    generation: 0,
    isActive: false,
  })

  // Build free list in reverse order
  let freeList = Belt.Array.makeBy(cap, i => cap - 1 - i)

  {
    slots: slots,
    freeList: freeList,
    freeCount: cap,
    activeCount: 0,
    capacity: cap,
  }
}

/** Acquire a slot and store a resource */
let acquire = (pool: resourcePool<'a>, resource: 'a): result<handle, resourceError> => {
  if pool.freeCount == 0 {
    Error(ResourceLimitExceeded)
  } else {
    pool.freeCount = pool.freeCount - 1
    let index = Belt.Array.getUnsafe(pool.freeList, pool.freeCount)

    let slot = Belt.Array.getUnsafe(pool.slots, index)
    slot.resource = Some(resource)
    slot.isActive = true
    pool.activeCount = pool.activeCount + 1

    Ok({
      index: index,
      generation: slot.generation,
    })
  }
}

/** Release a resource by handle */
let release = (pool: resourcePool<'a>, h: handle): result<'a, resourceError> => {
  if !isValidHandle(h) {
    Error(InvalidHandle)
  } else if h.index >= pool.capacity {
    Error(InvalidHandle)
  } else {
    let slot = Belt.Array.getUnsafe(pool.slots, h.index)

    if !slot.isActive {
      Error(ResourceAlreadyReleased)
    } else if slot.generation != h.generation {
      Error(ResourceAlreadyReleased)
    } else {
      switch slot.resource {
      | None => Error(ResourceAlreadyReleased)
      | Some(resource) =>
        slot.resource = None
        slot.isActive = false
        slot.generation = slot.generation + 1

        Belt.Array.setUnsafe(pool.freeList, pool.freeCount, h.index)
        pool.freeCount = pool.freeCount + 1
        pool.activeCount = pool.activeCount - 1

        Ok(resource)
      }
    }
  }
}

/** Get a reference to a resource without releasing it */
let get = (pool: resourcePool<'a>, h: handle): result<'a, resourceError> => {
  if !isValidHandle(h) {
    Error(InvalidHandle)
  } else if h.index >= pool.capacity {
    Error(InvalidHandle)
  } else {
    let slot = Belt.Array.getUnsafe(pool.slots, h.index)

    if !slot.isActive {
      Error(ResourceAlreadyReleased)
    } else if slot.generation != h.generation {
      Error(ResourceAlreadyReleased)
    } else {
      switch slot.resource {
      | None => Error(ResourceAlreadyReleased)
      | Some(resource) => Ok(resource)
      }
    }
  }
}

/** Check if a handle is still valid */
let isActive = (pool: resourcePool<'a>, h: handle): bool => {
  if !isValidHandle(h) {
    false
  } else if h.index >= pool.capacity {
    false
  } else {
    let slot = Belt.Array.getUnsafe(pool.slots, h.index)
    slot.isActive && slot.generation == h.generation
  }
}

/** Get the number of active resources */
let activeCount = (pool: resourcePool<'a>): int => {
  pool.activeCount
}

/** Get remaining capacity */
let remainingCapacity = (pool: resourcePool<'a>): int => {
  pool.freeCount
}

/** Clear all resources (does not call cleanup on resources) */
let clear = (pool: resourcePool<'a>): unit => {
  for i in 0 to pool.capacity - 1 {
    let slot = Belt.Array.getUnsafe(pool.slots, i)
    if slot.isActive {
      slot.resource = None
      slot.isActive = false
      slot.generation = slot.generation + 1
    }
    Belt.Array.setUnsafe(pool.freeList, i, pool.capacity - 1 - i)
  }
  pool.freeCount = pool.capacity
  pool.activeCount = 0
}

/** A reference-counted resource wrapper */
type refCounted<'a> = {
  value: 'a,
  mutable refCount: int,
}

/** Create a new reference-counted resource */
let createRefCounted = (value: 'a): refCounted<'a> => {
  {
    value: value,
    refCount: 1,
  }
}

/** Increment reference count */
let retain = (rc: refCounted<'a>): unit => {
  rc.refCount = rc.refCount + 1
}

/** Decrement reference count, returns true if resource should be freed */
let releaseRef = (rc: refCounted<'a>): bool => {
  if rc.refCount == 0 {
    true
  } else {
    rc.refCount = rc.refCount - 1
    rc.refCount == 0
  }
}

/** Get the current reference count */
let refCount = (rc: refCounted<'a>): int => {
  rc.refCount
}

/** Check if this is the only reference */
let isUnique = (rc: refCounted<'a>): bool => {
  rc.refCount == 1
}

/** A unique resource that cannot be copied (move-only semantics) */
type uniqueResource<'a> = {
  mutable value: option<'a>,
}

/** Create a unique resource */
let createUnique = (value: 'a): uniqueResource<'a> => {
  {value: Some(value)}
}

/** Create an empty unique resource */
let createUniqueEmpty = (): uniqueResource<'a> => {
  {value: None}
}

/** Check if the resource is present */
let hasValue = (unique: uniqueResource<'a>): bool => {
  Belt.Option.isSome(unique.value)
}

/** Get a reference to the value */
let getValue = (unique: uniqueResource<'a>): result<'a, resourceError> => {
  switch unique.value {
  | Some(v) => Ok(v)
  | None => Error(ResourceAlreadyReleased)
  }
}

/** Take ownership of the value, leaving this empty */
let take = (unique: uniqueResource<'a>): result<'a, resourceError> => {
  switch unique.value {
  | Some(v) =>
    unique.value = None
    Ok(v)
  | None => Error(ResourceAlreadyReleased)
  }
}

/** Replace the value, returning the old one */
let replace = (unique: uniqueResource<'a>, newValue: 'a): option<'a> => {
  let old = unique.value
  unique.value = Some(newValue)
  old
}

/** Clear the value */
let clearUnique = (unique: uniqueResource<'a>): unit => {
  unique.value = None
}

/** Lease-based resource that expires after a deadline */
type leasedResource<'a> = {
  value: 'a,
  leaseStart: float,
  leaseDuration: float,
}

/** Create a leased resource */
let createLeased = (value: 'a, currentTime: float, duration: float): leasedResource<'a> => {
  {
    value: value,
    leaseStart: currentTime,
    leaseDuration: duration,
  }
}

/** Check if the lease has expired */
let isExpired = (leased: leasedResource<'a>, currentTime: float): bool => {
  currentTime >= leased.leaseStart +. leased.leaseDuration
}

/** Get remaining lease time */
let remainingTime = (leased: leasedResource<'a>, currentTime: float): float => {
  let expiry = leased.leaseStart +. leased.leaseDuration
  if currentTime >= expiry {
    0.0
  } else {
    expiry -. currentTime
  }
}

/** Create a new leased resource with renewed lease */
let renew = (leased: leasedResource<'a>, currentTime: float, newDuration: float): leasedResource<
  'a,
> => {
  {
    value: leased.value,
    leaseStart: currentTime,
    leaseDuration: newDuration,
  }
}

/** Get the value if lease is valid */
let getLeasedValue = (leased: leasedResource<'a>, currentTime: float): result<'a, resourceError> => {
  if isExpired(leased, currentTime) {
    Error(ResourceAlreadyReleased)
  } else {
    Ok(leased.value)
  }
}

/** Execute a function with a resource, ensuring cleanup */
let withResource = (
  pool: resourcePool<'a>,
  resource: 'a,
  f: 'a => 'b,
): result<'b, resourceError> => {
  switch acquire(pool, resource) {
  | Error(e) => Error(e)
  | Ok(h) =>
    switch get(pool, h) {
    | Error(e) =>
      let _ = release(pool, h)
      Error(e)
    | Ok(r) =>
      let result = f(r)
      let _ = release(pool, h)
      Ok(result)
    }
  }
}

/** Map over a resource if the handle is valid */
let mapResource = (
  pool: resourcePool<'a>,
  h: handle,
  f: 'a => 'b,
): result<'b, resourceError> => {
  switch get(pool, h) {
  | Error(e) => Error(e)
  | Ok(r) => Ok(f(r))
  }
}
