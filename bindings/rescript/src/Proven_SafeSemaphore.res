// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeSemaphore - Counting semaphore operations that cannot crash.
 *
 * Provides synchronization primitives for resource management with
 * guaranteed bounds checking. Useful for limiting concurrent access
 * to resources, implementing connection pools, and rate limiting.
 */

/** Error types for semaphore operations */
type semaphoreError =
  | NoPermitsAvailable
  | TooManyPermits
  | InvalidPermitCount
  | WouldOverflow
  | AlreadyReleased

/** Result of a try-acquire operation */
type acquireResult =
  | Acquired
  | Unavailable(int) // Number of permits needed

/** Statistics about semaphore usage */
type semaphoreStats = {
  available: int,
  max: int,
  held: int,
  totalAcquired: int,
  totalReleased: int,
}

/** Counting semaphore with a maximum permit limit */
type countingSemaphore = {
  mutable available: int,
  max: int,
  mutable totalAcquired: int,
  mutable totalReleased: int,
}

/** Create a counting semaphore with initial permits */
let createCountingSemaphore = (maxPermits: int, initialPermits: int): countingSemaphore => {
  let initial = min(initialPermits, maxPermits)
  {
    available: max(0, initial),
    max: max(0, maxPermits),
    totalAcquired: 0,
    totalReleased: 0,
  }
}

/** Create a full semaphore (all permits available) */
let createFull = (maxPermits: int): countingSemaphore => {
  createCountingSemaphore(maxPermits, maxPermits)
}

/** Create an empty semaphore (no permits available) */
let createEmpty = (maxPermits: int): countingSemaphore => {
  createCountingSemaphore(maxPermits, 0)
}

/** Get the number of currently available permits */
let availablePermits = (sem: countingSemaphore): int => {
  sem.available
}

/** Get the maximum number of permits */
let maxPermits = (sem: countingSemaphore): int => {
  sem.max
}

/** Check if any permits are available */
let hasAvailable = (sem: countingSemaphore): bool => {
  sem.available > 0
}

/** Check if all permits are available */
let isFull = (sem: countingSemaphore): bool => {
  sem.available == sem.max
}

/** Check if no permits are available */
let isEmpty = (sem: countingSemaphore): bool => {
  sem.available == 0
}

/** Get the number of permits currently held */
let heldPermits = (sem: countingSemaphore): int => {
  sem.max - sem.available
}

/** Try to acquire a single permit without blocking */
let tryAcquire = (sem: countingSemaphore): result<unit, semaphoreError> => {
  if sem.available < 1 {
    Error(NoPermitsAvailable)
  } else {
    sem.available = sem.available - 1
    sem.totalAcquired = sem.totalAcquired + 1
    Ok()
  }
}

/** Try to acquire multiple permits without blocking */
let tryAcquireMany = (sem: countingSemaphore, count: int): result<unit, semaphoreError> => {
  if count <= 0 {
    Error(InvalidPermitCount)
  } else if count > sem.max {
    Error(TooManyPermits)
  } else if sem.available < count {
    Error(NoPermitsAvailable)
  } else {
    sem.available = sem.available - count
    sem.totalAcquired = sem.totalAcquired + count
    Ok()
  }
}

/** Try to acquire, returning a result variant */
let tryAcquireResult = (sem: countingSemaphore): acquireResult => {
  if sem.available < 1 {
    Unavailable(1)
  } else {
    sem.available = sem.available - 1
    sem.totalAcquired = sem.totalAcquired + 1
    Acquired
  }
}

/** Try to acquire many, returning a result variant */
let tryAcquireManyResult = (sem: countingSemaphore, count: int): acquireResult => {
  if count <= 0 || count > sem.max {
    Unavailable(count)
  } else if sem.available < count {
    Unavailable(count - sem.available)
  } else {
    sem.available = sem.available - count
    sem.totalAcquired = sem.totalAcquired + count
    Acquired
  }
}

/** Release a single permit */
let release = (sem: countingSemaphore): result<unit, semaphoreError> => {
  if sem.available >= sem.max {
    Error(WouldOverflow)
  } else {
    sem.available = sem.available + 1
    sem.totalReleased = sem.totalReleased + 1
    Ok()
  }
}

/** Release multiple permits */
let releaseMany = (sem: countingSemaphore, count: int): result<unit, semaphoreError> => {
  if count <= 0 {
    Error(InvalidPermitCount)
  } else if sem.available + count > sem.max {
    Error(WouldOverflow)
  } else {
    sem.available = sem.available + count
    sem.totalReleased = sem.totalReleased + count
    Ok()
  }
}

/** Force release (saturating - won't exceed max) */
let releaseSaturating = (sem: countingSemaphore, count: int): unit => {
  let newAvailable = sem.available + count
  let actualRelease = min(count, sem.max - sem.available)
  sem.available = min(newAvailable, sem.max)
  sem.totalReleased = sem.totalReleased + actualRelease
}

/** Drain all available permits, returning count acquired */
let drainAll = (sem: countingSemaphore): int => {
  let drained = sem.available
  sem.totalAcquired = sem.totalAcquired + drained
  sem.available = 0
  drained
}

/** Reset to initial state with given permits */
let reset = (sem: countingSemaphore, permits: int): unit => {
  sem.available = min(max(0, permits), sem.max)
  sem.totalAcquired = 0
  sem.totalReleased = 0
}

/** Get statistics about semaphore usage */
let stats = (sem: countingSemaphore): semaphoreStats => {
  {
    available: sem.available,
    max: sem.max,
    held: sem.max - sem.available,
    totalAcquired: sem.totalAcquired,
    totalReleased: sem.totalReleased,
  }
}

/** Calculate utilization percentage */
let utilizationPercent = (s: semaphoreStats): float => {
  if s.max == 0 {
    0.0
  } else {
    Belt.Int.toFloat(s.held) /. Belt.Int.toFloat(s.max) *. 100.0
  }
}

/** Binary semaphore (mutex-like, 0 or 1 permits) */
type binarySemaphore = {
  mutable locked: bool,
  mutable lockCount: int,
  mutable unlockCount: int,
}

/** Create a binary semaphore */
let createBinarySemaphore = (): binarySemaphore => {
  {
    locked: false,
    lockCount: 0,
    unlockCount: 0,
  }
}

/** Create a binary semaphore in locked state */
let createBinarySemaphoreLocked = (): binarySemaphore => {
  {
    locked: true,
    lockCount: 0,
    unlockCount: 0,
  }
}

/** Check if binary semaphore is locked */
let isLocked = (sem: binarySemaphore): bool => {
  sem.locked
}

/** Check if binary semaphore is unlocked */
let isUnlocked = (sem: binarySemaphore): bool => {
  !sem.locked
}

/** Try to lock binary semaphore */
let tryLock = (sem: binarySemaphore): result<unit, semaphoreError> => {
  if sem.locked {
    Error(NoPermitsAvailable)
  } else {
    sem.locked = true
    sem.lockCount = sem.lockCount + 1
    Ok()
  }
}

/** Try to lock binary semaphore, returning bool */
let tryLockResult = (sem: binarySemaphore): bool => {
  if sem.locked {
    false
  } else {
    sem.locked = true
    sem.lockCount = sem.lockCount + 1
    true
  }
}

/** Unlock binary semaphore */
let unlock = (sem: binarySemaphore): result<unit, semaphoreError> => {
  if !sem.locked {
    Error(AlreadyReleased)
  } else {
    sem.locked = false
    sem.unlockCount = sem.unlockCount + 1
    Ok()
  }
}

/** Force unlock binary semaphore */
let forceUnlock = (sem: binarySemaphore): unit => {
  if sem.locked {
    sem.locked = false
    sem.unlockCount = sem.unlockCount + 1
  }
}

/** Reset binary semaphore */
let resetBinary = (sem: binarySemaphore): unit => {
  sem.locked = false
  sem.lockCount = 0
  sem.unlockCount = 0
}

/** Weighted semaphore where different resources have different costs */
type weightedSemaphore = {
  mutable availableWeight: int,
  maxWeight: int,
}

/** Create a weighted semaphore */
let createWeightedSemaphore = (maxWeight: int, initialWeight: int): weightedSemaphore => {
  let mw = max(0, maxWeight)
  {
    availableWeight: min(max(0, initialWeight), mw),
    maxWeight: mw,
  }
}

/** Create a full weighted semaphore */
let createWeightedSemaphoreFull = (maxWeight: int): weightedSemaphore => {
  createWeightedSemaphore(maxWeight, maxWeight)
}

/** Try to acquire weight */
let tryAcquireWeight = (sem: weightedSemaphore, weight: int): result<unit, semaphoreError> => {
  if weight <= 0 {
    Error(InvalidPermitCount)
  } else if weight > sem.maxWeight {
    Error(TooManyPermits)
  } else if sem.availableWeight < weight {
    Error(NoPermitsAvailable)
  } else {
    sem.availableWeight = sem.availableWeight - weight
    Ok()
  }
}

/** Release weight back to the semaphore */
let releaseWeight = (sem: weightedSemaphore, weight: int): result<unit, semaphoreError> => {
  if weight <= 0 {
    Error(InvalidPermitCount)
  } else if sem.availableWeight + weight > sem.maxWeight {
    Error(WouldOverflow)
  } else {
    sem.availableWeight = sem.availableWeight + weight
    Ok()
  }
}

/** Get available weight */
let getAvailableWeight = (sem: weightedSemaphore): int => {
  sem.availableWeight
}

/** Check if a given weight can be acquired */
let canAcquireWeight = (sem: weightedSemaphore, weight: int): bool => {
  weight <= sem.availableWeight
}
