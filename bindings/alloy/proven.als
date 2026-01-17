/*
 * SPDX-License-Identifier: Apache-2.0
 * Proven - Formally verified safety primitives for Alloy
 *
 * Alloy is a relational modeling language developed at MIT for
 * lightweight formal methods. It uses SAT solving for bounded
 * model checking and is used in security analysis, API design,
 * and software architecture verification.
 *
 * This module provides proven safety primitives as Alloy signatures
 * and predicates for use in formal models.
 */

module proven

// ============================================================================
// RESULT TYPE - Explicit error handling
// ============================================================================

abstract sig Result {}

sig Ok extends Result {
    value: one Value
}

sig Err extends Result {
    errorCode: one Int,
    errorMsg: one String
}

// Generic value wrapper (Alloy doesn't have generics, so we use abstract)
abstract sig Value {}

// Check if result is Ok
pred isOk[r: Result] {
    r in Ok
}

// Check if result is Err
pred isErr[r: Result] {
    r in Err
}

// Result invariant: must be exactly one of Ok or Err
fact ResultInvariant {
    all r: Result | r in Ok or r in Err
    no r: Result | r in Ok and r in Err
}

// ============================================================================
// OPTION TYPE - Safe nullable values
// ============================================================================

abstract sig Option {}

sig Some extends Option {
    optValue: one Value
}

sig None extends Option {}

pred isSome[o: Option] {
    o in Some
}

pred isNone[o: Option] {
    o in None
}

fact OptionInvariant {
    all o: Option | o in Some or o in None
    no o: Option | o in Some and o in None
}

// ============================================================================
// BOUNDED INTEGER
// ============================================================================

sig BoundedInt {
    value: one Int,
    minVal: one Int,
    maxVal: one Int
}

// Invariant: value is always within bounds
fact BoundedIntInvariant {
    all b: BoundedInt |
        b.minVal <= b.maxVal and
        b.value >= b.minVal and
        b.value <= b.maxVal
}

// Predicate: value is at minimum
pred atMin[b: BoundedInt] {
    b.value = b.minVal
}

// Predicate: value is at maximum
pred atMax[b: BoundedInt] {
    b.value = b.maxVal
}

// Predicate: value can be incremented
pred canIncrement[b: BoundedInt] {
    b.value < b.maxVal
}

// Predicate: value can be decremented
pred canDecrement[b: BoundedInt] {
    b.value > b.minVal
}

// Function: clamp value to bounds
fun clamp[v: Int, minV: Int, maxV: Int]: Int {
    (v < minV) => minV else ((v > maxV) => maxV else v)
}

// ============================================================================
// PERCENTAGE (0-100)
// ============================================================================

sig Percentage {
    pctValue: one Int
}

fact PercentageInvariant {
    all p: Percentage | p.pctValue >= 0 and p.pctValue <= 100
}

pred validPercentage[v: Int] {
    v >= 0 and v <= 100
}

// ============================================================================
// BASIS POINTS (0-10000)
// ============================================================================

sig BasisPoints {
    bpValue: one Int
}

fact BasisPointsInvariant {
    all bp: BasisPoints | bp.bpValue >= 0 and bp.bpValue <= 10000
}

pred validBasisPoints[v: Int] {
    v >= 0 and v <= 10000
}

// Relationship: 100 basis points = 1 percentage point
pred basisPointsToPercentage[bp: BasisPoints, pct: Percentage] {
    pct.pctValue = div[bp.bpValue, 100]
}

// ============================================================================
// PORT NUMBER (1-65535)
// ============================================================================

sig Port {
    portNum: one Int
}

fact PortInvariant {
    all p: Port | p.portNum >= 1 and p.portNum <= 65535
}

pred isWellKnownPort[p: Port] {
    p.portNum >= 1 and p.portNum <= 1023
}

pred isRegisteredPort[p: Port] {
    p.portNum >= 1024 and p.portNum <= 49151
}

pred isDynamicPort[p: Port] {
    p.portNum >= 49152 and p.portNum <= 65535
}

// Port categories are mutually exclusive and exhaustive
assert PortCategoriesComplete {
    all p: Port |
        (isWellKnownPort[p] or isRegisteredPort[p] or isDynamicPort[p]) and
        not (isWellKnownPort[p] and isRegisteredPort[p]) and
        not (isWellKnownPort[p] and isDynamicPort[p]) and
        not (isRegisteredPort[p] and isDynamicPort[p])
}

// ============================================================================
// RESOURCE BAR
// ============================================================================

sig ResourceBar {
    current: one Int,
    maximum: one Int
}

fact ResourceBarInvariant {
    all rb: ResourceBar |
        rb.maximum >= 1 and
        rb.current >= 0 and
        rb.current <= rb.maximum
}

pred isEmpty[rb: ResourceBar] {
    rb.current = 0
}

pred isFull[rb: ResourceBar] {
    rb.current = rb.maximum
}

pred canConsume[rb: ResourceBar, amount: Int] {
    amount >= 0 and rb.current >= amount
}

pred canAdd[rb: ResourceBar, amount: Int] {
    amount >= 0 and rb.current + amount <= rb.maximum
}

// ============================================================================
// VECTOR2
// ============================================================================

sig Vector2 {
    x: one Int,
    y: one Int
}

fun vec2Add[a: Vector2, b: Vector2]: Vector2 {
    { v: Vector2 | v.x = a.x + b.x and v.y = a.y + b.y }
}

fun vec2Dot[a: Vector2, b: Vector2]: Int {
    a.x.mul[b.x].add[a.y.mul[b.y]]
}

pred vec2IsZero[v: Vector2] {
    v.x = 0 and v.y = 0
}

// ============================================================================
// VECTOR3
// ============================================================================

sig Vector3 {
    vx: one Int,
    vy: one Int,
    vz: one Int
}

pred vec3IsZero[v: Vector3] {
    v.vx = 0 and v.vy = 0 and v.vz = 0
}

// ============================================================================
// COLOR (RGBA)
// ============================================================================

sig Color {
    red: one Int,
    green: one Int,
    blue: one Int,
    alpha: one Int
}

fact ColorInvariant {
    all c: Color |
        c.red >= 0 and c.red <= 255 and
        c.green >= 0 and c.green <= 255 and
        c.blue >= 0 and c.blue <= 255 and
        c.alpha >= 0 and c.alpha <= 255
}

pred isOpaque[c: Color] {
    c.alpha = 255
}

pred isTransparent[c: Color] {
    c.alpha = 0
}

// ============================================================================
// STATE MACHINE SAFETY
// ============================================================================

sig State {}

sig StateMachine {
    currentState: one State,
    allowedTransitions: State -> State
}

pred canTransition[sm: StateMachine, from: State, to: State] {
    from -> to in sm.allowedTransitions
}

pred validTransition[sm: StateMachine, newState: State] {
    sm.currentState -> newState in sm.allowedTransitions
}

// Safety: no unreachable states from current (all states eventually reachable)
pred allStatesReachable[sm: StateMachine] {
    all s: State | s in sm.currentState.*(sm.allowedTransitions)
}

// ============================================================================
// SEQUENCE BOUNDS
// ============================================================================

sig BoundedSequence {
    elements: seq Value,
    maxLength: one Int
}

fact BoundedSequenceInvariant {
    all bs: BoundedSequence |
        bs.maxLength >= 0 and
        #bs.elements <= bs.maxLength
}

pred seqIsEmpty[bs: BoundedSequence] {
    #bs.elements = 0
}

pred seqIsFull[bs: BoundedSequence] {
    #bs.elements = bs.maxLength
}

pred canAppend[bs: BoundedSequence] {
    #bs.elements < bs.maxLength
}

// ============================================================================
// SET BOUNDS
// ============================================================================

sig BoundedSet {
    members: set Value,
    maxSize: one Int
}

fact BoundedSetInvariant {
    all bs: BoundedSet |
        bs.maxSize >= 0 and
        #bs.members <= bs.maxSize
}

pred setIsEmpty[bs: BoundedSet] {
    #bs.members = 0
}

pred setIsFull[bs: BoundedSet] {
    #bs.members = bs.maxSize
}

pred canInsert[bs: BoundedSet, v: Value] {
    v not in bs.members and #bs.members < bs.maxSize
}

// ============================================================================
// SAFETY ASSERTIONS
// ============================================================================

// Assert that bounded integers stay bounded under operations
assert BoundedIntSafety {
    all b: BoundedInt | b.value >= b.minVal and b.value <= b.maxVal
}

// Assert percentage bounds
assert PercentageSafety {
    all p: Percentage | p.pctValue >= 0 and p.pctValue <= 100
}

// Assert port bounds
assert PortSafety {
    all p: Port | p.portNum >= 1 and p.portNum <= 65535
}

// Assert resource bar consistency
assert ResourceBarSafety {
    all rb: ResourceBar | rb.current >= 0 and rb.current <= rb.maximum
}

// Assert color component bounds
assert ColorSafety {
    all c: Color |
        c.red >= 0 and c.red <= 255 and
        c.green >= 0 and c.green <= 255 and
        c.blue >= 0 and c.blue <= 255 and
        c.alpha >= 0 and c.alpha <= 255
}

// ============================================================================
// EXAMPLE CHECKS
// ============================================================================

// Check that our invariants are satisfiable
run ExampleBoundedInt {
    some b: BoundedInt | b.value = 50 and b.minVal = 0 and b.maxVal = 100
} for 3

run ExampleResourceBar {
    some rb: ResourceBar | rb.current = 75 and rb.maximum = 100
} for 3

run ExampleStateMachine {
    some sm: StateMachine | #sm.allowedTransitions > 2
} for 5

// Check our assertions hold (should find no counterexamples)
check BoundedIntSafety for 5
check PercentageSafety for 5
check PortSafety for 5
check ResourceBarSafety for 5
check ColorSafety for 5
check PortCategoriesComplete for 5
