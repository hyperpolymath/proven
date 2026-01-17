// SPDX-License-Identifier: Apache-2.0
// Proven - Safety primitives for OpenQASM 3.0 quantum computing
//
// Provides safe abstractions for quantum operations including bounded angles,
// probability validation, qubit index checking, and gate parameter safety.

OPENQASM 3.0;

// ============================================================================
// CONSTANTS
// ============================================================================

const float[64] PI = 3.14159265358979323846;
const float[64] TAU = 6.28318530717958647692;
const float[64] PROB_EPSILON = 1e-10;

// ============================================================================
// SAFE ANGLE OPERATIONS
// ============================================================================

// Wrap angle to [-π, π] range
def proven_wrap_angle(float[64] angle) -> float[64] {
    float[64] wrapped = angle;
    while (wrapped > PI) { wrapped = wrapped - TAU; }
    while (wrapped < -PI) { wrapped = wrapped + TAU; }
    return wrapped;
}

// Clamp angle to [-π, π]
def proven_clamp_angle(float[64] angle) -> float[64] {
    if (angle < -PI) { return -PI; }
    if (angle > PI) { return PI; }
    return angle;
}

// Safe rotation angle
def proven_safe_rotation(float[64] angle) -> float[64] {
    return proven_wrap_angle(angle);
}

// Degrees to radians
def proven_deg_to_rad(float[64] deg) -> float[64] {
    return proven_wrap_angle(deg * PI / 180.0);
}

// ============================================================================
// SAFE PROBABILITY OPERATIONS
// ============================================================================

// Clamp probability to [0, 1]
def proven_clamp_prob(float[64] p) -> float[64] {
    if (p < 0.0) { return 0.0; }
    if (p > 1.0) { return 1.0; }
    return p;
}

// Check if valid probability
def proven_is_valid_prob(float[64] p) -> bool {
    return p >= 0.0 && p <= 1.0;
}

// Check if two probabilities sum to 1
def proven_probs_normalized(float[64] p0, float[64] p1) -> bool {
    float[64] diff = p0 + p1 - 1.0;
    if (diff < 0.0) { diff = -diff; }
    return diff < PROB_EPSILON;
}

// ============================================================================
// SAFE QUBIT INDEX OPERATIONS
// ============================================================================

// Check valid qubit index
def proven_valid_qubit(int[32] idx, int[32] size) -> bool {
    return idx >= 0 && idx < size;
}

// Clamp qubit index
def proven_clamp_qubit(int[32] idx, int[32] size) -> int[32] {
    if (idx < 0) { return 0; }
    if (idx >= size) { return size - 1; }
    return idx;
}

// Check qubits are distinct (for 2-qubit gates)
def proven_qubits_distinct(int[32] a, int[32] b) -> bool {
    return a != b;
}

// Check three qubits distinct (for Toffoli)
def proven_qubits_distinct_3(int[32] a, int[32] b, int[32] c) -> bool {
    return a != b && b != c && a != c;
}

// ============================================================================
// SAFE GATE PARAMETERS
// ============================================================================

def proven_safe_rx(float[64] theta) -> float[64] { return proven_wrap_angle(theta); }
def proven_safe_ry(float[64] theta) -> float[64] { return proven_wrap_angle(theta); }
def proven_safe_rz(float[64] theta) -> float[64] { return proven_wrap_angle(theta); }
def proven_safe_phase(float[64] phi) -> float[64] { return proven_wrap_angle(phi); }

// U3 gate parameters
def proven_safe_u3_theta(float[64] t) -> float[64] {
    float[64] w = proven_wrap_angle(t);
    if (w < 0.0) { return -w; }
    return w;
}

// ============================================================================
// BOUNDED VALUES
// ============================================================================

// Shot count bounds
const int[32] SHOTS_MIN = 1;
const int[32] SHOTS_MAX = 1000000;

def proven_clamp_shots(int[32] n) -> int[32] {
    if (n < SHOTS_MIN) { return SHOTS_MIN; }
    if (n > SHOTS_MAX) { return SHOTS_MAX; }
    return n;
}

// Circuit depth bounds
const int[32] DEPTH_MAX = 10000;

def proven_clamp_depth(int[32] d) -> int[32] {
    if (d < 1) { return 1; }
    if (d > DEPTH_MAX) { return DEPTH_MAX; }
    return d;
}

// ============================================================================
// QUANTUM ERROR METRICS
// ============================================================================

const float[64] FIDELITY_THRESHOLD = 0.99;
const float[64] ERROR_THRESHOLD = 0.01;

def proven_clamp_fidelity(float[64] f) -> float[64] { return proven_clamp_prob(f); }
def proven_is_high_fidelity(float[64] f) -> bool { return f >= FIDELITY_THRESHOLD; }
def proven_clamp_error_rate(float[64] e) -> float[64] { return proven_clamp_prob(e); }
def proven_is_low_error(float[64] e) -> bool { return e <= ERROR_THRESHOLD; }

// ============================================================================
// UTILITY
// ============================================================================

// Safe power of 2 for Hilbert space dimensions
def proven_safe_pow2(int[32] exp) -> int[32] {
    if (exp < 0) { return 1; }
    if (exp > 30) { return 1073741824; }
    int[32] result = 1;
    for int[32] i in [0:exp] { result = result * 2; }
    return result;
}

// Hilbert space dimension
def proven_hilbert_dim(int[32] qubits) -> int[32] {
    return proven_safe_pow2(qubits);
}
