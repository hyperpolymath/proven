// SPDX-License-Identifier: Apache-2.0
// Proven - Safety primitives for Q# (Microsoft Quantum SDK)

namespace Proven {
    open Microsoft.Quantum.Math;
    open Microsoft.Quantum.Convert;
    open Microsoft.Quantum.Intrinsic;

    // ========================================================================
    // RESULT TYPE
    // ========================================================================

    newtype QuantumResult<'T> = (IsOk: Bool, Value: 'T, ErrorCode: Int, ErrorMsg: String);

    function Ok<'T>(value: 'T) : QuantumResult<'T> {
        return QuantumResult(true, value, 0, "");
    }

    function Err<'T>(code: Int, msg: String, default_val: 'T) : QuantumResult<'T> {
        return QuantumResult(false, default_val, code, msg);
    }

    function UnwrapOr<'T>(r: QuantumResult<'T>, default_val: 'T) : 'T {
        if r::IsOk { return r::Value; }
        return default_val;
    }

    // ========================================================================
    // OPTION TYPE
    // ========================================================================

    newtype Option<'T> = (IsSome: Bool, Value: 'T);

    function Some<'T>(v: 'T) : Option<'T> { return Option(true, v); }
    function None<'T>(default_val: 'T) : Option<'T> { return Option(false, default_val); }

    // ========================================================================
    // CONSTANTS
    // ========================================================================

    function TAU() : Double { return 2.0 * PI(); }
    function ProbEpsilon() : Double { return 1e-10; }
    function FidelityThreshold() : Double { return 0.99; }
    function ErrorThreshold() : Double { return 0.01; }
    function MaxShots() : Int { return 1000000; }
    function MaxDepth() : Int { return 10000; }

    // ========================================================================
    // SAFE ANGLE OPERATIONS
    // ========================================================================

    function WrapAngle(angle: Double) : Double {
        mutable w = angle;
        while w > PI() { set w = w - TAU(); }
        while w < -PI() { set w = w + TAU(); }
        return w;
    }

    function ClampAngle(angle: Double) : Double {
        if angle < -PI() { return -PI(); }
        if angle > PI() { return PI(); }
        return angle;
    }

    function SafeRotation(angle: Double) : Double { return WrapAngle(angle); }
    function DegreesToRadians(deg: Double) : Double { return WrapAngle(deg * PI() / 180.0); }

    // ========================================================================
    // SAFE PROBABILITY
    // ========================================================================

    function ClampProbability(p: Double) : Double {
        if p < 0.0 { return 0.0; }
        if p > 1.0 { return 1.0; }
        return p;
    }

    function IsValidProbability(p: Double) : Bool { return p >= 0.0 and p <= 1.0; }

    function AmplitudeToProbability(re: Double, im: Double) : Double {
        return ClampProbability(re * re + im * im);
    }

    // ========================================================================
    // SAFE QUBIT INDEX
    // ========================================================================

    function IsValidQubitIndex(idx: Int, size: Int) : Bool { return idx >= 0 and idx < size; }

    function ClampQubitIndex(idx: Int, size: Int) : Int {
        if idx < 0 { return 0; }
        if idx >= size { return size - 1; }
        return idx;
    }

    function AreQubitsDistinct(a: Int, b: Int) : Bool { return a != b; }
    function AreQubitsDistinct3(a: Int, b: Int, c: Int) : Bool { return a != b and b != c and a != c; }

    // ========================================================================
    // BOUNDED VALUES
    // ========================================================================

    newtype BoundedShots = (Value: Int);
    function CreateBoundedShots(n: Int) : BoundedShots {
        mutable v = n;
        if v < 1 { set v = 1; }
        if v > MaxShots() { set v = MaxShots(); }
        return BoundedShots(v);
    }

    newtype BoundedDepth = (Value: Int);
    function CreateBoundedDepth(d: Int) : BoundedDepth {
        mutable v = d;
        if v < 1 { set v = 1; }
        if v > MaxDepth() { set v = MaxDepth(); }
        return BoundedDepth(v);
    }

    // ========================================================================
    // QUANTUM METRICS
    // ========================================================================

    newtype Fidelity = (Value: Double);
    function CreateFidelity(f: Double) : Fidelity { return Fidelity(ClampProbability(f)); }
    function IsHighFidelity(f: Fidelity) : Bool { return f::Value >= FidelityThreshold(); }

    newtype ErrorRate = (Value: Double);
    function CreateErrorRate(e: Double) : ErrorRate { return ErrorRate(ClampProbability(e)); }
    function IsLowError(e: ErrorRate) : Bool { return e::Value <= ErrorThreshold(); }

    newtype Concurrence = (Value: Double);
    function CreateConcurrence(c: Double) : Concurrence { return Concurrence(ClampProbability(c)); }
    function IsEntangled(c: Concurrence) : Bool { return c::Value > ProbEpsilon(); }

    // ========================================================================
    // SAFE GATE OPERATIONS
    // ========================================================================

    operation SafeRx(qubit: Qubit, angle: Double) : Unit { Rx(WrapAngle(angle), qubit); }
    operation SafeRy(qubit: Qubit, angle: Double) : Unit { Ry(WrapAngle(angle), qubit); }
    operation SafeRz(qubit: Qubit, angle: Double) : Unit { Rz(WrapAngle(angle), qubit); }

    operation SafeH(qubits: Qubit[], idx: Int) : QuantumResult<Unit> {
        if not IsValidQubitIndex(idx, Length(qubits)) {
            return Err(1, "Invalid qubit index", ());
        }
        H(qubits[idx]);
        return Ok(());
    }

    operation SafeCNOT(qubits: Qubit[], ctrl: Int, tgt: Int) : QuantumResult<Unit> {
        let n = Length(qubits);
        if not IsValidQubitIndex(ctrl, n) { return Err(1, "Invalid control", ()); }
        if not IsValidQubitIndex(tgt, n) { return Err(2, "Invalid target", ()); }
        if not AreQubitsDistinct(ctrl, tgt) { return Err(3, "Must be distinct", ()); }
        CNOT(qubits[ctrl], qubits[tgt]);
        return Ok(());
    }

    // ========================================================================
    // UTILITY
    // ========================================================================

    function SafePow2(exp: Int) : Int {
        if exp < 0 { return 1; }
        if exp > 30 { return 1073741824; }
        return 1 <<< exp;
    }

    function HilbertDimension(qubits: Int) : Int { return SafePow2(qubits); }
}
