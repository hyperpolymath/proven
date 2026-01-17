// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe angle operations with unit conversions and normalization.
module SafeAngle =
    open System

    /// Angle representation.
    type Angle = private {
        Radians: float
    }

    /// Create angle from radians.
    let fromRadians (radians: float) : Angle =
        { Radians = radians }

    /// Create angle from degrees.
    let fromDegrees (degrees: float) : Angle =
        { Radians = degrees * Math.PI / 180.0 }

    /// Create angle from gradians (400 gradians = full circle).
    let fromGradians (gradians: float) : Angle =
        { Radians = gradians * Math.PI / 200.0 }

    /// Create angle from turns (1 turn = full circle).
    let fromTurns (turns: float) : Angle =
        { Radians = turns * 2.0 * Math.PI }

    /// Get angle in radians.
    let toRadians (angle: Angle) : float =
        angle.Radians

    /// Get angle in degrees.
    let toDegrees (angle: Angle) : float =
        angle.Radians * 180.0 / Math.PI

    /// Get angle in gradians.
    let toGradians (angle: Angle) : float =
        angle.Radians * 200.0 / Math.PI

    /// Get angle in turns.
    let toTurns (angle: Angle) : float =
        angle.Radians / (2.0 * Math.PI)

    /// Normalize angle to [0, 2*pi) radians.
    let normalize (angle: Angle) : Angle =
        let twoPi = 2.0 * Math.PI
        let normalizedRadians = angle.Radians % twoPi
        let result = if normalizedRadians < 0.0 then normalizedRadians + twoPi else normalizedRadians
        { Radians = result }

    /// Normalize angle to [-pi, pi) radians.
    let normalizeSigned (angle: Angle) : Angle =
        let normalized = normalize angle
        if normalized.Radians >= Math.PI then
            { Radians = normalized.Radians - 2.0 * Math.PI }
        else
            normalized

    /// Normalize angle to [0, 360) degrees.
    let normalizeDegrees (angle: Angle) : Angle =
        normalize angle

    /// Add two angles.
    let add (a: Angle) (b: Angle) : Angle =
        { Radians = a.Radians + b.Radians }

    /// Subtract angles.
    let subtract (a: Angle) (b: Angle) : Angle =
        { Radians = a.Radians - b.Radians }

    /// Multiply angle by scalar.
    let multiply (scalar: float) (angle: Angle) : Angle =
        { Radians = angle.Radians * scalar }

    /// Divide angle by scalar.
    let divide (scalar: float) (angle: Angle) : Angle option =
        if scalar = 0.0 then None
        else Some { Radians = angle.Radians / scalar }

    /// Negate angle.
    let negate (angle: Angle) : Angle =
        { Radians = -angle.Radians }

    /// Absolute value of angle.
    let abs (angle: Angle) : Angle =
        { Radians = Math.Abs(angle.Radians) }

    /// Sine of angle.
    let sin (angle: Angle) : float =
        Math.Sin(angle.Radians)

    /// Cosine of angle.
    let cos (angle: Angle) : float =
        Math.Cos(angle.Radians)

    /// Tangent of angle.
    let tan (angle: Angle) : float =
        Math.Tan(angle.Radians)

    /// Secant of angle.
    let sec (angle: Angle) : float option =
        let cosValue = cos angle
        if cosValue = 0.0 then None
        else Some(1.0 / cosValue)

    /// Cosecant of angle.
    let csc (angle: Angle) : float option =
        let sinValue = sin angle
        if sinValue = 0.0 then None
        else Some(1.0 / sinValue)

    /// Cotangent of angle.
    let cot (angle: Angle) : float option =
        let tanValue = tan angle
        if tanValue = 0.0 then None
        else Some(1.0 / tanValue)

    /// Arc sine (returns angle).
    let asin (value: float) : Angle option =
        if value < -1.0 || value > 1.0 then None
        else Some { Radians = Math.Asin(value) }

    /// Arc cosine (returns angle).
    let acos (value: float) : Angle option =
        if value < -1.0 || value > 1.0 then None
        else Some { Radians = Math.Acos(value) }

    /// Arc tangent (returns angle).
    let atan (value: float) : Angle =
        { Radians = Math.Atan(value) }

    /// Arc tangent of y/x (returns angle in correct quadrant).
    let atan2 (y: float) (x: float) : Angle =
        { Radians = Math.Atan2(y, x) }

    /// Get the smallest positive difference between two angles.
    let difference (a: Angle) (b: Angle) : Angle =
        let diff = Math.Abs(a.Radians - b.Radians)
        let normalized = diff % (2.0 * Math.PI)
        if normalized > Math.PI then
            { Radians = 2.0 * Math.PI - normalized }
        else
            { Radians = normalized }

    /// Linear interpolation between angles (shortest path).
    let lerp (t: float) (a: Angle) (b: Angle) : Angle =
        let aNorm = normalize a
        let bNorm = normalize b
        let diff = bNorm.Radians - aNorm.Radians
        let shortestDiff =
            if diff > Math.PI then diff - 2.0 * Math.PI
            elif diff < -Math.PI then diff + 2.0 * Math.PI
            else diff
        { Radians = aNorm.Radians + t * shortestDiff }

    /// Check if angle is in range (inclusive).
    let isInRange (minAngle: Angle) (maxAngle: Angle) (angle: Angle) : bool =
        let normMin = normalize minAngle
        let normMax = normalize maxAngle
        let normAngle = normalize angle
        if normMin.Radians <= normMax.Radians then
            normAngle.Radians >= normMin.Radians && normAngle.Radians <= normMax.Radians
        else
            // Range crosses 0/2*pi boundary
            normAngle.Radians >= normMin.Radians || normAngle.Radians <= normMax.Radians

    /// Clamp angle to range.
    let clamp (minAngle: Angle) (maxAngle: Angle) (angle: Angle) : Angle =
        if isInRange minAngle maxAngle angle then angle
        else
            let diffToMin = difference angle minAngle
            let diffToMax = difference angle maxAngle
            if diffToMin.Radians <= diffToMax.Radians then minAngle else maxAngle

    /// Compare angles (normalized).
    let compare (a: Angle) (b: Angle) : int =
        Operators.compare (normalize a).Radians (normalize b).Radians

    /// Check equality (normalized).
    let equals (a: Angle) (b: Angle) : bool =
        compare a b = 0

    /// Check approximate equality.
    let almostEqual (tolerance: Angle) (a: Angle) (b: Angle) : bool =
        (difference a b).Radians <= (abs tolerance).Radians

    /// Check if angle is acute (< 90 degrees).
    let isAcute (angle: Angle) : bool =
        let radians = (abs (normalize angle)).Radians
        radians < Math.PI / 2.0

    /// Check if angle is right angle (90 degrees).
    let isRight (tolerance: Angle) (angle: Angle) : bool =
        let radians = (abs (normalize angle)).Radians
        Math.Abs(radians - Math.PI / 2.0) <= (abs tolerance).Radians

    /// Check if angle is obtuse (> 90 degrees and < 180 degrees).
    let isObtuse (angle: Angle) : bool =
        let radians = (abs (normalize angle)).Radians
        radians > Math.PI / 2.0 && radians < Math.PI

    /// Check if angle is straight (180 degrees).
    let isStraight (tolerance: Angle) (angle: Angle) : bool =
        let radians = (abs (normalize angle)).Radians
        Math.Abs(radians - Math.PI) <= (abs tolerance).Radians

    /// Check if angle is reflex (> 180 degrees).
    let isReflex (angle: Angle) : bool =
        let radians = (normalize angle).Radians
        radians > Math.PI

    /// Get complementary angle (90 - angle).
    let complementary (angle: Angle) : Angle =
        { Radians = Math.PI / 2.0 - angle.Radians }

    /// Get supplementary angle (180 - angle).
    let supplementary (angle: Angle) : Angle =
        { Radians = Math.PI - angle.Radians }

    /// Get explementary angle (360 - angle).
    let explementary (angle: Angle) : Angle =
        { Radians = 2.0 * Math.PI - angle.Radians }

    /// Format angle as degrees string.
    let formatDegrees (decimalPlaces: int) (angle: Angle) : string =
        let degrees = toDegrees angle
        sprintf "%.*f\u00B0" decimalPlaces degrees

    /// Format angle as radians string.
    let formatRadians (decimalPlaces: int) (angle: Angle) : string =
        sprintf "%.*f rad" decimalPlaces angle.Radians

    /// Format angle as degrees, minutes, seconds.
    let formatDMS (angle: Angle) : string =
        let totalDegrees = toDegrees (abs angle)
        let degrees = int totalDegrees
        let minutesFloat = (totalDegrees - float degrees) * 60.0
        let minutes = int minutesFloat
        let seconds = (minutesFloat - float minutes) * 60.0
        let sign = if angle.Radians < 0.0 then "-" else ""
        sprintf "%s%d\u00B0 %d' %.2f\"" sign degrees minutes seconds

    /// Common angle constants.
    let zero : Angle = fromRadians 0.0
    let quarterTurn : Angle = fromRadians (Math.PI / 2.0)
    let halfTurn : Angle = fromRadians Math.PI
    let threeQuarterTurn : Angle = fromRadians (3.0 * Math.PI / 2.0)
    let fullTurn : Angle = fromRadians (2.0 * Math.PI)

    /// Degree constants.
    let deg0 : Angle = fromDegrees 0.0
    let deg30 : Angle = fromDegrees 30.0
    let deg45 : Angle = fromDegrees 45.0
    let deg60 : Angle = fromDegrees 60.0
    let deg90 : Angle = fromDegrees 90.0
    let deg120 : Angle = fromDegrees 120.0
    let deg180 : Angle = fromDegrees 180.0
    let deg270 : Angle = fromDegrees 270.0
    let deg360 : Angle = fromDegrees 360.0
