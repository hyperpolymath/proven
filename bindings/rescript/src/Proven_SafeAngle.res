// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeAngle - Safe angle operations that cannot crash.
 *
 * Provides angle conversions, normalization, and trigonometry.
 */

/** Degrees type */
type degrees = Degrees(float)

/** Radians type */
type radians = Radians(float)

/** Pi constant */
let pi = 3.14159265358979323846

/** Tau (2 * pi) constant */
let tau = 2.0 *. pi

/** Convert degrees to radians */
let toRadians = (Degrees(deg)): radians => {
  Radians(deg *. pi /. 180.0)
}

/** Convert radians to degrees */
let toDegrees = (Radians(rad)): degrees => {
  Degrees(rad *. 180.0 /. pi)
}

/** Normalize degrees to [0, 360) */
let normalizeDegrees = (Degrees(deg)): degrees => {
  let normalized = mod_float(deg, 360.0)
  if normalized < 0.0 {
    Degrees(normalized +. 360.0)
  } else {
    Degrees(normalized)
  }
}

/** Normalize radians to [0, 2*pi) */
let normalizeRadians = (Radians(rad)): radians => {
  let normalized = mod_float(rad, tau)
  if normalized < 0.0 {
    Radians(normalized +. tau)
  } else {
    Radians(normalized)
  }
}

/** Normalize degrees to [-180, 180) */
let normalizeDegreesSymmetric = (Degrees(deg)): degrees => {
  let Degrees(normalized) = normalizeDegrees(Degrees(deg))
  if normalized >= 180.0 {
    Degrees(normalized -. 360.0)
  } else {
    Degrees(normalized)
  }
}

/** Normalize radians to [-pi, pi) */
let normalizeRadiansSymmetric = (Radians(rad)): radians => {
  let Radians(normalized) = normalizeRadians(Radians(rad))
  if normalized >= pi {
    Radians(normalized -. tau)
  } else {
    Radians(normalized)
  }
}

/** Get the raw value from degrees */
let degreesValue = (Degrees(deg)): float => deg

/** Get the raw value from radians */
let radiansValue = (Radians(rad)): float => rad

/** Create degrees from float */
let degrees = (n: float): degrees => Degrees(n)

/** Create radians from float */
let radians = (n: float): radians => Radians(n)

/** Safe sine */
let sin = (angle: radians): float => {
  let Radians(rad) = angle
  Js.Math.sin(rad)
}

/** Safe cosine */
let cos = (angle: radians): float => {
  let Radians(rad) = angle
  Js.Math.cos(rad)
}

/** Safe tangent */
let tan = (angle: radians): option<float> => {
  let Radians(rad) = angle
  let result = Js.Math.tan(rad)
  if Js.Float.isFinite(result) {
    Some(result)
  } else {
    None
  }
}

/** Safe arcsine (returns None if input out of [-1, 1]) */
let asin = (n: float): option<radians> => {
  if n < -1.0 || n > 1.0 {
    None
  } else {
    Some(Radians(Js.Math.asin(n)))
  }
}

/** Safe arccosine (returns None if input out of [-1, 1]) */
let acos = (n: float): option<radians> => {
  if n < -1.0 || n > 1.0 {
    None
  } else {
    Some(Radians(Js.Math.acos(n)))
  }
}

/** Safe arctangent */
let atan = (n: float): radians => {
  Radians(Js.Math.atan(n))
}

/** Safe arctangent2 */
let atan2 = (~y: float, ~x: float): radians => {
  Radians(Js.Math.atan2(~y, ~x, ()))
}

/** Sine from degrees */
let sinDeg = (angle: degrees): float => {
  sin(toRadians(angle))
}

/** Cosine from degrees */
let cosDeg = (angle: degrees): float => {
  cos(toRadians(angle))
}

/** Tangent from degrees */
let tanDeg = (angle: degrees): option<float> => {
  tan(toRadians(angle))
}

/** Add two angles in degrees */
let addDegrees = (Degrees(a), Degrees(b)): degrees => {
  Degrees(a +. b)
}

/** Add two angles in radians */
let addRadians = (Radians(a), Radians(b)): radians => {
  Radians(a +. b)
}

/** Subtract angles in degrees */
let subDegrees = (Degrees(a), Degrees(b)): degrees => {
  Degrees(a -. b)
}

/** Subtract angles in radians */
let subRadians = (Radians(a), Radians(b)): radians => {
  Radians(a -. b)
}

/** Multiply angle by scalar */
let scaleDegrees = (Degrees(deg), factor: float): degrees => {
  Degrees(deg *. factor)
}

/** Multiply angle by scalar */
let scaleRadians = (Radians(rad), factor: float): radians => {
  Radians(rad *. factor)
}

/** Calculate the shortest difference between two angles in degrees */
let angleDiffDegrees = (Degrees(a), Degrees(b)): degrees => {
  let diff = mod_float(b -. a +. 180.0, 360.0) -. 180.0
  if diff < -180.0 {
    Degrees(diff +. 360.0)
  } else {
    Degrees(diff)
  }
}

/** Calculate the shortest difference between two angles in radians */
let angleDiffRadians = (Radians(a), Radians(b)): radians => {
  let diff = mod_float(b -. a +. pi, tau) -. pi
  if diff < -.pi {
    Radians(diff +. tau)
  } else {
    Radians(diff)
  }
}

/** Linear interpolation between angles (shortest path) in degrees */
let lerpDegrees = (Degrees(a), Degrees(b), t: float): degrees => {
  let Degrees(diff) = angleDiffDegrees(Degrees(a), Degrees(b))
  normalizeDegrees(Degrees(a +. diff *. t))
}

/** Linear interpolation between angles (shortest path) in radians */
let lerpRadians = (Radians(a), Radians(b), t: float): radians => {
  let Radians(diff) = angleDiffRadians(Radians(a), Radians(b))
  normalizeRadians(Radians(a +. diff *. t))
}

/** Check if angle is between two other angles (going clockwise from start to end) */
let isBetweenDegrees = (Degrees(angle), ~start: degrees, ~end_: degrees): bool => {
  let Degrees(s) = normalizeDegrees(start)
  let Degrees(e) = normalizeDegrees(end_)
  let Degrees(a) = normalizeDegrees(Degrees(angle))

  if s <= e {
    a >= s && a <= e
  } else {
    a >= s || a <= e
  }
}

/** Common angles */
let deg0: degrees = Degrees(0.0)
let deg45: degrees = Degrees(45.0)
let deg90: degrees = Degrees(90.0)
let deg180: degrees = Degrees(180.0)
let deg270: degrees = Degrees(270.0)
let deg360: degrees = Degrees(360.0)

let rad0: radians = Radians(0.0)
let radPi4: radians = Radians(pi /. 4.0)
let radPi2: radians = Radians(pi /. 2.0)
let radPi: radians = Radians(pi)
let rad3Pi2: radians = Radians(3.0 *. pi /. 2.0)
let radTau: radians = Radians(tau)
