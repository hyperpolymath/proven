# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeAngle - Angle handling for Nickel
#
# Provides safe angle operations with conversions between
# degrees, radians, and gradians. Handles normalization.

{
  # Result type
  Result = {
    value | Number,
    ok | Bool,
  },

  # Mathematical constants
  PI = 3.141592653589793,
  TWO_PI = 6.283185307179586,
  HALF_PI = 1.5707963267948966,

  # Conversion constants
  DEG_TO_RAD = 0.017453292519943295,  # PI / 180
  RAD_TO_DEG = 57.29577951308232,     # 180 / PI
  GRAD_TO_DEG = 0.9,                  # 360 / 400
  DEG_TO_GRAD = 1.1111111111111112,   # 400 / 360

  # Angle representation
  Angle = {
    value | Number,
    unit | String,  # "deg", "rad", "grad"
  },

  # Convert degrees to radians
  deg_to_rad
    : Number -> Number
    = fun deg =>
      deg * DEG_TO_RAD,

  # Convert radians to degrees
  rad_to_deg
    : Number -> Number
    = fun rad =>
      rad * RAD_TO_DEG,

  # Convert degrees to gradians
  deg_to_grad
    : Number -> Number
    = fun deg =>
      deg * DEG_TO_GRAD,

  # Convert gradians to degrees
  grad_to_deg
    : Number -> Number
    = fun grad =>
      grad * GRAD_TO_DEG,

  # Convert radians to gradians
  rad_to_grad
    : Number -> Number
    = fun rad =>
      rad_to_deg rad * DEG_TO_GRAD,

  # Convert gradians to radians
  grad_to_rad
    : Number -> Number
    = fun grad =>
      deg_to_rad (grad * GRAD_TO_DEG),

  # Normalize degrees to [0, 360)
  normalize_deg
    : Number -> Number
    = fun deg =>
      let normalized = deg % 360 in
      if normalized < 0 then normalized + 360 else normalized,

  # Normalize radians to [0, 2*PI)
  normalize_rad
    : Number -> Number
    = fun rad =>
      let normalized = rad % TWO_PI in
      if normalized < 0 then normalized + TWO_PI else normalized,

  # Normalize radians to [-PI, PI)
  normalize_rad_signed
    : Number -> Number
    = fun rad =>
      let normalized = normalize_rad rad in
      if normalized > PI then normalized - TWO_PI else normalized,

  # Normalize degrees to [-180, 180)
  normalize_deg_signed
    : Number -> Number
    = fun deg =>
      let normalized = normalize_deg deg in
      if normalized > 180 then normalized - 360 else normalized,

  # Create angle from degrees
  from_deg
    : Number -> Angle
    = fun deg =>
      { value = deg, unit = "deg" },

  # Create angle from radians
  from_rad
    : Number -> Angle
    = fun rad =>
      { value = rad, unit = "rad" },

  # Create angle from gradians
  from_grad
    : Number -> Angle
    = fun grad =>
      { value = grad, unit = "grad" },

  # Convert angle to degrees
  to_deg
    : Angle -> Number
    = fun angle =>
      if angle.unit == "deg" then angle.value
      else if angle.unit == "rad" then rad_to_deg angle.value
      else if angle.unit == "grad" then grad_to_deg angle.value
      else angle.value,

  # Convert angle to radians
  to_rad
    : Angle -> Number
    = fun angle =>
      if angle.unit == "rad" then angle.value
      else if angle.unit == "deg" then deg_to_rad angle.value
      else if angle.unit == "grad" then grad_to_rad angle.value
      else angle.value,

  # Convert angle to gradians
  to_grad
    : Angle -> Number
    = fun angle =>
      if angle.unit == "grad" then angle.value
      else if angle.unit == "deg" then deg_to_grad angle.value
      else if angle.unit == "rad" then rad_to_grad angle.value
      else angle.value,

  # Add two angles (result in first angle's unit)
  add_angles
    : Angle -> Angle -> Angle
    = fun a b =>
      let b_converted = to_deg { value = b.value, unit = b.unit } in
      let a_deg = to_deg a in
      if a.unit == "deg" then
        { value = a_deg + b_converted, unit = "deg" }
      else if a.unit == "rad" then
        { value = deg_to_rad (a_deg + b_converted), unit = "rad" }
      else
        { value = deg_to_grad (a_deg + b_converted), unit = "grad" },

  # Subtract angles
  sub_angles
    : Angle -> Angle -> Angle
    = fun a b =>
      let b_converted = to_deg { value = b.value, unit = b.unit } in
      let a_deg = to_deg a in
      if a.unit == "deg" then
        { value = a_deg - b_converted, unit = "deg" }
      else if a.unit == "rad" then
        { value = deg_to_rad (a_deg - b_converted), unit = "rad" }
      else
        { value = deg_to_grad (a_deg - b_converted), unit = "grad" },

  # Calculate angular difference (shortest path)
  angular_diff
    : Angle -> Angle -> Number
    = fun a b =>
      let a_deg = normalize_deg (to_deg a) in
      let b_deg = normalize_deg (to_deg b) in
      let diff = a_deg - b_deg in
      let abs_diff = if diff < 0 then -diff else diff in
      if abs_diff > 180 then 360 - abs_diff else abs_diff,

  # Check if angle is acute (< 90 degrees)
  is_acute
    : Angle -> Bool
    = fun angle =>
      let deg = normalize_deg (to_deg angle) in
      deg > 0 && deg < 90,

  # Check if angle is right (= 90 degrees)
  is_right
    : Angle -> Bool
    = fun angle =>
      let deg = normalize_deg (to_deg angle) in
      let diff = deg - 90 in
      (if diff < 0 then -diff else diff) < 0.0001,

  # Check if angle is obtuse (> 90 and < 180 degrees)
  is_obtuse
    : Angle -> Bool
    = fun angle =>
      let deg = normalize_deg (to_deg angle) in
      deg > 90 && deg < 180,

  # Check if angle is straight (= 180 degrees)
  is_straight
    : Angle -> Bool
    = fun angle =>
      let deg = normalize_deg (to_deg angle) in
      let diff = deg - 180 in
      (if diff < 0 then -diff else diff) < 0.0001,

  # Check if angle is reflex (> 180 and < 360 degrees)
  is_reflex
    : Angle -> Bool
    = fun angle =>
      let deg = normalize_deg (to_deg angle) in
      deg > 180 && deg < 360,

  # Common angles in degrees
  ANGLE_0 = { value = 0, unit = "deg" },
  ANGLE_30 = { value = 30, unit = "deg" },
  ANGLE_45 = { value = 45, unit = "deg" },
  ANGLE_60 = { value = 60, unit = "deg" },
  ANGLE_90 = { value = 90, unit = "deg" },
  ANGLE_120 = { value = 120, unit = "deg" },
  ANGLE_180 = { value = 180, unit = "deg" },
  ANGLE_270 = { value = 270, unit = "deg" },
  ANGLE_360 = { value = 360, unit = "deg" },

  # Contracts
  Degrees = std.contract.from_predicate (fun n => std.is_number n),
  Radians = std.contract.from_predicate (fun n => std.is_number n),
  NormalizedDegrees = std.contract.from_predicate (fun n => n >= 0 && n < 360),
  NormalizedRadians = std.contract.from_predicate (fun n => n >= 0 && n < TWO_PI),
}
