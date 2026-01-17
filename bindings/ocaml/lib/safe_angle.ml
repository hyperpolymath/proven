(* SPDX-License-Identifier: PMPL-1.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe angle operations with unit conversions and normalization. *)

(** Angle unit types. *)
type unit_type =
  | Degrees
  | Radians
  | Gradians
  | Turns

(** Angle representation. *)
type t = {
  value: float;
  unit_type: unit_type;
}

(** Mathematical constants. *)
let pi = Float.pi
let tau = 2.0 *. pi

(** Create an angle in degrees. *)
let degrees value = { value; unit_type = Degrees }

(** Create an angle in radians. *)
let radians value = { value; unit_type = Radians }

(** Create an angle in gradians. *)
let gradians value = { value; unit_type = Gradians }

(** Create an angle in turns. *)
let turns value = { value; unit_type = Turns }

(** Convert angle to radians. *)
let to_radians angle =
  match angle.unit_type with
  | Radians -> angle.value
  | Degrees -> angle.value *. pi /. 180.0
  | Gradians -> angle.value *. pi /. 200.0
  | Turns -> angle.value *. tau

(** Convert angle to degrees. *)
let to_degrees angle =
  match angle.unit_type with
  | Degrees -> angle.value
  | Radians -> angle.value *. 180.0 /. pi
  | Gradians -> angle.value *. 0.9
  | Turns -> angle.value *. 360.0

(** Convert angle to gradians. *)
let to_gradians angle =
  match angle.unit_type with
  | Gradians -> angle.value
  | Degrees -> angle.value /. 0.9
  | Radians -> angle.value *. 200.0 /. pi
  | Turns -> angle.value *. 400.0

(** Convert angle to turns. *)
let to_turns angle =
  match angle.unit_type with
  | Turns -> angle.value
  | Degrees -> angle.value /. 360.0
  | Radians -> angle.value /. tau
  | Gradians -> angle.value /. 400.0

(** Convert angle to a specific unit. *)
let convert target_unit angle =
  let rad = to_radians angle in
  match target_unit with
  | Radians -> { value = rad; unit_type = Radians }
  | Degrees -> { value = rad *. 180.0 /. pi; unit_type = Degrees }
  | Gradians -> { value = rad *. 200.0 /. pi; unit_type = Gradians }
  | Turns -> { value = rad /. tau; unit_type = Turns }

(** Normalize angle to [0, 360) degrees or equivalent. *)
let normalize angle =
  let turns = to_turns angle in
  let normalized = turns -. Float.floor turns in
  convert angle.unit_type { value = normalized; unit_type = Turns }

(** Normalize angle to [-180, 180) degrees or equivalent. *)
let normalize_signed angle =
  let turns = to_turns angle in
  let normalized = turns -. Float.floor (turns +. 0.5) in
  convert angle.unit_type { value = normalized; unit_type = Turns }

(** Add two angles. *)
let add a b =
  let result = to_radians a +. to_radians b in
  convert a.unit_type { value = result; unit_type = Radians }

(** Subtract two angles. *)
let sub a b =
  let result = to_radians a -. to_radians b in
  convert a.unit_type { value = result; unit_type = Radians }

(** Multiply angle by scalar. *)
let mul scalar angle =
  { angle with value = angle.value *. scalar }

(** Divide angle by scalar. *)
let div scalar angle =
  if scalar = 0.0 then None
  else Some { angle with value = angle.value /. scalar }

(** Negate angle. *)
let negate angle =
  { angle with value = -. angle.value }

(** Compare two angles (after normalization). *)
let compare a b =
  let a_turns = to_turns (normalize a) in
  let b_turns = to_turns (normalize b) in
  Float.compare a_turns b_turns

(** Check if two angles are equal (within epsilon). *)
let equal ?(epsilon=1e-10) a b =
  let a_rad = to_radians (normalize a) in
  let b_rad = to_radians (normalize b) in
  Float.abs (a_rad -. b_rad) < epsilon

(** Compute sine of angle. *)
let sin angle =
  Float.sin (to_radians angle)

(** Compute cosine of angle. *)
let cos angle =
  Float.cos (to_radians angle)

(** Compute tangent of angle. *)
let tan angle =
  Float.tan (to_radians angle)

(** Create angle from sine value. *)
let asin value =
  if value < -1.0 || value > 1.0 then None
  else Some { value = Float.asin value; unit_type = Radians }

(** Create angle from cosine value. *)
let acos value =
  if value < -1.0 || value > 1.0 then None
  else Some { value = Float.acos value; unit_type = Radians }

(** Create angle from tangent value. *)
let atan value =
  Some { value = Float.atan value; unit_type = Radians }

(** Create angle from y/x coordinates (atan2). *)
let atan2 ~y ~x =
  { value = Float.atan2 y x; unit_type = Radians }

(** Get the smallest angle between two angles. *)
let difference a b =
  let diff = normalize_signed (sub a b) in
  if to_degrees diff < 0.0 then negate diff else diff

(** Linear interpolation between two angles (takes shortest path). *)
let lerp t a b =
  let diff = normalize_signed (sub b a) in
  add a (mul t diff)

(** Check if angle is in range [start, end) going clockwise. *)
let is_between ~start ~stop angle =
  let start_turns = to_turns (normalize start) in
  let stop_turns = to_turns (normalize stop) in
  let angle_turns = to_turns (normalize angle) in
  if start_turns <= stop_turns then
    angle_turns >= start_turns && angle_turns < stop_turns
  else
    angle_turns >= start_turns || angle_turns < stop_turns

(** Format angle as string. *)
let format angle =
  let suffix = match angle.unit_type with
    | Degrees -> "deg"
    | Radians -> "rad"
    | Gradians -> "grad"
    | Turns -> "turn"
  in
  Printf.sprintf "%.6f%s" angle.value suffix

(** Format as degrees with degree symbol. *)
let format_degrees angle =
  Printf.sprintf "%.2f\xC2\xB0" (to_degrees angle)

(** Common angles. *)
let zero = degrees 0.0
let right_angle = degrees 90.0
let straight_angle = degrees 180.0
let full_rotation = degrees 360.0
let quarter_turn = turns 0.25
let half_turn = turns 0.5

(** Cardinal directions. *)
let north = degrees 0.0
let east = degrees 90.0
let south = degrees 180.0
let west = degrees 270.0

(** Convert compass bearing to mathematical angle. *)
let bearing_to_math angle =
  normalize (sub (degrees 90.0) angle)

(** Convert mathematical angle to compass bearing. *)
let math_to_bearing angle =
  normalize (sub (degrees 90.0) angle)
