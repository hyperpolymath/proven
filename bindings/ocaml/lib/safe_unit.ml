(* SPDX-License-Identifier: PMPL-1.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe unit conversions with dimensional analysis. *)

(** Length units. *)
type length_unit =
  | Meters
  | Kilometers
  | Centimeters
  | Millimeters
  | Micrometers
  | Nanometers
  | Miles
  | Yards
  | Feet
  | Inches
  | NauticalMiles
  | LightYears
  | AstronomicalUnits

(** Mass units. *)
type mass_unit =
  | Kilograms
  | Grams
  | Milligrams
  | Micrograms
  | MetricTons
  | Pounds
  | Ounces
  | Stones

(** Time units. *)
type time_unit =
  | Seconds
  | Milliseconds
  | Microseconds
  | Nanoseconds
  | Minutes
  | Hours
  | Days
  | Weeks
  | Years

(** Temperature units. *)
type temperature_unit =
  | Celsius
  | Fahrenheit
  | Kelvin

(** Data size units. *)
type data_unit =
  | Bits
  | Bytes
  | Kilobytes
  | Megabytes
  | Gigabytes
  | Terabytes
  | Petabytes
  | Kibibytes
  | Mebibytes
  | Gibibytes
  | Tebibytes

(** Speed units. *)
type speed_unit =
  | MetersPerSecond
  | KilometersPerHour
  | MilesPerHour
  | Knots
  | FeetPerSecond

(** Pressure units. *)
type pressure_unit =
  | Pascals
  | Kilopascals
  | Bar
  | Atmospheres
  | PSI
  | Torr

(** Energy units. *)
type energy_unit =
  | Joules
  | Kilojoules
  | Calories
  | Kilocalories
  | WattHours
  | KilowattHours
  | Electronvolts

(** Length value. *)
type length = { value: float; unit_type: length_unit }

(** Mass value. *)
type mass = { value: float; unit_type: mass_unit }

(** Time value. *)
type time = { value: float; unit_type: time_unit }

(** Temperature value. *)
type temperature = { value: float; unit_type: temperature_unit }

(** Data size value. *)
type data_size = { value: float; unit_type: data_unit }

(** Speed value. *)
type speed = { value: float; unit_type: speed_unit }

(** Pressure value. *)
type pressure = { value: float; unit_type: pressure_unit }

(** Energy value. *)
type energy = { value: float; unit_type: energy_unit }

(** Length conversion factors to meters. *)
let length_to_meters = function
  | Meters -> 1.0
  | Kilometers -> 1000.0
  | Centimeters -> 0.01
  | Millimeters -> 0.001
  | Micrometers -> 1e-6
  | Nanometers -> 1e-9
  | Miles -> 1609.344
  | Yards -> 0.9144
  | Feet -> 0.3048
  | Inches -> 0.0254
  | NauticalMiles -> 1852.0
  | LightYears -> 9.461e15
  | AstronomicalUnits -> 1.496e11

(** Convert length to meters. *)
let length_in_meters l =
  l.value *. length_to_meters l.unit_type

(** Convert length between units. *)
let convert_length target l =
  let meters = length_in_meters l in
  { value = meters /. length_to_meters target; unit_type = target }

(** Mass conversion factors to kilograms. *)
let mass_to_kg = function
  | Kilograms -> 1.0
  | Grams -> 0.001
  | Milligrams -> 1e-6
  | Micrograms -> 1e-9
  | MetricTons -> 1000.0
  | Pounds -> 0.453592
  | Ounces -> 0.0283495
  | Stones -> 6.35029

(** Convert mass to kilograms. *)
let mass_in_kg m =
  m.value *. mass_to_kg m.unit_type

(** Convert mass between units. *)
let convert_mass target m =
  let kg = mass_in_kg m in
  { value = kg /. mass_to_kg target; unit_type = target }

(** Time conversion factors to seconds. *)
let time_to_seconds = function
  | Seconds -> 1.0
  | Milliseconds -> 0.001
  | Microseconds -> 1e-6
  | Nanoseconds -> 1e-9
  | Minutes -> 60.0
  | Hours -> 3600.0
  | Days -> 86400.0
  | Weeks -> 604800.0
  | Years -> 31557600.0  (* Julian year *)

(** Convert time to seconds. *)
let time_in_seconds t =
  t.value *. time_to_seconds t.unit_type

(** Convert time between units. *)
let convert_time target t =
  let secs = time_in_seconds t in
  { value = secs /. time_to_seconds target; unit_type = target }

(** Convert temperature to Celsius. *)
let temp_to_celsius t =
  match t.unit_type with
  | Celsius -> t.value
  | Fahrenheit -> (t.value -. 32.0) *. 5.0 /. 9.0
  | Kelvin -> t.value -. 273.15

(** Convert temperature from Celsius to target unit. *)
let temp_from_celsius target c =
  match target with
  | Celsius -> { value = c; unit_type = Celsius }
  | Fahrenheit -> { value = c *. 9.0 /. 5.0 +. 32.0; unit_type = Fahrenheit }
  | Kelvin -> { value = c +. 273.15; unit_type = Kelvin }

(** Convert temperature between units. *)
let convert_temperature target t =
  temp_from_celsius target (temp_to_celsius t)

(** Data size conversion factors to bytes. *)
let data_to_bytes = function
  | Bits -> 0.125
  | Bytes -> 1.0
  | Kilobytes -> 1000.0
  | Megabytes -> 1e6
  | Gigabytes -> 1e9
  | Terabytes -> 1e12
  | Petabytes -> 1e15
  | Kibibytes -> 1024.0
  | Mebibytes -> 1048576.0
  | Gibibytes -> 1073741824.0
  | Tebibytes -> 1099511627776.0

(** Convert data size to bytes. *)
let data_in_bytes d =
  d.value *. data_to_bytes d.unit_type

(** Convert data size between units. *)
let convert_data target d =
  let bytes = data_in_bytes d in
  { value = bytes /. data_to_bytes target; unit_type = target }

(** Speed conversion factors to m/s. *)
let speed_to_mps = function
  | MetersPerSecond -> 1.0
  | KilometersPerHour -> 1.0 /. 3.6
  | MilesPerHour -> 0.44704
  | Knots -> 0.514444
  | FeetPerSecond -> 0.3048

(** Convert speed to m/s. *)
let speed_in_mps s =
  s.value *. speed_to_mps s.unit_type

(** Convert speed between units. *)
let convert_speed target s =
  let mps = speed_in_mps s in
  { value = mps /. speed_to_mps target; unit_type = target }

(** Pressure conversion factors to Pascals. *)
let pressure_to_pascals = function
  | Pascals -> 1.0
  | Kilopascals -> 1000.0
  | Bar -> 100000.0
  | Atmospheres -> 101325.0
  | PSI -> 6894.76
  | Torr -> 133.322

(** Convert pressure to Pascals. *)
let pressure_in_pascals p =
  p.value *. pressure_to_pascals p.unit_type

(** Convert pressure between units. *)
let convert_pressure target p =
  let pascals = pressure_in_pascals p in
  { value = pascals /. pressure_to_pascals target; unit_type = target }

(** Energy conversion factors to Joules. *)
let energy_to_joules = function
  | Joules -> 1.0
  | Kilojoules -> 1000.0
  | Calories -> 4.184
  | Kilocalories -> 4184.0
  | WattHours -> 3600.0
  | KilowattHours -> 3.6e6
  | Electronvolts -> 1.60218e-19

(** Convert energy to Joules. *)
let energy_in_joules e =
  e.value *. energy_to_joules e.unit_type

(** Convert energy between units. *)
let convert_energy target e =
  let joules = energy_in_joules e in
  { value = joules /. energy_to_joules target; unit_type = target }

(** Create values with units. *)
let meters v = { value = v; unit_type = Meters }
let kilometers v = { value = v; unit_type = Kilometers }
let miles v = { value = v; unit_type = Miles }
let feet v = { value = v; unit_type = Feet }
let inches v = { value = v; unit_type = Inches }

let kilograms v = { value = v; unit_type = Kilograms }
let grams v = { value = v; unit_type = Grams }
let pounds v = { value = v; unit_type = Pounds }
let ounces v = { value = v; unit_type = Ounces }

let seconds v = { value = v; unit_type = Seconds }
let milliseconds v = { value = v; unit_type = Milliseconds }
let minutes v = { value = v; unit_type = Minutes }
let hours v = { value = v; unit_type = Hours }
let days v = { value = v; unit_type = Days }

let celsius v = { value = v; unit_type = Celsius }
let fahrenheit v = { value = v; unit_type = Fahrenheit }
let kelvin v = { value = v; unit_type = Kelvin }

let bytes_val v = { value = v; unit_type = Bytes }
let kilobytes v = { value = v; unit_type = Kilobytes }
let megabytes v = { value = v; unit_type = Megabytes }
let gigabytes v = { value = v; unit_type = Gigabytes }
let terabytes v = { value = v; unit_type = Terabytes }

let mps v = { value = v; unit_type = MetersPerSecond }
let kmh v = { value = v; unit_type = KilometersPerHour }
let mph v = { value = v; unit_type = MilesPerHour }

(** Format length with unit suffix. *)
let format_length l =
  let suffix = match l.unit_type with
    | Meters -> "m"
    | Kilometers -> "km"
    | Centimeters -> "cm"
    | Millimeters -> "mm"
    | Micrometers -> "\xCE\xBCm"
    | Nanometers -> "nm"
    | Miles -> "mi"
    | Yards -> "yd"
    | Feet -> "ft"
    | Inches -> "in"
    | NauticalMiles -> "nmi"
    | LightYears -> "ly"
    | AstronomicalUnits -> "AU"
  in
  Printf.sprintf "%.4g %s" l.value suffix

(** Format mass with unit suffix. *)
let format_mass m =
  let suffix = match m.unit_type with
    | Kilograms -> "kg"
    | Grams -> "g"
    | Milligrams -> "mg"
    | Micrograms -> "\xCE\xBCg"
    | MetricTons -> "t"
    | Pounds -> "lb"
    | Ounces -> "oz"
    | Stones -> "st"
  in
  Printf.sprintf "%.4g %s" m.value suffix

(** Format temperature with unit suffix. *)
let format_temperature t =
  let suffix = match t.unit_type with
    | Celsius -> "\xC2\xB0C"
    | Fahrenheit -> "\xC2\xB0F"
    | Kelvin -> "K"
  in
  Printf.sprintf "%.2f %s" t.value suffix

(** Format data size with appropriate unit. *)
let format_data_size ?(binary=false) d =
  let bytes = data_in_bytes d in
  let units = if binary then
    [| (1099511627776.0, "TiB"); (1073741824.0, "GiB"); (1048576.0, "MiB");
       (1024.0, "KiB"); (1.0, "B") |]
  else
    [| (1e12, "TB"); (1e9, "GB"); (1e6, "MB"); (1e3, "KB"); (1.0, "B") |]
  in
  let rec find i =
    if i >= Array.length units - 1 then units.(i)
    else
      let (threshold, _) = units.(i) in
      if bytes >= threshold then units.(i)
      else find (i + 1)
  in
  let (divisor, suffix) = find 0 in
  Printf.sprintf "%.2f %s" (bytes /. divisor) suffix
