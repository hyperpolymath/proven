// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

// Unit conversion module for common measurements.

// Length units in meters.
LengthUnit :: enum {
    Millimeter,
    Centimeter,
    Meter,
    Kilometer,
    Inch,
    Foot,
    Yard,
    Mile,
    NauticalMile,
}

// Get length unit in meters.
length_to_meters :: proc(value: f64, unit: LengthUnit) -> f64 {
    switch unit {
    case .Millimeter: return value * 0.001
    case .Centimeter: return value * 0.01
    case .Meter: return value
    case .Kilometer: return value * 1000.0
    case .Inch: return value * 0.0254
    case .Foot: return value * 0.3048
    case .Yard: return value * 0.9144
    case .Mile: return value * 1609.344
    case .NauticalMile: return value * 1852.0
    case: return value
    }
}

// Convert meters to length unit.
meters_to_length :: proc(meters: f64, unit: LengthUnit) -> f64 {
    switch unit {
    case .Millimeter: return meters / 0.001
    case .Centimeter: return meters / 0.01
    case .Meter: return meters
    case .Kilometer: return meters / 1000.0
    case .Inch: return meters / 0.0254
    case .Foot: return meters / 0.3048
    case .Yard: return meters / 0.9144
    case .Mile: return meters / 1609.344
    case .NauticalMile: return meters / 1852.0
    case: return meters
    }
}

// Convert between any two length units.
convert_length :: proc(value: f64, from, to: LengthUnit) -> f64 {
    meters := length_to_meters(value, from)
    return meters_to_length(meters, to)
}

// Mass units in kilograms.
MassUnit :: enum {
    Milligram,
    Gram,
    Kilogram,
    MetricTon,
    Ounce,
    Pound,
    Stone,
    ShortTon,
    LongTon,
}

// Get mass unit in kilograms.
mass_to_kilograms :: proc(value: f64, unit: MassUnit) -> f64 {
    switch unit {
    case .Milligram: return value * 1e-6
    case .Gram: return value * 0.001
    case .Kilogram: return value
    case .MetricTon: return value * 1000.0
    case .Ounce: return value * 0.028349523125
    case .Pound: return value * 0.45359237
    case .Stone: return value * 6.35029318
    case .ShortTon: return value * 907.18474
    case .LongTon: return value * 1016.0469088
    case: return value
    }
}

// Convert kilograms to mass unit.
kilograms_to_mass :: proc(kg: f64, unit: MassUnit) -> f64 {
    switch unit {
    case .Milligram: return kg / 1e-6
    case .Gram: return kg / 0.001
    case .Kilogram: return kg
    case .MetricTon: return kg / 1000.0
    case .Ounce: return kg / 0.028349523125
    case .Pound: return kg / 0.45359237
    case .Stone: return kg / 6.35029318
    case .ShortTon: return kg / 907.18474
    case .LongTon: return kg / 1016.0469088
    case: return kg
    }
}

// Convert between any two mass units.
convert_mass :: proc(value: f64, from, to: MassUnit) -> f64 {
    kg := mass_to_kilograms(value, from)
    return kilograms_to_mass(kg, to)
}

// Temperature units.
TemperatureUnit :: enum {
    Celsius,
    Fahrenheit,
    Kelvin,
}

// Convert temperature to Celsius.
temp_to_celsius :: proc(value: f64, unit: TemperatureUnit) -> f64 {
    switch unit {
    case .Celsius: return value
    case .Fahrenheit: return (value - 32.0) * 5.0 / 9.0
    case .Kelvin: return value - 273.15
    case: return value
    }
}

// Convert Celsius to temperature unit.
celsius_to_temp :: proc(celsius: f64, unit: TemperatureUnit) -> f64 {
    switch unit {
    case .Celsius: return celsius
    case .Fahrenheit: return celsius * 9.0 / 5.0 + 32.0
    case .Kelvin: return celsius + 273.15
    case: return celsius
    }
}

// Convert between any two temperature units.
convert_temperature :: proc(value: f64, from, to: TemperatureUnit) -> f64 {
    celsius := temp_to_celsius(value, from)
    return celsius_to_temp(celsius, to)
}

// Volume units in liters.
VolumeUnit :: enum {
    Milliliter,
    Liter,
    CubicMeter,
    USGallon,
    UKGallon,
    USQuart,
    USPint,
    USCup,
    USFluidOunce,
    USTablespoon,
    USTeaspoon,
}

// Get volume unit in liters.
volume_to_liters :: proc(value: f64, unit: VolumeUnit) -> f64 {
    switch unit {
    case .Milliliter: return value * 0.001
    case .Liter: return value
    case .CubicMeter: return value * 1000.0
    case .USGallon: return value * 3.785411784
    case .UKGallon: return value * 4.54609
    case .USQuart: return value * 0.946352946
    case .USPint: return value * 0.473176473
    case .USCup: return value * 0.2365882365
    case .USFluidOunce: return value * 0.0295735295625
    case .USTablespoon: return value * 0.01478676478125
    case .USTeaspoon: return value * 0.00492892159375
    case: return value
    }
}

// Convert liters to volume unit.
liters_to_volume :: proc(liters: f64, unit: VolumeUnit) -> f64 {
    switch unit {
    case .Milliliter: return liters / 0.001
    case .Liter: return liters
    case .CubicMeter: return liters / 1000.0
    case .USGallon: return liters / 3.785411784
    case .UKGallon: return liters / 4.54609
    case .USQuart: return liters / 0.946352946
    case .USPint: return liters / 0.473176473
    case .USCup: return liters / 0.2365882365
    case .USFluidOunce: return liters / 0.0295735295625
    case .USTablespoon: return liters / 0.01478676478125
    case .USTeaspoon: return liters / 0.00492892159375
    case: return liters
    }
}

// Convert between any two volume units.
convert_volume :: proc(value: f64, from, to: VolumeUnit) -> f64 {
    liters := volume_to_liters(value, from)
    return liters_to_volume(liters, to)
}

// Area units in square meters.
AreaUnit :: enum {
    SquareMillimeter,
    SquareCentimeter,
    SquareMeter,
    SquareKilometer,
    Hectare,
    Are,
    SquareInch,
    SquareFoot,
    SquareYard,
    Acre,
    SquareMile,
}

// Get area unit in square meters.
area_to_sqmeters :: proc(value: f64, unit: AreaUnit) -> f64 {
    switch unit {
    case .SquareMillimeter: return value * 1e-6
    case .SquareCentimeter: return value * 1e-4
    case .SquareMeter: return value
    case .SquareKilometer: return value * 1e6
    case .Hectare: return value * 10000.0
    case .Are: return value * 100.0
    case .SquareInch: return value * 0.00064516
    case .SquareFoot: return value * 0.09290304
    case .SquareYard: return value * 0.83612736
    case .Acre: return value * 4046.8564224
    case .SquareMile: return value * 2589988.110336
    case: return value
    }
}

// Convert square meters to area unit.
sqmeters_to_area :: proc(sqm: f64, unit: AreaUnit) -> f64 {
    switch unit {
    case .SquareMillimeter: return sqm / 1e-6
    case .SquareCentimeter: return sqm / 1e-4
    case .SquareMeter: return sqm
    case .SquareKilometer: return sqm / 1e6
    case .Hectare: return sqm / 10000.0
    case .Are: return sqm / 100.0
    case .SquareInch: return sqm / 0.00064516
    case .SquareFoot: return sqm / 0.09290304
    case .SquareYard: return sqm / 0.83612736
    case .Acre: return sqm / 4046.8564224
    case .SquareMile: return sqm / 2589988.110336
    case: return sqm
    }
}

// Convert between any two area units.
convert_area :: proc(value: f64, from, to: AreaUnit) -> f64 {
    sqm := area_to_sqmeters(value, from)
    return sqmeters_to_area(sqm, to)
}

// Speed units in meters per second.
SpeedUnit :: enum {
    MetersPerSecond,
    KilometersPerHour,
    MilesPerHour,
    Knots,
    FeetPerSecond,
}

// Get speed unit in meters per second.
speed_to_mps :: proc(value: f64, unit: SpeedUnit) -> f64 {
    switch unit {
    case .MetersPerSecond: return value
    case .KilometersPerHour: return value / 3.6
    case .MilesPerHour: return value * 0.44704
    case .Knots: return value * 0.514444
    case .FeetPerSecond: return value * 0.3048
    case: return value
    }
}

// Convert meters per second to speed unit.
mps_to_speed :: proc(mps: f64, unit: SpeedUnit) -> f64 {
    switch unit {
    case .MetersPerSecond: return mps
    case .KilometersPerHour: return mps * 3.6
    case .MilesPerHour: return mps / 0.44704
    case .Knots: return mps / 0.514444
    case .FeetPerSecond: return mps / 0.3048
    case: return mps
    }
}

// Convert between any two speed units.
convert_speed :: proc(value: f64, from, to: SpeedUnit) -> f64 {
    mps := speed_to_mps(value, from)
    return mps_to_speed(mps, to)
}

// Data size units in bytes.
DataUnit :: enum {
    Bit,
    Byte,
    Kilobyte,
    Megabyte,
    Gigabyte,
    Terabyte,
    Petabyte,
    Kibibyte,
    Mebibyte,
    Gibibyte,
    Tebibyte,
    Pebibyte,
}

// Get data unit in bytes.
data_to_bytes :: proc(value: f64, unit: DataUnit) -> f64 {
    switch unit {
    case .Bit: return value / 8.0
    case .Byte: return value
    case .Kilobyte: return value * 1000.0
    case .Megabyte: return value * 1000000.0
    case .Gigabyte: return value * 1000000000.0
    case .Terabyte: return value * 1000000000000.0
    case .Petabyte: return value * 1000000000000000.0
    case .Kibibyte: return value * 1024.0
    case .Mebibyte: return value * 1048576.0
    case .Gibibyte: return value * 1073741824.0
    case .Tebibyte: return value * 1099511627776.0
    case .Pebibyte: return value * 1125899906842624.0
    case: return value
    }
}

// Convert bytes to data unit.
bytes_to_data :: proc(bytes: f64, unit: DataUnit) -> f64 {
    switch unit {
    case .Bit: return bytes * 8.0
    case .Byte: return bytes
    case .Kilobyte: return bytes / 1000.0
    case .Megabyte: return bytes / 1000000.0
    case .Gigabyte: return bytes / 1000000000.0
    case .Terabyte: return bytes / 1000000000000.0
    case .Petabyte: return bytes / 1000000000000000.0
    case .Kibibyte: return bytes / 1024.0
    case .Mebibyte: return bytes / 1048576.0
    case .Gibibyte: return bytes / 1073741824.0
    case .Tebibyte: return bytes / 1099511627776.0
    case .Pebibyte: return bytes / 1125899906842624.0
    case: return bytes
    }
}

// Convert between any two data units.
convert_data :: proc(value: f64, from, to: DataUnit) -> f64 {
    bytes := data_to_bytes(value, from)
    return bytes_to_data(bytes, to)
}
