// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * @file safe_unit.hpp
 * @brief Safe physical unit conversions
 *
 * Provides type-safe unit conversions for length, mass,
 * temperature, time, and other physical quantities.
 *
 * @example
 * @code
 * #include <proven/safe_unit.hpp>
 *
 * int main() {
 *     using namespace proven;
 *
 *     // Convert 5 miles to kilometers
 *     double km = convertLength(5.0, LengthUnit::Miles, LengthUnit::Kilometers);
 *
 *     // Convert temperature
 *     double celsius = convertTemperature(98.6, TemperatureUnit::Fahrenheit,
 *                                         TemperatureUnit::Celsius);
 *
 *     // Convert data sizes
 *     double mb = convertData(1.0, DataUnit::Gigabytes, DataUnit::Megabytes);
 *
 *     return 0;
 * }
 * @endcode
 */

#ifndef PROVEN_SAFE_UNIT_HPP
#define PROVEN_SAFE_UNIT_HPP

#include "common.hpp"
#include <cstdint>

namespace proven {

// ============================================================================
// Length Units
// ============================================================================

/**
 * @brief Length units
 */
enum class LengthUnit {
    Meters,
    Kilometers,
    Centimeters,
    Millimeters,
    Micrometers,
    Nanometers,
    Miles,
    Yards,
    Feet,
    Inches,
    NauticalMiles
};

/**
 * @brief Convert length to meters (base unit)
 */
[[nodiscard]] inline double lengthToMeters(double value, LengthUnit unit) noexcept {
    switch (unit) {
        case LengthUnit::Meters:        return value;
        case LengthUnit::Kilometers:    return value * 1000.0;
        case LengthUnit::Centimeters:   return value / 100.0;
        case LengthUnit::Millimeters:   return value / 1000.0;
        case LengthUnit::Micrometers:   return value / 1'000'000.0;
        case LengthUnit::Nanometers:    return value / 1'000'000'000.0;
        case LengthUnit::Miles:         return value * 1609.344;
        case LengthUnit::Yards:         return value * 0.9144;
        case LengthUnit::Feet:          return value * 0.3048;
        case LengthUnit::Inches:        return value * 0.0254;
        case LengthUnit::NauticalMiles: return value * 1852.0;
        default:                        return value;
    }
}

/**
 * @brief Convert length from meters
 */
[[nodiscard]] inline double lengthFromMeters(double meters, LengthUnit unit) noexcept {
    switch (unit) {
        case LengthUnit::Meters:        return meters;
        case LengthUnit::Kilometers:    return meters / 1000.0;
        case LengthUnit::Centimeters:   return meters * 100.0;
        case LengthUnit::Millimeters:   return meters * 1000.0;
        case LengthUnit::Micrometers:   return meters * 1'000'000.0;
        case LengthUnit::Nanometers:    return meters * 1'000'000'000.0;
        case LengthUnit::Miles:         return meters / 1609.344;
        case LengthUnit::Yards:         return meters / 0.9144;
        case LengthUnit::Feet:          return meters / 0.3048;
        case LengthUnit::Inches:        return meters / 0.0254;
        case LengthUnit::NauticalMiles: return meters / 1852.0;
        default:                        return meters;
    }
}

/**
 * @brief Convert length between units
 */
[[nodiscard]] inline double convertLength(double value, LengthUnit from, LengthUnit to) noexcept {
    double meters = lengthToMeters(value, from);
    return lengthFromMeters(meters, to);
}

// ============================================================================
// Mass Units
// ============================================================================

/**
 * @brief Mass units
 */
enum class MassUnit {
    Kilograms,
    Grams,
    Milligrams,
    Micrograms,
    MetricTons,
    Pounds,
    Ounces,
    Stones,
    ShortTons,
    LongTons
};

/**
 * @brief Convert mass to kilograms (base unit)
 */
[[nodiscard]] inline double massToKilograms(double value, MassUnit unit) noexcept {
    switch (unit) {
        case MassUnit::Kilograms:   return value;
        case MassUnit::Grams:       return value / 1000.0;
        case MassUnit::Milligrams:  return value / 1'000'000.0;
        case MassUnit::Micrograms:  return value / 1'000'000'000.0;
        case MassUnit::MetricTons:  return value * 1000.0;
        case MassUnit::Pounds:      return value * 0.453592;
        case MassUnit::Ounces:      return value * 0.0283495;
        case MassUnit::Stones:      return value * 6.35029;
        case MassUnit::ShortTons:   return value * 907.185;
        case MassUnit::LongTons:    return value * 1016.05;
        default:                    return value;
    }
}

/**
 * @brief Convert mass from kilograms
 */
[[nodiscard]] inline double massFromKilograms(double kg, MassUnit unit) noexcept {
    switch (unit) {
        case MassUnit::Kilograms:   return kg;
        case MassUnit::Grams:       return kg * 1000.0;
        case MassUnit::Milligrams:  return kg * 1'000'000.0;
        case MassUnit::Micrograms:  return kg * 1'000'000'000.0;
        case MassUnit::MetricTons:  return kg / 1000.0;
        case MassUnit::Pounds:      return kg / 0.453592;
        case MassUnit::Ounces:      return kg / 0.0283495;
        case MassUnit::Stones:      return kg / 6.35029;
        case MassUnit::ShortTons:   return kg / 907.185;
        case MassUnit::LongTons:    return kg / 1016.05;
        default:                    return kg;
    }
}

/**
 * @brief Convert mass between units
 */
[[nodiscard]] inline double convertMass(double value, MassUnit from, MassUnit to) noexcept {
    double kg = massToKilograms(value, from);
    return massFromKilograms(kg, to);
}

// ============================================================================
// Temperature Units
// ============================================================================

/**
 * @brief Temperature units
 */
enum class TemperatureUnit {
    Celsius,
    Fahrenheit,
    Kelvin,
    Rankine
};

/**
 * @brief Convert temperature between units
 */
[[nodiscard]] inline double convertTemperature(double value, TemperatureUnit from, TemperatureUnit to) noexcept {
    // Convert to Kelvin first
    double kelvin;
    switch (from) {
        case TemperatureUnit::Celsius:
            kelvin = value + 273.15;
            break;
        case TemperatureUnit::Fahrenheit:
            kelvin = (value - 32.0) * 5.0 / 9.0 + 273.15;
            break;
        case TemperatureUnit::Kelvin:
            kelvin = value;
            break;
        case TemperatureUnit::Rankine:
            kelvin = value * 5.0 / 9.0;
            break;
        default:
            kelvin = value;
            break;
    }

    // Convert from Kelvin
    switch (to) {
        case TemperatureUnit::Celsius:
            return kelvin - 273.15;
        case TemperatureUnit::Fahrenheit:
            return (kelvin - 273.15) * 9.0 / 5.0 + 32.0;
        case TemperatureUnit::Kelvin:
            return kelvin;
        case TemperatureUnit::Rankine:
            return kelvin * 9.0 / 5.0;
        default:
            return kelvin;
    }
}

// ============================================================================
// Time Units
// ============================================================================

/**
 * @brief Time units
 */
enum class TimeUnit {
    Seconds,
    Milliseconds,
    Microseconds,
    Nanoseconds,
    Minutes,
    Hours,
    Days,
    Weeks,
    Years
};

/**
 * @brief Convert time to seconds (base unit)
 */
[[nodiscard]] inline double timeToSeconds(double value, TimeUnit unit) noexcept {
    switch (unit) {
        case TimeUnit::Seconds:      return value;
        case TimeUnit::Milliseconds: return value / 1000.0;
        case TimeUnit::Microseconds: return value / 1'000'000.0;
        case TimeUnit::Nanoseconds:  return value / 1'000'000'000.0;
        case TimeUnit::Minutes:      return value * 60.0;
        case TimeUnit::Hours:        return value * 3600.0;
        case TimeUnit::Days:         return value * 86400.0;
        case TimeUnit::Weeks:        return value * 604800.0;
        case TimeUnit::Years:        return value * 31536000.0;  // 365 days
        default:                     return value;
    }
}

/**
 * @brief Convert time from seconds
 */
[[nodiscard]] inline double timeFromSeconds(double secs, TimeUnit unit) noexcept {
    switch (unit) {
        case TimeUnit::Seconds:      return secs;
        case TimeUnit::Milliseconds: return secs * 1000.0;
        case TimeUnit::Microseconds: return secs * 1'000'000.0;
        case TimeUnit::Nanoseconds:  return secs * 1'000'000'000.0;
        case TimeUnit::Minutes:      return secs / 60.0;
        case TimeUnit::Hours:        return secs / 3600.0;
        case TimeUnit::Days:         return secs / 86400.0;
        case TimeUnit::Weeks:        return secs / 604800.0;
        case TimeUnit::Years:        return secs / 31536000.0;
        default:                     return secs;
    }
}

/**
 * @brief Convert time between units
 */
[[nodiscard]] inline double convertTime(double value, TimeUnit from, TimeUnit to) noexcept {
    double secs = timeToSeconds(value, from);
    return timeFromSeconds(secs, to);
}

// ============================================================================
// Data Size Units
// ============================================================================

/**
 * @brief Data size units
 */
enum class DataUnit {
    Bits,
    Bytes,
    // SI units (base 10)
    Kilobytes,
    Megabytes,
    Gigabytes,
    Terabytes,
    Petabytes,
    // IEC units (base 2)
    Kibibytes,
    Mebibytes,
    Gibibytes,
    Tebibytes,
    Pebibytes
};

/**
 * @brief Convert data size to bytes (base unit)
 */
[[nodiscard]] inline double dataToBytes(double value, DataUnit unit) noexcept {
    switch (unit) {
        case DataUnit::Bits:      return value / 8.0;
        case DataUnit::Bytes:     return value;
        case DataUnit::Kilobytes: return value * 1000.0;
        case DataUnit::Megabytes: return value * 1'000'000.0;
        case DataUnit::Gigabytes: return value * 1'000'000'000.0;
        case DataUnit::Terabytes: return value * 1'000'000'000'000.0;
        case DataUnit::Petabytes: return value * 1'000'000'000'000'000.0;
        case DataUnit::Kibibytes: return value * 1024.0;
        case DataUnit::Mebibytes: return value * 1'048'576.0;
        case DataUnit::Gibibytes: return value * 1'073'741'824.0;
        case DataUnit::Tebibytes: return value * 1'099'511'627'776.0;
        case DataUnit::Pebibytes: return value * 1'125'899'906'842'624.0;
        default:                  return value;
    }
}

/**
 * @brief Convert data size from bytes
 */
[[nodiscard]] inline double dataFromBytes(double bytes, DataUnit unit) noexcept {
    switch (unit) {
        case DataUnit::Bits:      return bytes * 8.0;
        case DataUnit::Bytes:     return bytes;
        case DataUnit::Kilobytes: return bytes / 1000.0;
        case DataUnit::Megabytes: return bytes / 1'000'000.0;
        case DataUnit::Gigabytes: return bytes / 1'000'000'000.0;
        case DataUnit::Terabytes: return bytes / 1'000'000'000'000.0;
        case DataUnit::Petabytes: return bytes / 1'000'000'000'000'000.0;
        case DataUnit::Kibibytes: return bytes / 1024.0;
        case DataUnit::Mebibytes: return bytes / 1'048'576.0;
        case DataUnit::Gibibytes: return bytes / 1'073'741'824.0;
        case DataUnit::Tebibytes: return bytes / 1'099'511'627'776.0;
        case DataUnit::Pebibytes: return bytes / 1'125'899'906'842'624.0;
        default:                  return bytes;
    }
}

/**
 * @brief Convert data size between units
 */
[[nodiscard]] inline double convertData(double value, DataUnit from, DataUnit to) noexcept {
    double bytes = dataToBytes(value, from);
    return dataFromBytes(bytes, to);
}

// ============================================================================
// Speed Units
// ============================================================================

/**
 * @brief Speed units
 */
enum class SpeedUnit {
    MetersPerSecond,
    KilometersPerHour,
    MilesPerHour,
    FeetPerSecond,
    Knots
};

/**
 * @brief Convert speed to meters per second (base unit)
 */
[[nodiscard]] inline double speedToMPS(double value, SpeedUnit unit) noexcept {
    switch (unit) {
        case SpeedUnit::MetersPerSecond:    return value;
        case SpeedUnit::KilometersPerHour:  return value / 3.6;
        case SpeedUnit::MilesPerHour:       return value * 0.44704;
        case SpeedUnit::FeetPerSecond:      return value * 0.3048;
        case SpeedUnit::Knots:              return value * 0.514444;
        default:                            return value;
    }
}

/**
 * @brief Convert speed from meters per second
 */
[[nodiscard]] inline double speedFromMPS(double mps, SpeedUnit unit) noexcept {
    switch (unit) {
        case SpeedUnit::MetersPerSecond:    return mps;
        case SpeedUnit::KilometersPerHour:  return mps * 3.6;
        case SpeedUnit::MilesPerHour:       return mps / 0.44704;
        case SpeedUnit::FeetPerSecond:      return mps / 0.3048;
        case SpeedUnit::Knots:              return mps / 0.514444;
        default:                            return mps;
    }
}

/**
 * @brief Convert speed between units
 */
[[nodiscard]] inline double convertSpeed(double value, SpeedUnit from, SpeedUnit to) noexcept {
    double mps = speedToMPS(value, from);
    return speedFromMPS(mps, to);
}

// ============================================================================
// Pressure Units
// ============================================================================

/**
 * @brief Pressure units
 */
enum class PressureUnit {
    Pascals,
    Kilopascals,
    Bar,
    Atmospheres,
    PSI,
    Torr,
    MillimetersOfMercury
};

/**
 * @brief Convert pressure to Pascals (base unit)
 */
[[nodiscard]] inline double pressureToPascals(double value, PressureUnit unit) noexcept {
    switch (unit) {
        case PressureUnit::Pascals:             return value;
        case PressureUnit::Kilopascals:         return value * 1000.0;
        case PressureUnit::Bar:                 return value * 100000.0;
        case PressureUnit::Atmospheres:         return value * 101325.0;
        case PressureUnit::PSI:                 return value * 6894.76;
        case PressureUnit::Torr:                return value * 133.322;
        case PressureUnit::MillimetersOfMercury: return value * 133.322;
        default:                                return value;
    }
}

/**
 * @brief Convert pressure from Pascals
 */
[[nodiscard]] inline double pressureFromPascals(double pa, PressureUnit unit) noexcept {
    switch (unit) {
        case PressureUnit::Pascals:             return pa;
        case PressureUnit::Kilopascals:         return pa / 1000.0;
        case PressureUnit::Bar:                 return pa / 100000.0;
        case PressureUnit::Atmospheres:         return pa / 101325.0;
        case PressureUnit::PSI:                 return pa / 6894.76;
        case PressureUnit::Torr:                return pa / 133.322;
        case PressureUnit::MillimetersOfMercury: return pa / 133.322;
        default:                                return pa;
    }
}

/**
 * @brief Convert pressure between units
 */
[[nodiscard]] inline double convertPressure(double value, PressureUnit from, PressureUnit to) noexcept {
    double pa = pressureToPascals(value, from);
    return pressureFromPascals(pa, to);
}

// ============================================================================
// Energy Units
// ============================================================================

/**
 * @brief Energy units
 */
enum class EnergyUnit {
    Joules,
    Kilojoules,
    Calories,
    Kilocalories,
    WattHours,
    KilowattHours,
    BTU,
    Electronvolts
};

/**
 * @brief Convert energy to Joules (base unit)
 */
[[nodiscard]] inline double energyToJoules(double value, EnergyUnit unit) noexcept {
    switch (unit) {
        case EnergyUnit::Joules:        return value;
        case EnergyUnit::Kilojoules:    return value * 1000.0;
        case EnergyUnit::Calories:      return value * 4.184;
        case EnergyUnit::Kilocalories:  return value * 4184.0;
        case EnergyUnit::WattHours:     return value * 3600.0;
        case EnergyUnit::KilowattHours: return value * 3'600'000.0;
        case EnergyUnit::BTU:           return value * 1055.06;
        case EnergyUnit::Electronvolts: return value * 1.60218e-19;
        default:                        return value;
    }
}

/**
 * @brief Convert energy from Joules
 */
[[nodiscard]] inline double energyFromJoules(double joules, EnergyUnit unit) noexcept {
    switch (unit) {
        case EnergyUnit::Joules:        return joules;
        case EnergyUnit::Kilojoules:    return joules / 1000.0;
        case EnergyUnit::Calories:      return joules / 4.184;
        case EnergyUnit::Kilocalories:  return joules / 4184.0;
        case EnergyUnit::WattHours:     return joules / 3600.0;
        case EnergyUnit::KilowattHours: return joules / 3'600'000.0;
        case EnergyUnit::BTU:           return joules / 1055.06;
        case EnergyUnit::Electronvolts: return joules / 1.60218e-19;
        default:                        return joules;
    }
}

/**
 * @brief Convert energy between units
 */
[[nodiscard]] inline double convertEnergy(double value, EnergyUnit from, EnergyUnit to) noexcept {
    double joules = energyToJoules(value, from);
    return energyFromJoules(joules, to);
}

} // namespace proven

#endif // PROVEN_SAFE_UNIT_HPP
