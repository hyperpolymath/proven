// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package com.hyperpolymath.proven

/**
 * Length units.
 */
enum class LengthUnit(val symbol: String, val toMeters: Double) {
    MILLIMETERS("mm", 0.001),
    CENTIMETERS("cm", 0.01),
    METERS("m", 1.0),
    KILOMETERS("km", 1000.0),
    INCHES("in", 0.0254),
    FEET("ft", 0.3048),
    YARDS("yd", 0.9144),
    MILES("mi", 1609.344),
    NAUTICAL_MILES("nmi", 1852.0)
}

/**
 * Mass units.
 */
enum class MassUnit(val symbol: String, val toKilograms: Double) {
    MILLIGRAMS("mg", 0.000001),
    GRAMS("g", 0.001),
    KILOGRAMS("kg", 1.0),
    METRIC_TONS("t", 1000.0),
    OUNCES("oz", 0.0283495),
    POUNDS("lb", 0.453592),
    STONES("st", 6.35029)
}

/**
 * Temperature units.
 */
enum class TemperatureUnit(val symbol: String) {
    CELSIUS("°C"),
    FAHRENHEIT("°F"),
    KELVIN("K")
}

/**
 * Time units.
 */
enum class TimeUnit(val symbol: String, val toSeconds: Double) {
    MILLISECONDS("ms", 0.001),
    SECONDS("s", 1.0),
    MINUTES("min", 60.0),
    HOURS("h", 3600.0),
    DAYS("d", 86400.0),
    WEEKS("wk", 604800.0)
}

/**
 * Data size units.
 */
enum class DataUnit(val symbol: String, val toBytes: Double) {
    BITS("b", 0.125),
    BYTES("B", 1.0),
    KILOBYTES("KB", 1000.0),
    MEGABYTES("MB", 1_000_000.0),
    GIGABYTES("GB", 1_000_000_000.0),
    TERABYTES("TB", 1_000_000_000_000.0),
    KIBIBYTES("KiB", 1024.0),
    MEBIBYTES("MiB", 1_048_576.0),
    GIBIBYTES("GiB", 1_073_741_824.0),
    TEBIBYTES("TiB", 1_099_511_627_776.0)
}

/**
 * Unit conversion utilities.
 */
object SafeUnit {
    /**
     * Convert length.
     */
    fun convertLength(value: Double, from: LengthUnit, to: LengthUnit): Double {
        val meters = value * from.toMeters
        return meters / to.toMeters
    }

    /**
     * Convert mass.
     */
    fun convertMass(value: Double, from: MassUnit, to: MassUnit): Double {
        val kilograms = value * from.toKilograms
        return kilograms / to.toKilograms
    }

    /**
     * Convert temperature.
     */
    fun convertTemperature(value: Double, from: TemperatureUnit, to: TemperatureUnit): Double {
        // Convert to Celsius first
        val celsius = when (from) {
            TemperatureUnit.CELSIUS -> value
            TemperatureUnit.FAHRENHEIT -> (value - 32) * 5 / 9
            TemperatureUnit.KELVIN -> value - 273.15
        }

        // Convert from Celsius to target
        return when (to) {
            TemperatureUnit.CELSIUS -> celsius
            TemperatureUnit.FAHRENHEIT -> celsius * 9 / 5 + 32
            TemperatureUnit.KELVIN -> celsius + 273.15
        }
    }

    /**
     * Convert time.
     */
    fun convertTime(value: Double, from: TimeUnit, to: TimeUnit): Double {
        val seconds = value * from.toSeconds
        return seconds / to.toSeconds
    }

    /**
     * Convert data size.
     */
    fun convertData(value: Double, from: DataUnit, to: DataUnit): Double {
        val bytes = value * from.toBytes
        return bytes / to.toBytes
    }

    /**
     * Format length with unit.
     */
    fun formatLength(value: Double, unit: LengthUnit, precision: Int = 2): String {
        return "%.${precision}f ${unit.symbol}".format(value)
    }

    /**
     * Format mass with unit.
     */
    fun formatMass(value: Double, unit: MassUnit, precision: Int = 2): String {
        return "%.${precision}f ${unit.symbol}".format(value)
    }

    /**
     * Format temperature with unit.
     */
    fun formatTemperature(value: Double, unit: TemperatureUnit, precision: Int = 1): String {
        return "%.${precision}f${unit.symbol}".format(value)
    }

    /**
     * Format time with unit.
     */
    fun formatTime(value: Double, unit: TimeUnit, precision: Int = 2): String {
        return "%.${precision}f ${unit.symbol}".format(value)
    }

    /**
     * Format data size with unit.
     */
    fun formatData(value: Double, unit: DataUnit, precision: Int = 2): String {
        return "%.${precision}f ${unit.symbol}".format(value)
    }

    /**
     * Auto-format data size to most appropriate unit.
     */
    fun formatDataAuto(bytes: Double, binary: Boolean = false): String {
        val units = if (binary) {
            listOf(
                1_099_511_627_776.0 to "TiB",
                1_073_741_824.0 to "GiB",
                1_048_576.0 to "MiB",
                1024.0 to "KiB",
                1.0 to "B"
            )
        } else {
            listOf(
                1_000_000_000_000.0 to "TB",
                1_000_000_000.0 to "GB",
                1_000_000.0 to "MB",
                1000.0 to "KB",
                1.0 to "B"
            )
        }

        for ((threshold, unit) in units) {
            if (bytes >= threshold) {
                return "%.2f %s".format(bytes / threshold, unit)
            }
        }

        return "%.0f B".format(bytes)
    }
}
