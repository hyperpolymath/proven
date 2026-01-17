// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Length unit conversions.
public enum LengthUnit: String, CaseIterable {
    case millimeters = "mm"
    case centimeters = "cm"
    case meters = "m"
    case kilometers = "km"
    case inches = "in"
    case feet = "ft"
    case yards = "yd"
    case miles = "mi"
    case nauticalMiles = "nmi"

    /// Conversion factor to meters.
    public var toMeters: Double {
        switch self {
        case .millimeters: return 0.001
        case .centimeters: return 0.01
        case .meters: return 1.0
        case .kilometers: return 1000.0
        case .inches: return 0.0254
        case .feet: return 0.3048
        case .yards: return 0.9144
        case .miles: return 1609.344
        case .nauticalMiles: return 1852.0
        }
    }
}

/// Mass unit conversions.
public enum MassUnit: String, CaseIterable {
    case milligrams = "mg"
    case grams = "g"
    case kilograms = "kg"
    case metricTons = "t"
    case ounces = "oz"
    case pounds = "lb"
    case stones = "st"

    /// Conversion factor to kilograms.
    public var toKilograms: Double {
        switch self {
        case .milligrams: return 0.000001
        case .grams: return 0.001
        case .kilograms: return 1.0
        case .metricTons: return 1000.0
        case .ounces: return 0.0283495
        case .pounds: return 0.453592
        case .stones: return 6.35029
        }
    }
}

/// Temperature unit conversions.
public enum TemperatureUnit: String, CaseIterable {
    case celsius = "°C"
    case fahrenheit = "°F"
    case kelvin = "K"
}

/// Time unit conversions.
public enum TimeUnit: String, CaseIterable {
    case milliseconds = "ms"
    case seconds = "s"
    case minutes = "min"
    case hours = "h"
    case days = "d"
    case weeks = "wk"

    /// Conversion factor to seconds.
    public var toSeconds: Double {
        switch self {
        case .milliseconds: return 0.001
        case .seconds: return 1.0
        case .minutes: return 60.0
        case .hours: return 3600.0
        case .days: return 86400.0
        case .weeks: return 604800.0
        }
    }
}

/// Data size unit conversions.
public enum DataUnit: String, CaseIterable {
    case bits = "b"
    case bytes = "B"
    case kilobytes = "KB"
    case megabytes = "MB"
    case gigabytes = "GB"
    case terabytes = "TB"
    case kibibytes = "KiB"
    case mebibytes = "MiB"
    case gibibytes = "GiB"
    case tebibytes = "TiB"

    /// Conversion factor to bytes.
    public var toBytes: Double {
        switch self {
        case .bits: return 0.125
        case .bytes: return 1.0
        case .kilobytes: return 1000.0
        case .megabytes: return 1_000_000.0
        case .gigabytes: return 1_000_000_000.0
        case .terabytes: return 1_000_000_000_000.0
        case .kibibytes: return 1024.0
        case .mebibytes: return 1_048_576.0
        case .gibibytes: return 1_073_741_824.0
        case .tebibytes: return 1_099_511_627_776.0
        }
    }
}

/// Unit conversion utilities.
public enum SafeUnit {
    // MARK: - Length Conversions

    /// Convert length between units.
    public static func convertLength(_ value: Double, from: LengthUnit, to: LengthUnit) -> Double {
        let meters = value * from.toMeters
        return meters / to.toMeters
    }

    // MARK: - Mass Conversions

    /// Convert mass between units.
    public static func convertMass(_ value: Double, from: MassUnit, to: MassUnit) -> Double {
        let kilograms = value * from.toKilograms
        return kilograms / to.toKilograms
    }

    // MARK: - Temperature Conversions

    /// Convert temperature between units.
    public static func convertTemperature(_ value: Double, from: TemperatureUnit, to: TemperatureUnit) -> Double {
        // Convert to Celsius first
        let celsius: Double
        switch from {
        case .celsius:
            celsius = value
        case .fahrenheit:
            celsius = (value - 32) * 5 / 9
        case .kelvin:
            celsius = value - 273.15
        }

        // Convert from Celsius to target
        switch to {
        case .celsius:
            return celsius
        case .fahrenheit:
            return celsius * 9 / 5 + 32
        case .kelvin:
            return celsius + 273.15
        }
    }

    // MARK: - Time Conversions

    /// Convert time between units.
    public static func convertTime(_ value: Double, from: TimeUnit, to: TimeUnit) -> Double {
        let seconds = value * from.toSeconds
        return seconds / to.toSeconds
    }

    // MARK: - Data Size Conversions

    /// Convert data size between units.
    public static func convertData(_ value: Double, from: DataUnit, to: DataUnit) -> Double {
        let bytes = value * from.toBytes
        return bytes / to.toBytes
    }

    // MARK: - Format Helpers

    /// Format length with unit.
    public static func formatLength(_ value: Double, unit: LengthUnit, precision: Int = 2) -> String {
        String(format: "%.\(precision)f %@", value, unit.rawValue)
    }

    /// Format mass with unit.
    public static func formatMass(_ value: Double, unit: MassUnit, precision: Int = 2) -> String {
        String(format: "%.\(precision)f %@", value, unit.rawValue)
    }

    /// Format temperature with unit.
    public static func formatTemperature(_ value: Double, unit: TemperatureUnit, precision: Int = 1) -> String {
        String(format: "%.\(precision)f%@", value, unit.rawValue)
    }

    /// Format time with unit.
    public static func formatTime(_ value: Double, unit: TimeUnit, precision: Int = 2) -> String {
        String(format: "%.\(precision)f %@", value, unit.rawValue)
    }

    /// Format data size with unit.
    public static func formatData(_ value: Double, unit: DataUnit, precision: Int = 2) -> String {
        String(format: "%.\(precision)f %@", value, unit.rawValue)
    }

    /// Auto-format data size to most appropriate unit.
    public static func formatDataAuto(_ bytes: Double, binary: Bool = false) -> String {
        let units: [(Double, String)]
        if binary {
            units = [
                (1_099_511_627_776, "TiB"),
                (1_073_741_824, "GiB"),
                (1_048_576, "MiB"),
                (1024, "KiB"),
                (1, "B")
            ]
        } else {
            units = [
                (1_000_000_000_000, "TB"),
                (1_000_000_000, "GB"),
                (1_000_000, "MB"),
                (1000, "KB"),
                (1, "B")
            ]
        }

        for (threshold, unit) in units {
            if bytes >= threshold {
                let value = bytes / threshold
                return String(format: "%.2f %@", value, unit)
            }
        }

        return String(format: "%.0f B", bytes)
    }
}
