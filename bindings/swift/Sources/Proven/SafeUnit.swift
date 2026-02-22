// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// Physical unit conversions delegated to libproven FFI.
///
/// Length and temperature conversion via the formally verified
/// Idris 2 core.

import CProven

/// Length units supported by SafeUnit.
public enum LengthUnit: UInt32, Sendable {
    case meters = 0
    case kilometers = 1
    case centimeters = 2
    case millimeters = 3
    case feet = 4
    case inches = 5
    case miles = 6
    case yards = 7
}

/// Temperature units supported by SafeUnit.
public enum TempUnit: UInt32, Sendable {
    case celsius = 0
    case fahrenheit = 1
    case kelvin = 2
}

public enum SafeUnit {
    /// Convert length between units.
    public static func convertLength(_ value: Double, from: LengthUnit, to: LengthUnit) -> Result<Double, ProvenError> {
        let result = proven_unit_convert_length(value, ProvenLengthUnit(rawValue: from.rawValue), ProvenLengthUnit(rawValue: to.rawValue))
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }

    /// Convert temperature between units.
    public static func convertTemp(_ value: Double, from: TempUnit, to: TempUnit) -> Result<Double, ProvenError> {
        let result = proven_unit_convert_temp(value, ProvenTempUnit(rawValue: from.rawValue), ProvenTempUnit(rawValue: to.rawValue))
        if let error = ProvenError.fromStatus(result.status) {
            return .failure(error)
        }
        return .success(result.value)
    }
}
