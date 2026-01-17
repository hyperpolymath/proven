// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Safe calculator with overflow protection.
public enum SafeCalculator {
    public enum CalculatorError: Error, Equatable {
        case overflow
        case divisionByZero
        case invalidInput(String)
        case domainError(String)
    }

    /// Safe addition.
    public static func add(_ a: Double, _ b: Double) -> Result<Double, CalculatorError> {
        let result = a + b
        guard result.isFinite else { return .failure(.overflow) }
        return .success(result)
    }

    /// Safe subtraction.
    public static func sub(_ a: Double, _ b: Double) -> Result<Double, CalculatorError> {
        let result = a - b
        guard result.isFinite else { return .failure(.overflow) }
        return .success(result)
    }

    /// Safe multiplication.
    public static func mul(_ a: Double, _ b: Double) -> Result<Double, CalculatorError> {
        let result = a * b
        guard result.isFinite else { return .failure(.overflow) }
        return .success(result)
    }

    /// Safe division.
    public static func div(_ a: Double, _ b: Double) -> Result<Double, CalculatorError> {
        guard b != 0 else { return .failure(.divisionByZero) }
        let result = a / b
        guard result.isFinite else { return .failure(.overflow) }
        return .success(result)
    }

    /// Safe modulo.
    public static func mod(_ a: Double, _ b: Double) -> Result<Double, CalculatorError> {
        guard b != 0 else { return .failure(.divisionByZero) }
        return .success(a.truncatingRemainder(dividingBy: b))
    }

    /// Safe power.
    public static func pow(_ base: Double, _ exponent: Double) -> Result<Double, CalculatorError> {
        let result = Foundation.pow(base, exponent)
        guard result.isFinite else { return .failure(.overflow) }
        return .success(result)
    }

    /// Safe square root.
    public static func sqrt(_ value: Double) -> Result<Double, CalculatorError> {
        guard value >= 0 else { return .failure(.domainError("Cannot take square root of negative number")) }
        return .success(Foundation.sqrt(value))
    }

    /// Safe natural logarithm.
    public static func ln(_ value: Double) -> Result<Double, CalculatorError> {
        guard value > 0 else { return .failure(.domainError("Logarithm requires positive input")) }
        return .success(Foundation.log(value))
    }

    /// Safe log base 10.
    public static func log10(_ value: Double) -> Result<Double, CalculatorError> {
        guard value > 0 else { return .failure(.domainError("Logarithm requires positive input")) }
        return .success(Foundation.log10(value))
    }

    /// Safe log with arbitrary base.
    public static func log(_ value: Double, base: Double) -> Result<Double, CalculatorError> {
        guard value > 0 else { return .failure(.domainError("Logarithm requires positive input")) }
        guard base > 0 && base != 1 else { return .failure(.domainError("Invalid logarithm base")) }
        return .success(Foundation.log(value) / Foundation.log(base))
    }

    /// Safe factorial (for small integers).
    public static func factorial(_ n: Int) -> Result<Double, CalculatorError> {
        guard n >= 0 else { return .failure(.invalidInput("Factorial requires non-negative integer")) }
        guard n <= 170 else { return .failure(.overflow) }

        var result: Double = 1
        for i in 2...max(n, 1) {
            result *= Double(i)
        }
        return .success(result)
    }

    /// Safe permutation P(n, r).
    public static func permutation(n: Int, r: Int) -> Result<Double, CalculatorError> {
        guard n >= 0 && r >= 0 else { return .failure(.invalidInput("Permutation requires non-negative integers")) }
        guard r <= n else { return .failure(.invalidInput("r cannot be greater than n")) }

        var result: Double = 1
        for i in (n - r + 1)...n {
            result *= Double(i)
            guard result.isFinite else { return .failure(.overflow) }
        }
        return .success(result)
    }

    /// Safe combination C(n, r).
    public static func combination(n: Int, r: Int) -> Result<Double, CalculatorError> {
        guard n >= 0 && r >= 0 else { return .failure(.invalidInput("Combination requires non-negative integers")) }
        guard r <= n else { return .failure(.invalidInput("r cannot be greater than n")) }

        var r = r
        if r > n - r {
            r = n - r
        }

        var result: Double = 1
        for i in 0..<r {
            result = result * Double(n - i) / Double(i + 1)
            guard result.isFinite else { return .failure(.overflow) }
        }
        return .success(Foundation.round(result))
    }

    /// Safe GCD using Euclidean algorithm.
    public static func gcd(_ a: Int, _ b: Int) -> Int {
        var a = abs(a)
        var b = abs(b)

        while b != 0 {
            let temp = b
            b = a % b
            a = temp
        }

        return a
    }

    /// Safe LCM.
    public static func lcm(_ a: Int, _ b: Int) -> Result<Int, CalculatorError> {
        let g = gcd(a, b)
        guard g != 0 else { return .success(0) }

        let (result, overflow) = abs(a).multipliedReportingOverflow(by: abs(b) / g)
        if overflow {
            return .failure(.overflow)
        }
        return .success(result)
    }

    /// Check if a number is prime.
    public static func isPrime(_ n: Int) -> Bool {
        guard n >= 2 else { return false }
        if n == 2 { return true }
        if n % 2 == 0 { return false }

        let sqrtN = Int(Foundation.sqrt(Double(n)))
        for i in stride(from: 3, through: sqrtN, by: 2) {
            if n % i == 0 { return false }
        }
        return true
    }

    /// Clamp a value to a range.
    public static func clamp(_ value: Double, min: Double, max: Double) -> Double {
        Swift.max(min, Swift.min(max, value))
    }

    /// Linear interpolation.
    public static func lerp(a: Double, b: Double, t: Double) -> Double {
        a + (b - a) * clamp(t, min: 0, max: 1)
    }

    /// Map a value from one range to another.
    public static func map(value: Double, inMin: Double, inMax: Double, outMin: Double, outMax: Double) -> Result<Double, CalculatorError> {
        guard inMax != inMin else { return .failure(.invalidInput("Input range cannot be zero")) }
        let t = (value - inMin) / (inMax - inMin)
        return .success(outMin + (outMax - outMin) * t)
    }
}

/// Expression parser for evaluating mathematical expressions.
public class ExpressionParser {
    private var constants: [String: Double] = [
        "pi": .pi,
        "e": Darwin.M_E,
        "phi": (1 + Foundation.sqrt(5)) / 2,
        "tau": .pi * 2
    ]

    private var functions: [String: ([Double]) -> Result<Double, SafeCalculator.CalculatorError>] = [:]

    public init() {
        setupDefaultFunctions()
    }

    private func setupDefaultFunctions() {
        functions["sin"] = { args in .success(Foundation.sin(args[0])) }
        functions["cos"] = { args in .success(Foundation.cos(args[0])) }
        functions["tan"] = { args in .success(Foundation.tan(args[0])) }
        functions["asin"] = { args in
            guard args[0] >= -1 && args[0] <= 1 else { return .failure(.domainError("asin domain error")) }
            return .success(Foundation.asin(args[0]))
        }
        functions["acos"] = { args in
            guard args[0] >= -1 && args[0] <= 1 else { return .failure(.domainError("acos domain error")) }
            return .success(Foundation.acos(args[0]))
        }
        functions["atan"] = { args in .success(Foundation.atan(args[0])) }
        functions["sqrt"] = { args in SafeCalculator.sqrt(args[0]) }
        functions["ln"] = { args in SafeCalculator.ln(args[0]) }
        functions["log"] = { args in SafeCalculator.log10(args[0]) }
        functions["abs"] = { args in .success(Foundation.abs(args[0])) }
        functions["ceil"] = { args in .success(Foundation.ceil(args[0])) }
        functions["floor"] = { args in .success(Foundation.floor(args[0])) }
        functions["round"] = { args in .success(Foundation.round(args[0])) }
        functions["exp"] = { args in
            let result = Foundation.exp(args[0])
            guard result.isFinite else { return .failure(.overflow) }
            return .success(result)
        }
        functions["pow"] = { args in SafeCalculator.pow(args[0], args[1]) }
        functions["min"] = { args in .success(args.min() ?? 0) }
        functions["max"] = { args in .success(args.max() ?? 0) }
    }

    /// Register a custom constant.
    public func addConstant(_ name: String, value: Double) {
        constants[name.lowercased()] = value
    }

    /// Register a custom function.
    public func addFunction(_ name: String, fn: @escaping ([Double]) -> Result<Double, SafeCalculator.CalculatorError>) {
        functions[name.lowercased()] = fn
    }

    /// Evaluate a simple expression (basic support).
    public func evaluate(_ expr: String) -> Result<Double, SafeCalculator.CalculatorError> {
        // Remove whitespace
        let cleaned = expr.replacingOccurrences(of: " ", with: "").lowercased()

        // Try to parse as a simple number
        if let value = Double(cleaned) {
            return .success(value)
        }

        // Check if it's a constant
        if let value = constants[cleaned] {
            return .success(value)
        }

        // Basic arithmetic parsing (simplified)
        // This is a basic implementation - a full parser would use proper tokenization

        // Handle addition/subtraction at top level
        if let lastPlus = cleaned.lastIndex(of: "+"), lastPlus != cleaned.startIndex {
            let left = String(cleaned[..<lastPlus])
            let right = String(cleaned[cleaned.index(after: lastPlus)...])

            switch (evaluate(left), evaluate(right)) {
            case (.success(let a), .success(let b)):
                return SafeCalculator.add(a, b)
            case (.failure(let err), _), (_, .failure(let err)):
                return .failure(err)
            }
        }

        // Handle subtraction (but not negative numbers)
        if let lastMinus = cleaned.lastIndex(of: "-"),
           lastMinus != cleaned.startIndex,
           cleaned[cleaned.index(before: lastMinus)] != "e" { // Don't split scientific notation
            let left = String(cleaned[..<lastMinus])
            let right = String(cleaned[cleaned.index(after: lastMinus)...])

            switch (evaluate(left), evaluate(right)) {
            case (.success(let a), .success(let b)):
                return SafeCalculator.sub(a, b)
            case (.failure(let err), _), (_, .failure(let err)):
                return .failure(err)
            }
        }

        // Handle multiplication
        if let lastStar = cleaned.lastIndex(of: "*") {
            let left = String(cleaned[..<lastStar])
            let right = String(cleaned[cleaned.index(after: lastStar)...])

            switch (evaluate(left), evaluate(right)) {
            case (.success(let a), .success(let b)):
                return SafeCalculator.mul(a, b)
            case (.failure(let err), _), (_, .failure(let err)):
                return .failure(err)
            }
        }

        // Handle division
        if let lastSlash = cleaned.lastIndex(of: "/") {
            let left = String(cleaned[..<lastSlash])
            let right = String(cleaned[cleaned.index(after: lastSlash)...])

            switch (evaluate(left), evaluate(right)) {
            case (.success(let a), .success(let b)):
                return SafeCalculator.div(a, b)
            case (.failure(let err), _), (_, .failure(let err)):
                return .failure(err)
            }
        }

        return .failure(.invalidInput("Could not parse expression: \(expr)"))
    }
}
