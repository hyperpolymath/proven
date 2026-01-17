// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

import Foundation

/// Safe JSON parsing and manipulation.
public enum SafeJson {
    public enum JsonError: Error, Equatable {
        case invalidJson
        case pathNotFound
        case typeMismatch
        case encodingError
    }

    /// Parse a JSON string into a value.
    public static func parse(_ json: String) -> Result<Any, JsonError> {
        guard let data = json.data(using: .utf8) else {
            return .failure(.invalidJson)
        }
        do {
            let value = try JSONSerialization.jsonObject(with: data, options: [.fragmentsAllowed])
            return .success(value)
        } catch {
            return .failure(.invalidJson)
        }
    }

    /// Stringify a value to JSON.
    public static func stringify(_ value: Any, pretty: Bool = false) -> Result<String, JsonError> {
        guard JSONSerialization.isValidJSONObject(value) || value is String || value is NSNumber else {
            return .failure(.encodingError)
        }
        do {
            let options: JSONSerialization.WritingOptions = pretty ? [.prettyPrinted, .sortedKeys] : []
            let data = try JSONSerialization.data(withJSONObject: value, options: options)
            guard let string = String(data: data, encoding: .utf8) else {
                return .failure(.encodingError)
            }
            return .success(string)
        } catch {
            return .failure(.encodingError)
        }
    }

    /// Get a value at a path (e.g., "user.profile.name").
    public static func getPath(_ value: Any, path: String) -> Result<Any, JsonError> {
        let components = path.split(separator: ".").map(String.init)
        var current = value

        for component in components {
            if let dict = current as? [String: Any] {
                guard let next = dict[component] else {
                    return .failure(.pathNotFound)
                }
                current = next
            } else if let array = current as? [Any], let index = Int(component) {
                guard index >= 0 && index < array.count else {
                    return .failure(.pathNotFound)
                }
                current = array[index]
            } else {
                return .failure(.pathNotFound)
            }
        }

        return .success(current)
    }

    /// Get a string at a path.
    public static func getString(_ value: Any, path: String) -> String? {
        switch getPath(value, path: path) {
        case .success(let result):
            return result as? String
        case .failure:
            return nil
        }
    }

    /// Get an integer at a path.
    public static func getInt(_ value: Any, path: String) -> Int? {
        switch getPath(value, path: path) {
        case .success(let result):
            return result as? Int
        case .failure:
            return nil
        }
    }

    /// Get a double at a path.
    public static func getDouble(_ value: Any, path: String) -> Double? {
        switch getPath(value, path: path) {
        case .success(let result):
            return result as? Double
        case .failure:
            return nil
        }
    }

    /// Get a boolean at a path.
    public static func getBool(_ value: Any, path: String) -> Bool? {
        switch getPath(value, path: path) {
        case .success(let result):
            return result as? Bool
        case .failure:
            return nil
        }
    }

    /// Get an array at a path.
    public static func getArray(_ value: Any, path: String) -> [Any]? {
        switch getPath(value, path: path) {
        case .success(let result):
            return result as? [Any]
        case .failure:
            return nil
        }
    }

    /// Get a dictionary at a path.
    public static func getObject(_ value: Any, path: String) -> [String: Any]? {
        switch getPath(value, path: path) {
        case .success(let result):
            return result as? [String: Any]
        case .failure:
            return nil
        }
    }
}
