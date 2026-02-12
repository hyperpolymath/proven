# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# SafeConfig - Safe configuration value handling for Nickel
#
# Provides contracts and utilities for common configuration patterns.
# Designed specifically for Nickel's configuration use case.

let SafeMath = import "./safe-math.ncl" in
let SafeString = import "./safe-string.ncl" in
let SafePath = import "./safe-path.ncl" in
let SafeNetwork = import "./safe-network.ncl" in

{
  # Duration in seconds (positive)
  Duration = std.contract.from_predicate (fun d => d > 0),

  # Duration in milliseconds (positive integer)
  DurationMs = std.contract.from_predicate (fun d =>
    d > 0 && d == std.number.truncate d
  ),

  # Memory size in bytes (non-negative integer)
  Bytes = std.contract.from_predicate (fun b =>
    b >= 0 && b == std.number.truncate b
  ),

  # Memory size in kilobytes
  Kilobytes = std.contract.from_predicate (fun kb =>
    kb >= 0 && kb == std.number.truncate kb
  ),

  # Memory size in megabytes
  Megabytes = std.contract.from_predicate (fun mb =>
    mb >= 0 && mb == std.number.truncate mb
  ),

  # Memory size in gigabytes
  Gigabytes = std.contract.from_predicate (fun gb =>
    gb >= 0 && gb == std.number.truncate gb
  ),

  # Thread/worker count (positive integer, reasonable limit)
  WorkerCount = std.contract.from_predicate (fun w =>
    w > 0 && w <= 1024 && w == std.number.truncate w
  ),

  # Retry count (non-negative integer, reasonable limit)
  RetryCount = std.contract.from_predicate (fun r =>
    r >= 0 && r <= 100 && r == std.number.truncate r
  ),

  # Log level enum
  LogLevel = std.contract.from_predicate (fun level =>
    std.array.elem level ["trace", "debug", "info", "warn", "error", "fatal"]
  ),

  # Environment name (development, staging, production)
  Environment = std.contract.from_predicate (fun env =>
    std.array.elem env ["development", "staging", "production", "test"]
  ),

  # URL pattern (basic validation)
  Url = std.contract.from_predicate (fun url =>
    std.string.is_match "^https?://" url
  ),

  # Secure URL (HTTPS only)
  SecureUrl = std.contract.from_predicate (fun url =>
    std.string.is_match "^https://" url
  ),

  # Semantic version pattern
  SemVer = std.contract.from_predicate (fun v =>
    std.string.is_match "^[0-9]+\\.[0-9]+\\.[0-9]+(-[a-zA-Z0-9.-]+)?(\\+[a-zA-Z0-9.-]+)?$" v
  ),

  # Database connection string (basic pattern)
  DatabaseUrl = std.contract.from_predicate (fun url =>
    std.string.is_match "^(postgres|postgresql|mysql|sqlite|mongodb)://" url
  ),

  # Cron expression (basic 5-field validation)
  CronExpression = std.contract.from_predicate (fun cron =>
    let parts = std.string.split " " cron in
    std.array.length parts == 5 || std.array.length parts == 6
  ),

  # Feature flag (boolean with optional metadata support)
  FeatureFlag = Bool,

  # Secret placeholder (non-empty, typically env var reference)
  SecretRef = std.contract.from_predicate (fun s =>
    std.string.length s > 0
    && (std.string.is_match "^\\$\\{" s || std.string.is_match "^env:" s)
  ),

  # Parse duration string (e.g., "30s", "5m", "2h") to seconds
  parse_duration
    : String -> { value : Number, ok : Bool }
    = fun duration =>
      let len = std.string.length duration in
      if len < 2 then
        { value = 0, ok = false }
      else
        let unit = std.string.substring (len - 1) len duration in
        let num_str = std.string.substring 0 (len - 1) duration in
        if !(std.string.is_match "^[0-9]+$" num_str) then
          { value = 0, ok = false }
        else
          let num = std.string.to_number num_str in
          if unit == "s" then { value = num, ok = true }
          else if unit == "m" then { value = num * 60, ok = true }
          else if unit == "h" then { value = num * 3600, ok = true }
          else if unit == "d" then { value = num * 86400, ok = true }
          else { value = 0, ok = false },

  # Parse memory size string (e.g., "512M", "2G") to bytes
  parse_memory
    : String -> { value : Number, ok : Bool }
    = fun size =>
      let len = std.string.length size in
      if len < 2 then
        { value = 0, ok = false }
      else
        let unit = std.string.substring (len - 1) len size in
        let num_str = std.string.substring 0 (len - 1) size in
        if !(std.string.is_match "^[0-9]+$" num_str) then
          { value = 0, ok = false }
        else
          let num = std.string.to_number num_str in
          if unit == "B" then { value = num, ok = true }
          else if unit == "K" then { value = num * 1024, ok = true }
          else if unit == "M" then { value = num * 1048576, ok = true }
          else if unit == "G" then { value = num * 1073741824, ok = true }
          else { value = 0, ok = false },

  # Merge two configs with right precedence (right overwrites left)
  merge_configs
    : { .. } -> { .. } -> { .. }
    = fun base override =>
      std.record.merge base override,

  # Get config value with default fallback
  get_or_default
    : forall a. String -> a -> { .. } -> a
    = fun key default config =>
      if std.record.has_field key config then
        config."%{key}"
      else
        default,
}
