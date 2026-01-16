// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//
// Proven Safety Primitives for Jsonnet
//
// Provides type-safe value constructors and validation functions
// for use in Jsonnet configurations.

{
  // Version info
  version: '0.9.0',

  // Result type for safe operations
  Result:: {
    ok(value):: { ok: true, value: value },
    err(error):: { ok: false, error: error },
    isOk(result):: std.objectHas(result, 'ok') && result.ok == true,
    isErr(result):: std.objectHas(result, 'ok') && result.ok == false,
    unwrap(result):: if self.isOk(result) then result.value else error 'Unwrap on Err: ' + result.error,
    unwrapOr(result, default):: if self.isOk(result) then result.value else default,
    map(result, fn):: if self.isOk(result) then self.ok(fn(result.value)) else result,
  },

  // Safe math operations
  SafeMath:: {
    // Checked addition (Jsonnet uses doubles, so we check for integer overflow)
    add(a, b)::
      local result = a + b;
      if result > 9007199254740991 || result < -9007199254740991 then
        $.Result.err('Integer overflow in addition')
      else
        $.Result.ok(result),

    // Checked subtraction
    sub(a, b)::
      local result = a - b;
      if result > 9007199254740991 || result < -9007199254740991 then
        $.Result.err('Integer overflow in subtraction')
      else
        $.Result.ok(result),

    // Checked multiplication
    mul(a, b)::
      local result = a * b;
      if result > 9007199254740991 || result < -9007199254740991 then
        $.Result.err('Integer overflow in multiplication')
      else
        $.Result.ok(result),

    // Safe division (no divide by zero)
    div(a, b)::
      if b == 0 then
        $.Result.err('Division by zero')
      else
        $.Result.ok(a / b),

    // Clamp to range
    clamp(value, min, max)::
      std.max(min, std.min(max, value)),

    // Safe modulo
    mod(a, b)::
      if b == 0 then
        $.Result.err('Division by zero')
      else
        $.Result.ok(a % b),
  },

  // Safe string operations
  SafeString:: {
    // Validate non-empty
    nonEmpty(s)::
      if std.length(s) == 0 then
        $.Result.err('String cannot be empty')
      else
        $.Result.ok(s),

    // Validate max length
    maxLength(s, max)::
      if std.length(s) > max then
        $.Result.err('String exceeds max length: ' + max)
      else
        $.Result.ok(s),

    // Validate email format (basic)
    email(s)::
      if std.length(std.findSubstr('@', s)) != 1 then
        $.Result.err('Invalid email format')
      else
        $.Result.ok(s),

    // Safe substring
    slice(s, start, end)::
      local len = std.length(s);
      local safeStart = std.max(0, std.min(start, len));
      local safeEnd = std.max(safeStart, std.min(end, len));
      $.Result.ok(std.substr(s, safeStart, safeEnd - safeStart)),

    // Trim whitespace
    trim(s):: std.stripChars(s, ' \t\n\r'),
  },

  // Network safety
  SafeNetwork:: {
    // Validate port range
    port(p)::
      if p < 1 || p > 65535 then
        $.Result.err('Port must be between 1 and 65535')
      else
        $.Result.ok(p),

    // Validate CIDR format (basic)
    cidr(s)::
      local parts = std.split(s, '/');
      if std.length(parts) != 2 then
        $.Result.err('Invalid CIDR format')
      else
        $.Result.ok(s),

    // Common safe port ranges
    ports:: {
      http: 80,
      https: 443,
      ssh: 22,
      dns: 53,
    },
  },

  // Resource constraints for IaC
  Resources:: {
    // Memory size (returns bytes)
    memory(value, unit)::
      local units = {
        'B': 1,
        'KB': 1024,
        'MB': 1024 * 1024,
        'GB': 1024 * 1024 * 1024,
        'TB': 1024 * 1024 * 1024 * 1024,
      };
      if !std.objectHas(units, unit) then
        $.Result.err('Unknown memory unit: ' + unit)
      else if value <= 0 then
        $.Result.err('Memory must be positive')
      else
        $.Result.ok(value * units[unit]),

    // CPU (returns millicores)
    cpu(value, unit)::
      local units = {
        'millicores': 1,
        'm': 1,
        'cores': 1000,
        '': 1000,
      };
      local u = if unit == null then '' else unit;
      if !std.objectHas(units, u) then
        $.Result.err('Unknown CPU unit: ' + u)
      else if value <= 0 then
        $.Result.err('CPU must be positive')
      else
        $.Result.ok(value * units[u]),

    // Validate replica count
    replicas(n, min=1, max=100)::
      if n < min then
        $.Result.err('Replicas below minimum: ' + min)
      else if n > max then
        $.Result.err('Replicas exceed maximum: ' + max)
      else
        $.Result.ok(n),
  },

  // Validation combinators
  Validate:: {
    // All validations must pass
    all(results)::
      local errors = std.filter(function(r) $.Result.isErr(r), results);
      if std.length(errors) > 0 then
        $.Result.err(std.map(function(e) e.error, errors))
      else
        $.Result.ok(std.map(function(r) r.value, results)),

    // At least one must pass
    any(results)::
      local oks = std.filter(function(r) $.Result.isOk(r), results);
      if std.length(oks) > 0 then
        oks[0]
      else
        $.Result.err('All validations failed'),

    // Apply validation to object field
    field(obj, name, validator)::
      if !std.objectHas(obj, name) then
        $.Result.err('Missing field: ' + name)
      else
        validator(obj[name]),
  },
}
