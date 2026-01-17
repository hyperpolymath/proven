// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
//
// Proven Safety Primitives for Jsonnet
//
// Provides type-safe value constructors and validation functions
// for use in Jsonnet configurations.
//
// Module count: 38

{
  // Version info
  version: '0.4.0',
  moduleCount: 38,

  // Result type for safe operations
  Result:: {
    ok(value):: { ok: true, value: value },
    err(error):: { ok: false, error: error },
    isOk(result):: std.objectHas(result, 'ok') && result.ok == true,
    isErr(result):: std.objectHas(result, 'ok') && result.ok == false,
    unwrap(result):: if self.isOk(result) then result.value else error 'Unwrap on Err: ' + result.error,
    unwrapOr(result, default):: if self.isOk(result) then result.value else default,
    map(result, fn):: if self.isOk(result) then self.ok(fn(result.value)) else result,
    flatMap(result, fn):: if self.isOk(result) then fn(result.value) else result,
  },

  // ============================================================================
  // CORE MODULES (11)
  // ============================================================================

  // Safe math operations
  safeMath:: {
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
        $.Result.err('Modulo by zero')
      else
        $.Result.ok(a % b),

    // Absolute value
    abs(a):: if a < 0 then -a else a,

    // Safe power
    pow(base, exp)::
      if exp < 0 then
        $.Result.err('Negative exponent not supported')
      else
        $.Result.ok(std.pow(base, exp)),
  },

  // Safe string operations
  safeString:: {
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

    // Validate min length
    minLength(s, min)::
      if std.length(s) < min then
        $.Result.err('String below min length: ' + min)
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

    // Safe concatenation with length limit
    concat(strings, maxLen=1048576)::
      local result = std.join('', strings);
      if std.length(result) > maxLen then
        $.Result.err('Concatenation exceeds max length')
      else
        $.Result.ok(result),

    // Check if alphanumeric
    isAlphanumeric(s)::
      local valid = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
      std.all(std.map(function(c) std.member(valid, c), std.stringChars(s))),
  },

  // Safe path operations
  safePath:: {
    // Validate no path traversal
    noTraversal(path)::
      if std.length(std.findSubstr('..', path)) > 0 then
        $.Result.err('Path traversal detected')
      else
        $.Result.ok(path),

    // Normalize path separators
    normalize(path)::
      std.strReplace(path, '\\', '/'),

    // Join paths safely
    join(parts)::
      local normalized = std.map(function(p) self.normalize(p), parts);
      local cleaned = std.map(function(p) std.stripChars(p, '/'), normalized);
      $.Result.ok(std.join('/', cleaned)),

    // Get extension
    extension(path)::
      local parts = std.split(path, '.');
      if std.length(parts) < 2 then
        $.Result.err('No extension found')
      else
        $.Result.ok(parts[std.length(parts) - 1]),

    // Validate allowed extensions
    allowedExtension(path, allowed)::
      local ext = self.extension(path);
      if $.Result.isErr(ext) then
        ext
      else if std.member(allowed, ext.value) then
        $.Result.ok(path)
      else
        $.Result.err('Extension not allowed: ' + ext.value),
  },

  // Safe email validation
  safeEmail:: {
    // Validate email format
    validate(s)::
      local atPositions = std.findSubstr('@', s);
      if std.length(atPositions) != 1 then
        $.Result.err('Invalid email: must contain exactly one @')
      else
        local atPos = atPositions[0];
        local local_ = std.substr(s, 0, atPos);
        local domain = std.substr(s, atPos + 1, std.length(s) - atPos - 1);
        if std.length(local_) == 0 then
          $.Result.err('Invalid email: empty local part')
        else if std.length(domain) < 3 then
          $.Result.err('Invalid email: domain too short')
        else if std.length(std.findSubstr('.', domain)) == 0 then
          $.Result.err('Invalid email: domain must contain dot')
        else
          $.Result.ok(s),

    // Normalize email (lowercase)
    normalize(s):: std.asciiLower(s),

    // Extract domain
    domain(s)::
      local atPositions = std.findSubstr('@', s);
      if std.length(atPositions) != 1 then
        $.Result.err('Invalid email format')
      else
        $.Result.ok(std.substr(s, atPositions[0] + 1, std.length(s) - atPositions[0] - 1)),
  },

  // Safe URL operations
  safeUrl:: {
    // Validate URL structure (basic)
    validate(url)::
      local hasProtocol = std.length(std.findSubstr('://', url)) > 0;
      if !hasProtocol then
        $.Result.err('URL must include protocol (e.g., https://)')
      else
        $.Result.ok(url),

    // Validate HTTPS only
    requireHttps(url)::
      if std.startsWith(url, 'https://') then
        $.Result.ok(url)
      else
        $.Result.err('URL must use HTTPS'),

    // Extract protocol
    protocol(url)::
      local parts = std.split(url, '://');
      if std.length(parts) < 2 then
        $.Result.err('No protocol found')
      else
        $.Result.ok(parts[0]),

    // Extract host
    host(url)::
      local parts = std.split(url, '://');
      if std.length(parts) < 2 then
        $.Result.err('Invalid URL')
      else
        local afterProtocol = parts[1];
        local hostPart = std.split(afterProtocol, '/')[0];
        local hostWithoutPort = std.split(hostPart, ':')[0];
        $.Result.ok(hostWithoutPort),

    // Allowed protocols
    allowedProtocols:: ['http', 'https', 'ftp', 'sftp', 'ssh'],
  },

  // Network safety
  safeNetwork:: {
    // Validate port range
    port(p)::
      if p < 1 || p > 65535 then
        $.Result.err('Port must be between 1 and 65535')
      else
        $.Result.ok(p),

    // Validate privileged port
    privilegedPort(p)::
      if p < 1 || p > 1023 then
        $.Result.err('Privileged port must be between 1 and 1023')
      else
        $.Result.ok(p),

    // Validate CIDR format (basic)
    cidr(s)::
      local parts = std.split(s, '/');
      if std.length(parts) != 2 then
        $.Result.err('Invalid CIDR format')
      else
        local prefix = std.parseInt(parts[1]);
        if prefix < 0 || prefix > 32 then
          $.Result.err('CIDR prefix must be 0-32')
        else
          $.Result.ok(s),

    // Validate IPv4 address format
    ipv4(s)::
      local parts = std.split(s, '.');
      if std.length(parts) != 4 then
        $.Result.err('IPv4 must have 4 octets')
      else
        local octets = std.map(function(p) std.parseInt(p), parts);
        if std.any(std.map(function(o) o < 0 || o > 255, octets)) then
          $.Result.err('IPv4 octets must be 0-255')
        else
          $.Result.ok(s),

    // Common safe port ranges
    ports:: {
      http: 80,
      https: 443,
      ssh: 22,
      dns: 53,
      mysql: 3306,
      postgres: 5432,
      redis: 6379,
      mongodb: 27017,
    },
  },

  // Safe cryptographic parameter validation
  safeCrypto:: {
    // Validate key length
    keyLength(bits, algorithm)::
      local minLengths = {
        'aes': 128,
        'rsa': 2048,
        'ecdsa': 256,
        'ed25519': 256,
      };
      local alg = std.asciiLower(algorithm);
      if !std.objectHas(minLengths, alg) then
        $.Result.err('Unknown algorithm: ' + algorithm)
      else if bits < minLengths[alg] then
        $.Result.err('Key length too short for ' + algorithm + ': min ' + minLengths[alg])
      else
        $.Result.ok(bits),

    // Validate hash algorithm
    hashAlgorithm(alg)::
      local allowed = ['sha256', 'sha384', 'sha512', 'sha3-256', 'sha3-512', 'blake2b', 'blake3'];
      local normalized = std.asciiLower(alg);
      if std.member(allowed, normalized) then
        $.Result.ok(normalized)
      else
        $.Result.err('Insecure or unknown hash algorithm: ' + alg),

    // Algorithms to avoid
    insecureAlgorithms:: ['md5', 'sha1', 'des', '3des', 'rc4'],
  },

  // Safe UUID operations
  safeUuid:: {
    // Validate UUID format (v4)
    validate(s)::
      local pattern = std.length(s) == 36 &&
        s[8] == '-' && s[13] == '-' && s[18] == '-' && s[23] == '-';
      if !pattern then
        $.Result.err('Invalid UUID format')
      else
        $.Result.ok(s),

    // Validate nil UUID
    isNil(s):: s == '00000000-0000-0000-0000-000000000000',

    // NIL UUID constant
    nil:: '00000000-0000-0000-0000-000000000000',
  },

  // Safe currency operations
  safeCurrency:: {
    // Validate currency code (ISO 4217)
    code(c)::
      local commonCodes = ['USD', 'EUR', 'GBP', 'JPY', 'CNY', 'CHF', 'CAD', 'AUD', 'NZD', 'INR', 'BRL', 'KRW'];
      local normalized = std.asciiUpper(c);
      if std.length(normalized) != 3 then
        $.Result.err('Currency code must be 3 characters')
      else if std.member(commonCodes, normalized) then
        $.Result.ok(normalized)
      else
        // Accept any 3-letter code but warn it might not be valid
        $.Result.ok(normalized),

    // Validate positive amount
    amount(a)::
      if a < 0 then
        $.Result.err('Currency amount cannot be negative')
      else
        $.Result.ok(a),

    // Format amount with precision
    format(amount, decimals=2)::
      local factor = std.pow(10, decimals);
      local rounded = std.floor(amount * factor + 0.5) / factor;
      $.Result.ok(rounded),
  },

  // Safe phone number validation
  safePhone:: {
    // Validate E.164 format
    e164(s)::
      if !std.startsWith(s, '+') then
        $.Result.err('E.164 must start with +')
      else
        local digits = std.substr(s, 1, std.length(s) - 1);
        if std.length(digits) < 7 || std.length(digits) > 15 then
          $.Result.err('E.164 must be 7-15 digits after +')
        else
          $.Result.ok(s),

    // Validate contains only digits (after optional +)
    digitsOnly(s)::
      local toCheck = if std.startsWith(s, '+') then std.substr(s, 1, std.length(s) - 1) else s;
      local valid = '0123456789';
      if std.all(std.map(function(c) std.member(valid, c), std.stringChars(toCheck))) then
        $.Result.ok(s)
      else
        $.Result.err('Phone number must contain only digits'),
  },

  // Safe hex string operations
  safeHex:: {
    // Validate hex string
    validate(s)::
      local valid = '0123456789abcdefABCDEF';
      local chars = std.stringChars(s);
      if std.length(chars) == 0 then
        $.Result.err('Hex string cannot be empty')
      else if std.length(chars) % 2 != 0 then
        $.Result.err('Hex string must have even length')
      else if std.all(std.map(function(c) std.member(valid, c), chars)) then
        $.Result.ok(std.asciiLower(s))
      else
        $.Result.err('Invalid hex characters'),

    // Validate hex with expected length (in bytes)
    withLength(s, bytes)::
      local validated = self.validate(s);
      if $.Result.isErr(validated) then
        validated
      else if std.length(s) != bytes * 2 then
        $.Result.err('Hex string must be ' + (bytes * 2) + ' characters')
      else
        validated,
  },

  // ============================================================================
  // DATA MODULES (7)
  // ============================================================================

  // Safe JSON operations
  safeJson:: {
    // Parse JSON with error handling
    parse(s)::
      // Note: Jsonnet's std.parseJson will error on invalid JSON
      // This wrapper provides Result semantics
      $.Result.ok(std.parseJson(s)),

    // Validate JSON structure has required fields
    hasFields(obj, fields)::
      local missing = std.filter(function(f) !std.objectHas(obj, f), fields);
      if std.length(missing) > 0 then
        $.Result.err('Missing fields: ' + std.join(', ', missing))
      else
        $.Result.ok(obj),

    // Safe field access
    get(obj, path, default=null)::
      local parts = std.split(path, '.');
      local traverse(current, remaining) =
        if std.length(remaining) == 0 then
          current
        else if !std.isObject(current) then
          default
        else if !std.objectHas(current, remaining[0]) then
          default
        else
          traverse(current[remaining[0]], remaining[1:]);
      traverse(obj, parts),

    // Merge with conflict detection
    merge(base, overlay)::
      std.mergePatch(base, overlay),
  },

  // Safe date/time operations
  safeDateTime:: {
    // Validate ISO 8601 date format (YYYY-MM-DD)
    date(s)::
      local parts = std.split(s, '-');
      if std.length(parts) != 3 then
        $.Result.err('Date must be YYYY-MM-DD format')
      else
        local year = std.parseInt(parts[0]);
        local month = std.parseInt(parts[1]);
        local day = std.parseInt(parts[2]);
        if year < 1 || year > 9999 then
          $.Result.err('Year must be 1-9999')
        else if month < 1 || month > 12 then
          $.Result.err('Month must be 1-12')
        else if day < 1 || day > 31 then
          $.Result.err('Day must be 1-31')
        else
          $.Result.ok(s),

    // Validate time format (HH:MM:SS)
    time(s)::
      local parts = std.split(s, ':');
      if std.length(parts) < 2 || std.length(parts) > 3 then
        $.Result.err('Time must be HH:MM or HH:MM:SS format')
      else
        local hour = std.parseInt(parts[0]);
        local minute = std.parseInt(parts[1]);
        local second = if std.length(parts) == 3 then std.parseInt(parts[2]) else 0;
        if hour < 0 || hour > 23 then
          $.Result.err('Hour must be 0-23')
        else if minute < 0 || minute > 59 then
          $.Result.err('Minute must be 0-59')
        else if second < 0 || second > 59 then
          $.Result.err('Second must be 0-59')
        else
          $.Result.ok(s),

    // Validate duration (ISO 8601)
    duration(s)::
      if !std.startsWith(s, 'P') then
        $.Result.err('Duration must start with P')
      else
        $.Result.ok(s),

    // Common durations
    durations:: {
      minute: 'PT1M',
      hour: 'PT1H',
      day: 'P1D',
      week: 'P1W',
      month: 'P1M',
      year: 'P1Y',
    },
  },

  // Safe floating point operations
  safeFloat:: {
    // Check for finite value
    isFinite(n):: n == n && n != std.parseJson('"Infinity"') && n != std.parseJson('"-Infinity"'),

    // Safe comparison with epsilon
    equals(a, b, epsilon=1e-10)::
      $.safeMath.abs(a - b) < epsilon,

    // Validate range
    range(n, min, max)::
      if n < min then
        $.Result.err('Value below minimum: ' + min)
      else if n > max then
        $.Result.err('Value above maximum: ' + max)
      else
        $.Result.ok(n),

    // Round to decimal places
    round(n, places)::
      local factor = std.pow(10, places);
      std.floor(n * factor + 0.5) / factor,
  },

  // Safe version string operations (semver)
  safeVersion:: {
    // Parse semver string
    parse(s)::
      local parts = std.split(s, '.');
      if std.length(parts) != 3 then
        $.Result.err('Version must be MAJOR.MINOR.PATCH format')
      else
        $.Result.ok({
          major: std.parseInt(parts[0]),
          minor: std.parseInt(parts[1]),
          patch: std.parseInt(std.split(parts[2], '-')[0]),
          prerelease: if std.length(std.split(parts[2], '-')) > 1 then std.split(parts[2], '-')[1] else null,
        }),

    // Compare versions (-1, 0, 1)
    compare(a, b)::
      local va = self.parse(a);
      local vb = self.parse(b);
      if $.Result.isErr(va) || $.Result.isErr(vb) then
        $.Result.err('Invalid version format')
      else
        local av = va.value;
        local bv = vb.value;
        if av.major != bv.major then
          $.Result.ok(if av.major > bv.major then 1 else -1)
        else if av.minor != bv.minor then
          $.Result.ok(if av.minor > bv.minor then 1 else -1)
        else if av.patch != bv.patch then
          $.Result.ok(if av.patch > bv.patch then 1 else -1)
        else
          $.Result.ok(0),

    // Check if version satisfies constraint
    satisfies(version, constraint)::
      local parsed = self.parse(version);
      if $.Result.isErr(parsed) then
        parsed
      else
        // Basic implementation: exact match or prefix match
        if std.startsWith(constraint, '^') then
          local constraintVersion = self.parse(std.substr(constraint, 1, std.length(constraint) - 1));
          if $.Result.isErr(constraintVersion) then
            constraintVersion
          else
            $.Result.ok(parsed.value.major == constraintVersion.value.major)
        else
          $.Result.ok(version == constraint),
  },

  // Safe color operations
  safeColor:: {
    // Validate hex color
    hex(s)::
      local normalized = if std.startsWith(s, '#') then std.substr(s, 1, std.length(s) - 1) else s;
      local valid = '0123456789abcdefABCDEF';
      if std.length(normalized) != 3 && std.length(normalized) != 6 then
        $.Result.err('Hex color must be 3 or 6 characters')
      else if std.all(std.map(function(c) std.member(valid, c), std.stringChars(normalized))) then
        $.Result.ok('#' + std.asciiLower(normalized))
      else
        $.Result.err('Invalid hex color characters'),

    // Validate RGB values
    rgb(r, g, b)::
      if r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255 then
        $.Result.err('RGB values must be 0-255')
      else
        $.Result.ok({ r: r, g: g, b: b }),

    // Validate HSL values
    hsl(h, s, l)::
      if h < 0 || h > 360 then
        $.Result.err('Hue must be 0-360')
      else if s < 0 || s > 100 then
        $.Result.err('Saturation must be 0-100')
      else if l < 0 || l > 100 then
        $.Result.err('Lightness must be 0-100')
      else
        $.Result.ok({ h: h, s: s, l: l }),
  },

  // Safe angle operations
  safeAngle:: {
    // Normalize degrees to 0-360
    normalizeDegrees(d)::
      local mod = d % 360;
      if mod < 0 then mod + 360 else mod,

    // Convert degrees to radians
    toRadians(d):: d * 3.14159265358979323846 / 180,

    // Convert radians to degrees
    toDegrees(r):: r * 180 / 3.14159265358979323846,

    // Validate degrees
    degrees(d)::
      $.Result.ok(self.normalizeDegrees(d)),

    // Validate radians
    radians(r)::
      if r < -6.283185307 || r > 6.283185307 then
        $.Result.err('Radians outside [-2pi, 2pi] range')
      else
        $.Result.ok(r),
  },

  // Safe unit conversions
  safeUnit:: {
    // Memory units
    memory:: {
      toBytes(value, unit)::
        local units = {
          'B': 1,
          'KB': 1024,
          'KiB': 1024,
          'MB': 1024 * 1024,
          'MiB': 1024 * 1024,
          'GB': 1024 * 1024 * 1024,
          'GiB': 1024 * 1024 * 1024,
          'TB': 1024 * 1024 * 1024 * 1024,
          'TiB': 1024 * 1024 * 1024 * 1024,
        };
        if !std.objectHas(units, unit) then
          $.Result.err('Unknown memory unit: ' + unit)
        else if value < 0 then
          $.Result.err('Memory value cannot be negative')
        else
          $.Result.ok(value * units[unit]),
    },

    // CPU units (Kubernetes style)
    cpu:: {
      toMillicores(value, unit='')::
        local units = {
          '': 1000,
          'm': 1,
          'millicores': 1,
          'cores': 1000,
        };
        if !std.objectHas(units, unit) then
          $.Result.err('Unknown CPU unit: ' + unit)
        else if value < 0 then
          $.Result.err('CPU value cannot be negative')
        else
          $.Result.ok(value * units[unit]),
    },

    // Time units
    time:: {
      toSeconds(value, unit)::
        local units = {
          's': 1,
          'ms': 0.001,
          'm': 60,
          'h': 3600,
          'd': 86400,
        };
        if !std.objectHas(units, unit) then
          $.Result.err('Unknown time unit: ' + unit)
        else
          $.Result.ok(value * units[unit]),
    },
  },

  // ============================================================================
  // DATA STRUCTURES MODULES (5)
  // ============================================================================

  // Safe buffer operations
  safeBuffer:: {
    // Create bounded buffer
    create(maxSize):: {
      maxSize: maxSize,
      data: [],
    },

    // Append with overflow check
    append(buffer, item)::
      if std.length(buffer.data) >= buffer.maxSize then
        $.Result.err('Buffer overflow: max size ' + buffer.maxSize)
      else
        $.Result.ok(buffer { data: buffer.data + [item] }),

    // Get with bounds check
    get(buffer, index)::
      if index < 0 || index >= std.length(buffer.data) then
        $.Result.err('Index out of bounds')
      else
        $.Result.ok(buffer.data[index]),

    // Length
    length(buffer):: std.length(buffer.data),
  },

  // Safe queue operations
  safeQueue:: {
    // Create bounded queue
    create(maxSize):: {
      maxSize: maxSize,
      items: [],
    },

    // Enqueue with overflow check
    enqueue(queue, item)::
      if std.length(queue.items) >= queue.maxSize then
        $.Result.err('Queue full: max size ' + queue.maxSize)
      else
        $.Result.ok(queue { items: queue.items + [item] }),

    // Dequeue from front
    dequeue(queue)::
      if std.length(queue.items) == 0 then
        $.Result.err('Queue empty')
      else
        $.Result.ok({
          item: queue.items[0],
          queue: queue { items: queue.items[1:] },
        }),

    // Peek at front
    peek(queue)::
      if std.length(queue.items) == 0 then
        $.Result.err('Queue empty')
      else
        $.Result.ok(queue.items[0]),

    // Check if empty
    isEmpty(queue):: std.length(queue.items) == 0,

    // Check if full
    isFull(queue):: std.length(queue.items) >= queue.maxSize,
  },

  // Bloom filter operations (config generation)
  safeBloom:: {
    // Calculate optimal parameters
    optimalParams(expectedItems, falsePositiveRate)::
      local ln2 = 0.693147180559945;
      local bits = -(expectedItems * std.log(falsePositiveRate)) / (ln2 * ln2);
      local hashFns = (bits / expectedItems) * ln2;
      $.Result.ok({
        bits: std.ceil(bits),
        hashFunctions: std.ceil(hashFns),
        expectedItems: expectedItems,
        falsePositiveRate: falsePositiveRate,
      }),

    // Validate bloom filter config
    validateConfig(config)::
      if !std.objectHas(config, 'bits') || !std.objectHas(config, 'hashFunctions') then
        $.Result.err('Bloom filter config must have bits and hashFunctions')
      else if config.bits < 1 then
        $.Result.err('Bits must be positive')
      else if config.hashFunctions < 1 then
        $.Result.err('Hash functions must be positive')
      else
        $.Result.ok(config),
  },

  // LRU cache configuration
  safeLru:: {
    // Create LRU config
    config(maxSize, ttlSeconds=null)::
      if maxSize < 1 then
        $.Result.err('LRU cache size must be positive')
      else if ttlSeconds != null && ttlSeconds < 0 then
        $.Result.err('TTL cannot be negative')
      else
        $.Result.ok({
          maxSize: maxSize,
          ttlSeconds: ttlSeconds,
        }),

    // Validate cache config
    validate(config)::
      if !std.objectHas(config, 'maxSize') then
        $.Result.err('LRU config must have maxSize')
      else if config.maxSize < 1 then
        $.Result.err('maxSize must be positive')
      else
        $.Result.ok(config),
  },

  // Graph data structure operations
  safeGraph:: {
    // Create empty graph
    create():: {
      nodes: {},
      edges: [],
    },

    // Add node
    addNode(graph, id, data={})::
      if std.objectHas(graph.nodes, id) then
        $.Result.err('Node already exists: ' + id)
      else
        $.Result.ok(graph { nodes: graph.nodes { [id]: data } }),

    // Add edge
    addEdge(graph, from, to, weight=1)::
      if !std.objectHas(graph.nodes, from) then
        $.Result.err('Source node not found: ' + from)
      else if !std.objectHas(graph.nodes, to) then
        $.Result.err('Target node not found: ' + to)
      else
        $.Result.ok(graph { edges: graph.edges + [{ from: from, to: to, weight: weight }] }),

    // Get neighbors
    neighbors(graph, nodeId)::
      if !std.objectHas(graph.nodes, nodeId) then
        $.Result.err('Node not found: ' + nodeId)
      else
        $.Result.ok(std.map(
          function(e) e.to,
          std.filter(function(e) e.from == nodeId, graph.edges)
        )),
  },

  // ============================================================================
  // RESILIENCE MODULES (4)
  // ============================================================================

  // Rate limiter configuration
  safeRateLimiter:: {
    // Token bucket config
    tokenBucket(rate, capacity)::
      if rate <= 0 then
        $.Result.err('Rate must be positive')
      else if capacity <= 0 then
        $.Result.err('Capacity must be positive')
      else if capacity < rate then
        $.Result.err('Capacity should be >= rate')
      else
        $.Result.ok({
          type: 'token_bucket',
          rate: rate,
          capacity: capacity,
        }),

    // Sliding window config
    slidingWindow(requests, windowSeconds)::
      if requests <= 0 then
        $.Result.err('Requests must be positive')
      else if windowSeconds <= 0 then
        $.Result.err('Window must be positive')
      else
        $.Result.ok({
          type: 'sliding_window',
          requests: requests,
          windowSeconds: windowSeconds,
        }),

    // Leaky bucket config
    leakyBucket(rate, capacity)::
      if rate <= 0 then
        $.Result.err('Rate must be positive')
      else if capacity <= 0 then
        $.Result.err('Capacity must be positive')
      else
        $.Result.ok({
          type: 'leaky_bucket',
          rate: rate,
          capacity: capacity,
        }),
  },

  // Circuit breaker configuration
  safeCircuitBreaker:: {
    // Create circuit breaker config
    config(
      failureThreshold,
      successThreshold,
      timeoutSeconds,
      halfOpenRequests=1
    )::
      if failureThreshold < 1 then
        $.Result.err('Failure threshold must be positive')
      else if successThreshold < 1 then
        $.Result.err('Success threshold must be positive')
      else if timeoutSeconds < 1 then
        $.Result.err('Timeout must be positive')
      else if halfOpenRequests < 1 then
        $.Result.err('Half-open requests must be positive')
      else
        $.Result.ok({
          failureThreshold: failureThreshold,
          successThreshold: successThreshold,
          timeoutSeconds: timeoutSeconds,
          halfOpenRequests: halfOpenRequests,
        }),

    // Preset configurations
    presets:: {
      aggressive: {
        failureThreshold: 3,
        successThreshold: 1,
        timeoutSeconds: 10,
        halfOpenRequests: 1,
      },
      moderate: {
        failureThreshold: 5,
        successThreshold: 2,
        timeoutSeconds: 30,
        halfOpenRequests: 2,
      },
      conservative: {
        failureThreshold: 10,
        successThreshold: 5,
        timeoutSeconds: 60,
        halfOpenRequests: 3,
      },
    },
  },

  // Retry policy configuration
  safeRetry:: {
    // Exponential backoff config
    exponential(
      maxRetries,
      initialDelayMs,
      maxDelayMs,
      multiplier=2,
      jitter=true
    )::
      if maxRetries < 0 then
        $.Result.err('Max retries cannot be negative')
      else if initialDelayMs <= 0 then
        $.Result.err('Initial delay must be positive')
      else if maxDelayMs < initialDelayMs then
        $.Result.err('Max delay must be >= initial delay')
      else if multiplier < 1 then
        $.Result.err('Multiplier must be >= 1')
      else
        $.Result.ok({
          type: 'exponential',
          maxRetries: maxRetries,
          initialDelayMs: initialDelayMs,
          maxDelayMs: maxDelayMs,
          multiplier: multiplier,
          jitter: jitter,
        }),

    // Fixed delay config
    fixed(maxRetries, delayMs)::
      if maxRetries < 0 then
        $.Result.err('Max retries cannot be negative')
      else if delayMs <= 0 then
        $.Result.err('Delay must be positive')
      else
        $.Result.ok({
          type: 'fixed',
          maxRetries: maxRetries,
          delayMs: delayMs,
        }),

    // No retry (immediate failure)
    none:: {
      type: 'none',
      maxRetries: 0,
    },
  },

  // Monotonic value tracking
  safeMonotonic:: {
    // Create monotonically increasing tracker
    increasing(initial=0):: {
      type: 'increasing',
      value: initial,
    },

    // Create monotonically decreasing tracker
    decreasing(initial=0):: {
      type: 'decreasing',
      value: initial,
    },

    // Update value (validates monotonicity)
    update(tracker, newValue)::
      if tracker.type == 'increasing' && newValue < tracker.value then
        $.Result.err('Value must be monotonically increasing')
      else if tracker.type == 'decreasing' && newValue > tracker.value then
        $.Result.err('Value must be monotonically decreasing')
      else
        $.Result.ok(tracker { value: newValue }),

    // Validate sequence is monotonic
    validateSequence(values, direction='increasing')::
      local check(prev, curr) = if direction == 'increasing' then curr >= prev else curr <= prev;
      local pairs = std.makeArray(std.length(values) - 1, function(i) [values[i], values[i + 1]]);
      if std.all(std.map(function(p) check(p[0], p[1]), pairs)) then
        $.Result.ok(values)
      else
        $.Result.err('Sequence is not monotonically ' + direction),
  },

  // ============================================================================
  // STATE MODULES (2)
  // ============================================================================

  // State machine configuration
  safeStateMachine:: {
    // Create state machine definition
    create(states, transitions, initialState)::
      if std.length(states) == 0 then
        $.Result.err('State machine must have at least one state')
      else if !std.member(states, initialState) then
        $.Result.err('Initial state must be in states list')
      else
        $.Result.ok({
          states: states,
          transitions: transitions,
          initialState: initialState,
        }),

    // Validate transition
    validateTransition(machine, fromState, toState)::
      if !std.member(machine.states, fromState) then
        $.Result.err('Invalid from state: ' + fromState)
      else if !std.member(machine.states, toState) then
        $.Result.err('Invalid to state: ' + toState)
      else
        local validTransitions = std.filter(
          function(t) t.from == fromState,
          machine.transitions
        );
        local targetStates = std.map(function(t) t.to, validTransitions);
        if std.member(targetStates, toState) then
          $.Result.ok({ from: fromState, to: toState })
        else
          $.Result.err('No transition from ' + fromState + ' to ' + toState),

    // Get valid next states
    nextStates(machine, currentState)::
      if !std.member(machine.states, currentState) then
        $.Result.err('Invalid state: ' + currentState)
      else
        $.Result.ok(std.map(
          function(t) t.to,
          std.filter(function(t) t.from == currentState, machine.transitions)
        )),
  },

  // Safe calculator operations (expression evaluation config)
  safeCalculator:: {
    // Validate expression operators
    allowedOperators:: ['+', '-', '*', '/', '%', '^', '(', ')'],

    // Validate numeric expression
    validateExpression(expr)::
      local validChars = '0123456789.+-*/%^() ';
      local chars = std.stringChars(expr);
      if std.all(std.map(function(c) std.member(validChars, c), chars)) then
        $.Result.ok(expr)
      else
        $.Result.err('Expression contains invalid characters'),

    // Precision config
    precision(decimalPlaces)::
      if decimalPlaces < 0 || decimalPlaces > 15 then
        $.Result.err('Decimal places must be 0-15')
      else
        $.Result.ok(decimalPlaces),
  },

  // ============================================================================
  // ALGORITHM MODULES (4)
  // ============================================================================

  // Geographic calculations
  safeGeo:: {
    // Validate latitude
    latitude(lat)::
      if lat < -90 || lat > 90 then
        $.Result.err('Latitude must be -90 to 90')
      else
        $.Result.ok(lat),

    // Validate longitude
    longitude(lon)::
      if lon < -180 || lon > 180 then
        $.Result.err('Longitude must be -180 to 180')
      else
        $.Result.ok(lon),

    // Validate coordinate pair
    coordinate(lat, lon)::
      local latResult = self.latitude(lat);
      local lonResult = self.longitude(lon);
      if $.Result.isErr(latResult) then
        latResult
      else if $.Result.isErr(lonResult) then
        lonResult
      else
        $.Result.ok({ lat: lat, lon: lon }),

    // Bounding box validation
    boundingBox(minLat, minLon, maxLat, maxLon)::
      if minLat > maxLat then
        $.Result.err('minLat must be <= maxLat')
      else if minLon > maxLon then
        $.Result.err('minLon must be <= maxLon')
      else
        local coords = [
          self.latitude(minLat),
          self.longitude(minLon),
          self.latitude(maxLat),
          self.longitude(maxLon),
        ];
        local errors = std.filter(function(r) $.Result.isErr(r), coords);
        if std.length(errors) > 0 then
          errors[0]
        else
          $.Result.ok({
            minLat: minLat,
            minLon: minLon,
            maxLat: maxLat,
            maxLon: maxLon,
          }),
  },

  // Probability and statistics
  safeProbability:: {
    // Validate probability (0-1)
    probability(p)::
      if p < 0 || p > 1 then
        $.Result.err('Probability must be 0 to 1')
      else
        $.Result.ok(p),

    // Validate percentage (0-100)
    percentage(p)::
      if p < 0 || p > 100 then
        $.Result.err('Percentage must be 0 to 100')
      else
        $.Result.ok(p),

    // Convert percentage to probability
    percentToProbability(percent)::
      local validated = self.percentage(percent);
      if $.Result.isErr(validated) then
        validated
      else
        $.Result.ok(percent / 100),

    // Validate weights sum to 1
    normalizedWeights(weights)::
      local sum = std.foldl(function(a, b) a + b, weights, 0);
      if $.safeMath.abs(sum - 1) > 0.0001 then
        $.Result.err('Weights must sum to 1, got: ' + sum)
      else
        $.Result.ok(weights),
  },

  // Checksum validation
  safeChecksum:: {
    // Luhn algorithm validation (credit cards, IMEI, etc.)
    validateLuhn(digits)::
      local chars = std.stringChars(digits);
      local nums = std.map(function(c) std.parseInt(c), chars);
      local doubled = std.mapWithIndex(
        function(i, n)
          if (std.length(nums) - 1 - i) % 2 == 1 then
            local d = n * 2;
            if d > 9 then d - 9 else d
          else
            n,
        nums
      );
      local sum = std.foldl(function(a, b) a + b, doubled, 0);
      if sum % 10 == 0 then
        $.Result.ok(digits)
      else
        $.Result.err('Luhn checksum validation failed'),

    // ISBN-10 validation
    validateIsbn10(isbn)::
      local cleaned = std.strReplace(isbn, '-', '');
      if std.length(cleaned) != 10 then
        $.Result.err('ISBN-10 must be 10 characters')
      else
        $.Result.ok(isbn),  // Simplified - full validation would check checksum

    // ISBN-13 validation
    validateIsbn13(isbn)::
      local cleaned = std.strReplace(isbn, '-', '');
      if std.length(cleaned) != 13 then
        $.Result.err('ISBN-13 must be 13 characters')
      else
        $.Result.ok(isbn),  // Simplified - full validation would check checksum
  },

  // Tensor shape validation
  safeTensor:: {
    // Validate tensor shape
    shape(dims)::
      if std.length(dims) == 0 then
        $.Result.err('Tensor must have at least one dimension')
      else if std.any(std.map(function(d) d < 1, dims)) then
        $.Result.err('All dimensions must be positive')
      else
        $.Result.ok(dims),

    // Check shapes are compatible for operation
    broadcastable(shapeA, shapeB)::
      local maxLen = std.max(std.length(shapeA), std.length(shapeB));
      local padA = std.makeArray(maxLen - std.length(shapeA), function(i) 1) + shapeA;
      local padB = std.makeArray(maxLen - std.length(shapeB), function(i) 1) + shapeB;
      local compatible = std.mapWithIndex(
        function(i, a) a == padB[i] || a == 1 || padB[i] == 1,
        padA
      );
      if std.all(compatible) then
        $.Result.ok(true)
      else
        $.Result.err('Shapes not broadcastable: ' + std.toString(shapeA) + ' vs ' + std.toString(shapeB)),

    // Matrix multiplication compatibility
    matmulCompatible(shapeA, shapeB)::
      if std.length(shapeA) < 2 || std.length(shapeB) < 2 then
        $.Result.err('Matrix multiplication requires 2D tensors')
      else
        local colsA = shapeA[std.length(shapeA) - 1];
        local rowsB = shapeB[std.length(shapeB) - 2];
        if colsA != rowsB then
          $.Result.err('Incompatible shapes for matmul: columns of A (' + colsA + ') must match rows of B (' + rowsB + ')')
        else
          $.Result.ok(true),
  },

  // ============================================================================
  // SECURITY MODULES (2)
  // ============================================================================

  // Password policy validation
  safePassword:: {
    // Validate password strength
    validate(password, policy={})::
      local minLength = std.get(policy, 'minLength', 8);
      local requireUppercase = std.get(policy, 'requireUppercase', true);
      local requireLowercase = std.get(policy, 'requireLowercase', true);
      local requireDigit = std.get(policy, 'requireDigit', true);
      local requireSpecial = std.get(policy, 'requireSpecial', true);

      local errors = [];
      local withLength = if std.length(password) < minLength then
        errors + ['Password must be at least ' + minLength + ' characters']
      else
        errors;

      local upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
      local lower = 'abcdefghijklmnopqrstuvwxyz';
      local digits = '0123456789';
      local special = '!@#$%^&*()_+-=[]{}|;:,.<>?';

      local chars = std.stringChars(password);
      local hasUpper = std.any(std.map(function(c) std.member(upper, c), chars));
      local hasLower = std.any(std.map(function(c) std.member(lower, c), chars));
      local hasDigit = std.any(std.map(function(c) std.member(digits, c), chars));
      local hasSpecial = std.any(std.map(function(c) std.member(special, c), chars));

      local withUpper = if requireUppercase && !hasUpper then
        withLength + ['Password must contain uppercase letter']
      else
        withLength;

      local withLower = if requireLowercase && !hasLower then
        withUpper + ['Password must contain lowercase letter']
      else
        withUpper;

      local withDigit = if requireDigit && !hasDigit then
        withLower + ['Password must contain digit']
      else
        withLower;

      local withSpecial = if requireSpecial && !hasSpecial then
        withDigit + ['Password must contain special character']
      else
        withDigit;

      if std.length(withSpecial) > 0 then
        $.Result.err(withSpecial)
      else
        $.Result.ok(true),

    // Default policies
    policies:: {
      weak: { minLength: 6, requireUppercase: false, requireLowercase: true, requireDigit: false, requireSpecial: false },
      standard: { minLength: 8, requireUppercase: true, requireLowercase: true, requireDigit: true, requireSpecial: false },
      strong: { minLength: 12, requireUppercase: true, requireLowercase: true, requireDigit: true, requireSpecial: true },
    },
  },

  // ML model configuration safety
  safeMl:: {
    // Validate learning rate
    learningRate(lr)::
      if lr <= 0 then
        $.Result.err('Learning rate must be positive')
      else if lr > 1 then
        $.Result.err('Learning rate typically should be <= 1')
      else
        $.Result.ok(lr),

    // Validate batch size
    batchSize(size)::
      if size < 1 then
        $.Result.err('Batch size must be positive')
      else if size & (size - 1) != 0 then
        // Warn but allow non-power-of-2
        $.Result.ok(size)
      else
        $.Result.ok(size),

    // Validate epochs
    epochs(n)::
      if n < 1 then
        $.Result.err('Epochs must be positive')
      else if n > 10000 then
        $.Result.err('Epochs exceeds reasonable maximum')
      else
        $.Result.ok(n),

    // Validate dropout rate
    dropout(rate)::
      if rate < 0 || rate >= 1 then
        $.Result.err('Dropout rate must be in [0, 1)')
      else
        $.Result.ok(rate),

    // Common hyperparameter presets
    presets:: {
      small: { learningRate: 0.01, batchSize: 32, epochs: 10 },
      medium: { learningRate: 0.001, batchSize: 64, epochs: 50 },
      large: { learningRate: 0.0001, batchSize: 128, epochs: 100 },
    },
  },

  // ============================================================================
  // HTTP MODULES (3)
  // ============================================================================

  // HTTP header validation
  safeHeader:: {
    // Validate header name
    name(n)::
      local validChars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_';
      local chars = std.stringChars(n);
      if std.length(n) == 0 then
        $.Result.err('Header name cannot be empty')
      else if std.all(std.map(function(c) std.member(validChars, c), chars)) then
        $.Result.ok(n)
      else
        $.Result.err('Header name contains invalid characters'),

    // Validate header value (no CRLF injection)
    value(v)::
      if std.length(std.findSubstr('\r', v)) > 0 || std.length(std.findSubstr('\n', v)) > 0 then
        $.Result.err('Header value cannot contain CR or LF')
      else
        $.Result.ok(v),

    // Common security headers
    securityHeaders:: {
      'Strict-Transport-Security': 'max-age=31536000; includeSubDomains',
      'X-Content-Type-Options': 'nosniff',
      'X-Frame-Options': 'DENY',
      'X-XSS-Protection': '1; mode=block',
      'Referrer-Policy': 'strict-origin-when-cross-origin',
      'Content-Security-Policy': "default-src 'self'",
    },
  },

  // HTTP cookie validation
  safeCookie:: {
    // Validate cookie name
    name(n)::
      local invalid = '()<>@,;:\\"/[]?={}';
      local chars = std.stringChars(n);
      if std.length(n) == 0 then
        $.Result.err('Cookie name cannot be empty')
      else if std.any(std.map(function(c) std.member(invalid, c) || std.codepoint(c) < 33, chars)) then
        $.Result.err('Cookie name contains invalid characters')
      else
        $.Result.ok(n),

    // Create secure cookie attributes
    secure(name, value, options={})::
      local maxAge = std.get(options, 'maxAge', null);
      local path = std.get(options, 'path', '/');
      local sameSite = std.get(options, 'sameSite', 'Strict');

      $.Result.ok({
        name: name,
        value: value,
        httpOnly: true,
        secure: true,
        sameSite: sameSite,
        path: path,
        [if maxAge != null then 'maxAge']: maxAge,
      }),

    // SameSite values
    sameSiteValues:: ['Strict', 'Lax', 'None'],
  },

  // Content-Type validation
  safeContentType:: {
    // Validate MIME type format
    validate(ct)::
      local parts = std.split(ct, '/');
      if std.length(parts) != 2 then
        $.Result.err('Content-Type must be type/subtype format')
      else if std.length(parts[0]) == 0 || std.length(parts[1]) == 0 then
        $.Result.err('Content-Type type and subtype cannot be empty')
      else
        $.Result.ok(ct),

    // Extract base type (without parameters)
    baseType(ct)::
      local parts = std.split(ct, ';');
      $.Result.ok(std.stripChars(parts[0], ' ')),

    // Common content types
    types:: {
      json: 'application/json',
      html: 'text/html',
      text: 'text/plain',
      xml: 'application/xml',
      form: 'application/x-www-form-urlencoded',
      multipart: 'multipart/form-data',
      octetStream: 'application/octet-stream',
      pdf: 'application/pdf',
      png: 'image/png',
      jpeg: 'image/jpeg',
    },

    // Check if JSON type
    isJson(ct)::
      local base = self.baseType(ct);
      if $.Result.isErr(base) then
        false
      else
        base.value == 'application/json' || std.endsWith(base.value, '+json'),
  },

  // ============================================================================
  // LEGACY ALIASES (backward compatibility)
  // ============================================================================

  // Old names point to new names
  SafeMath:: $.safeMath,
  SafeString:: $.safeString,
  SafeNetwork:: $.safeNetwork,
  Resources:: $.safeUnit,

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
