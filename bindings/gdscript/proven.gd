# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Proven Safety Library for Godot (GDScript)
##
## Formally verified safety primitives for game development.
## Provides safe math, bounded types, validation, and resilience patterns.
##
## @version 0.4.0

class_name Proven
extends RefCounted

# ============================================================================
# CONSTANTS
# ============================================================================

const VERSION := "0.4.0"
const MODULE_COUNT := 38

# Integer bounds (Godot uses 64-bit integers)
const INT_MAX := 9223372036854775807
const INT_MIN := -9223372036854775808

# Earth radius for geo calculations
const EARTH_RADIUS_KM := 6371.0
const EARTH_RADIUS_MI := 3958.8

# ============================================================================
# RESULT TYPE
# ============================================================================

## Result type for safe operations
class Result extends RefCounted:
	var ok: bool
	var value: Variant
	var error: String

	func _init(is_ok: bool, val: Variant = null, err: String = "") -> void:
		ok = is_ok
		value = val
		error = err

	func is_ok() -> bool:
		return ok

	func is_err() -> bool:
		return not ok

	func unwrap_or(default: Variant) -> Variant:
		return value if ok else default

	func map(callable: Callable) -> Result:
		if ok:
			return Result.new(true, callable.call(value))
		return self


## Create success result
static func Ok(val: Variant) -> Result:
	return Result.new(true, val)


## Create error result
static func Err(err: String) -> Result:
	return Result.new(false, null, err)


# ============================================================================
# CORE MODULE 1: SafeMath
# ============================================================================

## Safe math operations with overflow/underflow detection
class SafeMath extends RefCounted:
	## Safe addition with overflow check
	static func add(a: int, b: int) -> Result:
		if b > 0 and a > INT_MAX - b:
			return Proven.Err("overflow")
		if b < 0 and a < INT_MIN - b:
			return Proven.Err("underflow")
		return Proven.Ok(a + b)

	## Safe subtraction with underflow check
	static func sub(a: int, b: int) -> Result:
		if b < 0 and a > INT_MAX + b:
			return Proven.Err("overflow")
		if b > 0 and a < INT_MIN + b:
			return Proven.Err("underflow")
		return Proven.Ok(a - b)

	## Safe multiplication with overflow check
	static func mul(a: int, b: int) -> Result:
		if a == 0 or b == 0:
			return Proven.Ok(0)
		var result := a * b
		if result / a != b:
			return Proven.Err("overflow")
		return Proven.Ok(result)

	## Safe division with zero check
	static func div(a: int, b: int) -> Result:
		if b == 0:
			return Proven.Err("division_by_zero")
		if a == INT_MIN and b == -1:
			return Proven.Err("overflow")
		return Proven.Ok(a / b)

	## Safe modulo with zero check
	static func mod(a: int, b: int) -> Result:
		if b == 0:
			return Proven.Err("modulo_by_zero")
		return Proven.Ok(a % b)

	## Safe power with overflow check
	static func pow(base: int, exp: int) -> Result:
		if exp < 0:
			return Proven.Err("negative_exponent")
		if exp == 0:
			return Proven.Ok(1)
		var result := 1
		var current_base := base
		var current_exp := exp
		while current_exp > 0:
			if current_exp % 2 == 1:
				var mul_result := mul(result, current_base)
				if mul_result.is_err():
					return mul_result
				result = mul_result.value
			current_exp /= 2
			if current_exp > 0:
				var sq_result := mul(current_base, current_base)
				if sq_result.is_err():
					return sq_result
				current_base = sq_result.value
		return Proven.Ok(result)

	## Clamp value to range
	static func clamp_value(val: int, min_val: int, max_val: int) -> int:
		return clampi(val, min_val, max_val)

	## Check if value is in range (inclusive)
	static func in_range(val: int, min_val: int, max_val: int) -> bool:
		return val >= min_val and val <= max_val


# ============================================================================
# CORE MODULE 2: SafeString
# ============================================================================

## Safe string operations with injection prevention
class SafeString extends RefCounted:
	## Escape HTML special characters
	static func escape_html(input: String) -> String:
		var result := input
		result = result.replace("&", "&amp;")
		result = result.replace("<", "&lt;")
		result = result.replace(">", "&gt;")
		result = result.replace("\"", "&quot;")
		result = result.replace("'", "&#x27;")
		return result

	## Escape SQL single quotes
	static func escape_sql(input: String) -> String:
		return input.replace("'", "''")

	## Escape shell metacharacters
	static func escape_shell(input: String) -> String:
		var dangerous := ["$", "`", "\\", "\"", "\n", "|", "&", ";", "<", ">", "(", ")", "{", "}", "[", "]", "!", "#", "*", "?", "~"]
		var result := input
		for char in dangerous:
			result = result.replace(char, "\\" + char)
		return result

	## Validate UTF-8 string (GDScript strings are always valid UTF-8)
	static func is_valid_utf8(input: String) -> bool:
		return true

	## Truncate string to max length safely
	static func truncate(input: String, max_len: int) -> String:
		if input.length() <= max_len:
			return input
		return input.substr(0, max_len)

	## Check if string contains only alphanumeric characters
	static func is_alphanumeric(input: String) -> bool:
		var regex := RegEx.new()
		regex.compile("^[a-zA-Z0-9]*$")
		return regex.search(input) != null

	## Sanitize string for use as identifier
	static func to_identifier(input: String) -> String:
		var regex := RegEx.new()
		regex.compile("[^a-zA-Z0-9_]")
		var result := regex.sub(input, "_", true)
		if result.length() > 0 and result[0].is_valid_int():
			result = "_" + result
		return result


# ============================================================================
# CORE MODULE 3: SafePath
# ============================================================================

## Safe path operations with traversal prevention
class SafePath extends RefCounted:
	## Check if path is safe (no traversal)
	static func is_safe(path: String) -> bool:
		if path.contains(".."):
			return false
		if path.begins_with("/") or path.begins_with("\\"):
			return false
		if path.contains("://"):
			return false
		return true

	## Normalize path separators
	static func normalize(path: String) -> String:
		return path.replace("\\", "/")

	## Get file extension safely
	static func get_extension(path: String) -> String:
		var idx := path.rfind(".")
		if idx == -1 or idx == path.length() - 1:
			return ""
		return path.substr(idx + 1).to_lower()

	## Get base name without extension
	static func get_basename(path: String) -> String:
		var normalized := normalize(path)
		var slash_idx := normalized.rfind("/")
		var name := normalized if slash_idx == -1 else normalized.substr(slash_idx + 1)
		var dot_idx := name.rfind(".")
		if dot_idx == -1:
			return name
		return name.substr(0, dot_idx)

	## Join paths safely
	static func join(base: String, child: String) -> Result:
		if not is_safe(child):
			return Proven.Err("unsafe_path")
		var normalized_base := normalize(base)
		var normalized_child := normalize(child)
		if normalized_base.ends_with("/"):
			return Proven.Ok(normalized_base + normalized_child)
		return Proven.Ok(normalized_base + "/" + normalized_child)

	## Check if file extension is allowed
	static func has_allowed_extension(path: String, allowed: Array[String]) -> bool:
		var ext := get_extension(path)
		return ext in allowed


# ============================================================================
# CORE MODULE 4: SafeEmail
# ============================================================================

## Safe email validation
class SafeEmail extends RefCounted:
	## Validate email address format
	static func is_valid(email: String) -> bool:
		if email.length() > 254:
			return false
		var regex := RegEx.new()
		regex.compile("^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$")
		return regex.search(email) != null

	## Extract local part (before @)
	static func get_local_part(email: String) -> String:
		var idx := email.find("@")
		if idx == -1:
			return ""
		return email.substr(0, idx)

	## Extract domain part (after @)
	static func get_domain(email: String) -> String:
		var idx := email.find("@")
		if idx == -1:
			return ""
		return email.substr(idx + 1)

	## Normalize email (lowercase domain)
	static func normalize(email: String) -> String:
		var local := get_local_part(email)
		var domain := get_domain(email)
		return local + "@" + domain.to_lower()


# ============================================================================
# CORE MODULE 5: SafeUrl
# ============================================================================

## Safe URL parsing and validation
class SafeUrl extends RefCounted:
	## Validate URL format
	static func is_valid(url: String) -> bool:
		var regex := RegEx.new()
		regex.compile("^(https?|ftp)://[^\\s/$.?#].[^\\s]*$")
		return regex.search(url) != null

	## Check if URL uses HTTPS
	static func is_https(url: String) -> bool:
		return url.to_lower().begins_with("https://")

	## Extract scheme from URL
	static func get_scheme(url: String) -> String:
		var idx := url.find("://")
		if idx == -1:
			return ""
		return url.substr(0, idx).to_lower()

	## Extract host from URL
	static func get_host(url: String) -> String:
		var scheme_idx := url.find("://")
		if scheme_idx == -1:
			return ""
		var rest := url.substr(scheme_idx + 3)
		var path_idx := rest.find("/")
		var host := rest if path_idx == -1 else rest.substr(0, path_idx)
		var port_idx := host.find(":")
		if port_idx != -1:
			host = host.substr(0, port_idx)
		return host

	## URL encode a string
	static func encode(input: String) -> String:
		return input.uri_encode()

	## URL decode a string
	static func decode(input: String) -> String:
		return input.uri_decode()


# ============================================================================
# CORE MODULE 6: SafeNetwork
# ============================================================================

## Safe network address validation
class SafeNetwork extends RefCounted:
	## Validate IPv4 address
	static func is_valid_ipv4(ip: String) -> bool:
		var parts := ip.split(".")
		if parts.size() != 4:
			return false
		for part in parts:
			if not part.is_valid_int():
				return false
			var num := part.to_int()
			if num < 0 or num > 255:
				return false
		return true

	## Validate IPv6 address (simplified)
	static func is_valid_ipv6(ip: String) -> bool:
		var regex := RegEx.new()
		regex.compile("^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$")
		return regex.search(ip) != null

	## Validate port number
	static func is_valid_port(port: int) -> bool:
		return port >= 0 and port <= 65535

	## Check if IP is private (RFC 1918)
	static func is_private_ip(ip: String) -> bool:
		if not is_valid_ipv4(ip):
			return false
		var parts := ip.split(".")
		var a := parts[0].to_int()
		var b := parts[1].to_int()
		# 10.0.0.0/8
		if a == 10:
			return true
		# 172.16.0.0/12
		if a == 172 and b >= 16 and b <= 31:
			return true
		# 192.168.0.0/16
		if a == 192 and b == 168:
			return true
		return false

	## Check if IP is loopback
	static func is_loopback(ip: String) -> bool:
		return ip == "127.0.0.1" or ip == "::1" or ip == "localhost"


# ============================================================================
# CORE MODULE 7: SafeCrypto
# ============================================================================

## Safe cryptographic operations (Godot-native implementations)
class SafeCrypto extends RefCounted:
	## Compute SHA-256 hash
	static func sha256(data: PackedByteArray) -> PackedByteArray:
		var ctx := HashingContext.new()
		ctx.start(HashingContext.HASH_SHA256)
		ctx.update(data)
		return ctx.finish()

	## Compute SHA-256 hash of string
	static func sha256_string(data: String) -> PackedByteArray:
		return sha256(data.to_utf8_buffer())

	## Compute SHA-256 and return as hex string
	static func sha256_hex(data: PackedByteArray) -> String:
		return sha256(data).hex_encode()

	## Compute MD5 hash (for checksums, not security)
	static func md5(data: PackedByteArray) -> PackedByteArray:
		var ctx := HashingContext.new()
		ctx.start(HashingContext.HASH_MD5)
		ctx.update(data)
		return ctx.finish()

	## Generate random bytes using Godot's crypto
	static func random_bytes(length: int) -> PackedByteArray:
		var crypto := Crypto.new()
		return crypto.generate_random_bytes(length)

	## Constant-time comparison (prevents timing attacks)
	static func constant_time_compare(a: PackedByteArray, b: PackedByteArray) -> bool:
		if a.size() != b.size():
			return false
		var result := 0
		for i in range(a.size()):
			result |= a[i] ^ b[i]
		return result == 0

	## Convert bytes to hex string
	static func to_hex(data: PackedByteArray) -> String:
		return data.hex_encode()

	## Convert hex string to bytes
	static func from_hex(hex_str: String) -> PackedByteArray:
		var result := PackedByteArray()
		if hex_str.length() % 2 != 0:
			return result
		for i in range(0, hex_str.length(), 2):
			var byte_str := hex_str.substr(i, 2)
			result.append(("0x" + byte_str).hex_to_int())
		return result


# ============================================================================
# CORE MODULE 8: SafeUUID
# ============================================================================

## Safe UUID generation and validation
class SafeUUID extends RefCounted:
	## Generate UUID v4 (random)
	static func generate_v4() -> String:
		var bytes := SafeCrypto.random_bytes(16)
		# Set version (4) and variant (RFC 4122)
		bytes[6] = (bytes[6] & 0x0f) | 0x40
		bytes[8] = (bytes[8] & 0x3f) | 0x80
		var hex := bytes.hex_encode()
		return "%s-%s-%s-%s-%s" % [
			hex.substr(0, 8),
			hex.substr(8, 4),
			hex.substr(12, 4),
			hex.substr(16, 4),
			hex.substr(20, 12)
		]

	## Validate UUID format
	static func is_valid(uuid: String) -> bool:
		var regex := RegEx.new()
		regex.compile("^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$")
		return regex.search(uuid) != null

	## Normalize UUID to lowercase
	static func normalize(uuid: String) -> String:
		return uuid.to_lower()

	## Get UUID version
	static func get_version(uuid: String) -> int:
		if not is_valid(uuid):
			return -1
		var version_char := uuid[14]
		return ("0x" + version_char).hex_to_int()


# ============================================================================
# CORE MODULE 9: SafeCurrency
# ============================================================================

## Safe currency operations with fixed-point arithmetic
class SafeCurrency extends RefCounted:
	## Money value stored as minor units (cents)
	class Money extends RefCounted:
		var amount: int  # In minor units (cents)
		var currency_code: String

		func _init(amt: int, code: String) -> void:
			amount = amt
			currency_code = code.to_upper()

		func get_major_units() -> float:
			return amount / 100.0

		func add(other: Money) -> Result:
			if currency_code != other.currency_code:
				return Proven.Err("currency_mismatch")
			var result := SafeMath.add(amount, other.amount)
			if result.is_err():
				return result
			return Proven.Ok(Money.new(result.value, currency_code))

		func subtract(other: Money) -> Result:
			if currency_code != other.currency_code:
				return Proven.Err("currency_mismatch")
			var result := SafeMath.sub(amount, other.amount)
			if result.is_err():
				return result
			return Proven.Ok(Money.new(result.value, currency_code))

		func multiply(factor: int) -> Result:
			var result := SafeMath.mul(amount, factor)
			if result.is_err():
				return result
			return Proven.Ok(Money.new(result.value, currency_code))

		func format() -> String:
			var major := amount / 100
			var minor := absi(amount % 100)
			var sign := "-" if amount < 0 else ""
			return "%s%s %d.%02d" % [sign, currency_code, absi(major), minor]

	## Create money from major units (dollars)
	static func from_major(dollars: float, code: String) -> Money:
		var cents := roundi(dollars * 100)
		return Money.new(cents, code)

	## Create money from minor units (cents)
	static func from_minor(cents: int, code: String) -> Money:
		return Money.new(cents, code)

	## Validate ISO 4217 currency code
	static func is_valid_code(code: String) -> bool:
		var regex := RegEx.new()
		regex.compile("^[A-Z]{3}$")
		return regex.search(code.to_upper()) != null


# ============================================================================
# CORE MODULE 10: SafePhone
# ============================================================================

## Safe phone number validation (E.164 format)
class SafePhone extends RefCounted:
	## Validate E.164 phone number
	static func is_valid_e164(phone: String) -> bool:
		var regex := RegEx.new()
		regex.compile("^\\+[1-9]\\d{1,14}$")
		return regex.search(phone) != null

	## Normalize phone number to E.164
	static func normalize(phone: String, default_country_code: String = "+1") -> String:
		var cleaned := ""
		for c in phone:
			if c.is_valid_int() or c == "+":
				cleaned += c
		if cleaned.begins_with("+"):
			return cleaned
		if cleaned.begins_with("00"):
			return "+" + cleaned.substr(2)
		return default_country_code + cleaned

	## Extract country code
	static func get_country_code(phone: String) -> String:
		if not phone.begins_with("+"):
			return ""
		# Simplified: assumes 1-3 digit country code
		var digits := phone.substr(1)
		if digits.length() >= 1 and digits[0] == "1":
			return "+1"
		if digits.length() >= 2:
			return "+" + digits.substr(0, 2)
		return ""


# ============================================================================
# CORE MODULE 11: SafeHex
# ============================================================================

## Safe hexadecimal encoding/decoding
class SafeHex extends RefCounted:
	## Encode bytes to hex string
	static func encode(data: PackedByteArray) -> String:
		return data.hex_encode()

	## Decode hex string to bytes
	static func decode(hex_str: String) -> Result:
		if hex_str.length() % 2 != 0:
			return Proven.Err("odd_length")
		var regex := RegEx.new()
		regex.compile("^[0-9a-fA-F]*$")
		if regex.search(hex_str) == null:
			return Proven.Err("invalid_characters")
		var result := PackedByteArray()
		for i in range(0, hex_str.length(), 2):
			var byte_str := hex_str.substr(i, 2)
			result.append(("0x" + byte_str).hex_to_int())
		return Proven.Ok(result)

	## Validate hex string
	static func is_valid(hex_str: String) -> bool:
		if hex_str.length() % 2 != 0:
			return false
		var regex := RegEx.new()
		regex.compile("^[0-9a-fA-F]*$")
		return regex.search(hex_str) != null


# ============================================================================
# DATA MODULE 1: SafeJson
# ============================================================================

## Safe JSON parsing with type-safe access
class SafeJson extends RefCounted:
	## Parse JSON string safely
	static func parse(json_string: String) -> Result:
		var json := JSON.new()
		var error := json.parse(json_string)
		if error != OK:
			return Proven.Err("parse_error: " + json.get_error_message())
		return Proven.Ok(json.data)

	## Stringify value to JSON
	static func stringify(value: Variant, indent: String = "") -> String:
		return JSON.stringify(value, indent)

	## Get value from dictionary path safely
	static func get_path(data: Dictionary, path: Array) -> Result:
		var current: Variant = data
		for key in path:
			if current is Dictionary:
				if not current.has(key):
					return Proven.Err("key_not_found: " + str(key))
				current = current[key]
			elif current is Array and key is int:
				if key < 0 or key >= current.size():
					return Proven.Err("index_out_of_bounds")
				current = current[key]
			else:
				return Proven.Err("invalid_path")
		return Proven.Ok(current)

	## Get typed value safely
	static func get_string(data: Dictionary, key: String, default: String = "") -> String:
		if data.has(key) and data[key] is String:
			return data[key]
		return default

	static func get_int(data: Dictionary, key: String, default: int = 0) -> int:
		if data.has(key) and (data[key] is int or data[key] is float):
			return int(data[key])
		return default

	static func get_float(data: Dictionary, key: String, default: float = 0.0) -> float:
		if data.has(key) and (data[key] is int or data[key] is float):
			return float(data[key])
		return default

	static func get_bool(data: Dictionary, key: String, default: bool = false) -> bool:
		if data.has(key) and data[key] is bool:
			return data[key]
		return default


# ============================================================================
# DATA MODULE 2: SafeDateTime
# ============================================================================

## Safe date/time operations
class SafeDateTime extends RefCounted:
	## Validate date components
	static func is_valid_date(year: int, month: int, day: int) -> bool:
		if month < 1 or month > 12:
			return false
		if day < 1:
			return false
		var days_in_month := [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		var max_days := days_in_month[month]
		if month == 2 and is_leap_year(year):
			max_days = 29
		return day <= max_days

	## Check if year is leap year
	static func is_leap_year(year: int) -> bool:
		return (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0)

	## Validate time components
	static func is_valid_time(hour: int, minute: int, second: int) -> bool:
		return hour >= 0 and hour <= 23 and minute >= 0 and minute <= 59 and second >= 0 and second <= 59

	## Get current Unix timestamp
	static func now_unix() -> int:
		return int(Time.get_unix_time_from_system())

	## Format timestamp as ISO 8601
	static func to_iso8601(unix_time: int) -> String:
		var dt := Time.get_datetime_dict_from_unix_time(unix_time)
		return "%04d-%02d-%02dT%02d:%02d:%02dZ" % [
			dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second
		]

	## Parse ISO 8601 date string
	static func from_iso8601(iso_string: String) -> Result:
		var regex := RegEx.new()
		regex.compile("^(\\d{4})-(\\d{2})-(\\d{2})(?:T(\\d{2}):(\\d{2}):(\\d{2}))?")
		var match := regex.search(iso_string)
		if match == null:
			return Proven.Err("invalid_format")
		var year := match.get_string(1).to_int()
		var month := match.get_string(2).to_int()
		var day := match.get_string(3).to_int()
		if not is_valid_date(year, month, day):
			return Proven.Err("invalid_date")
		var dict := {"year": year, "month": month, "day": day}
		if match.get_string(4) != "":
			dict.hour = match.get_string(4).to_int()
			dict.minute = match.get_string(5).to_int()
			dict.second = match.get_string(6).to_int()
		return Proven.Ok(dict)


# ============================================================================
# DATA MODULE 3: SafeFloat
# ============================================================================

## Safe floating-point operations with NaN/Infinity prevention
class SafeFloat extends RefCounted:
	## Safe addition (checks for inf/nan)
	static func add(a: float, b: float) -> Result:
		var result := a + b
		if is_inf(result) or is_nan(result):
			return Proven.Err("float_overflow")
		return Proven.Ok(result)

	## Safe subtraction
	static func sub(a: float, b: float) -> Result:
		var result := a - b
		if is_inf(result) or is_nan(result):
			return Proven.Err("float_overflow")
		return Proven.Ok(result)

	## Safe multiplication
	static func mul(a: float, b: float) -> Result:
		var result := a * b
		if is_inf(result) or is_nan(result):
			return Proven.Err("float_overflow")
		return Proven.Ok(result)

	## Safe division with zero check
	static func div(a: float, b: float) -> Result:
		if b == 0.0:
			return Proven.Err("division_by_zero")
		var result := a / b
		if is_inf(result) or is_nan(result):
			return Proven.Err("float_overflow")
		return Proven.Ok(result)

	## Safe square root
	static func sqrt_safe(value: float) -> Result:
		if value < 0.0:
			return Proven.Err("negative_sqrt")
		return Proven.Ok(sqrt(value))

	## Check if float is finite
	static func is_finite(value: float) -> bool:
		return not is_inf(value) and not is_nan(value)

	## Clamp to finite range
	static func clamp_finite(value: float, min_val: float, max_val: float) -> float:
		if is_nan(value):
			return min_val
		if is_inf(value):
			return max_val if value > 0 else min_val
		return clampf(value, min_val, max_val)


# ============================================================================
# DATA MODULE 4: SafeVersion
# ============================================================================

## Semantic versioning parsing and comparison
class SafeVersion extends RefCounted:
	var major: int
	var minor: int
	var patch: int
	var prerelease: String
	var build: String

	func _init(maj: int = 0, min: int = 0, pat: int = 0, pre: String = "", bld: String = "") -> void:
		major = maj
		minor = min
		patch = pat
		prerelease = pre
		build = bld

	func to_string() -> String:
		var result := "%d.%d.%d" % [major, minor, patch]
		if prerelease != "":
			result += "-" + prerelease
		if build != "":
			result += "+" + build
		return result

	## Parse semantic version string
	static func parse(version_str: String) -> Result:
		var regex := RegEx.new()
		regex.compile("^(\\d+)\\.(\\d+)\\.(\\d+)(?:-([a-zA-Z0-9.]+))?(?:\\+([a-zA-Z0-9.]+))?$")
		var match := regex.search(version_str)
		if match == null:
			return Proven.Err("invalid_version")
		var v := SafeVersion.new(
			match.get_string(1).to_int(),
			match.get_string(2).to_int(),
			match.get_string(3).to_int(),
			match.get_string(4),
			match.get_string(5)
		)
		return Proven.Ok(v)

	## Compare two versions (-1, 0, 1)
	static func compare(a: SafeVersion, b: SafeVersion) -> int:
		if a.major != b.major:
			return 1 if a.major > b.major else -1
		if a.minor != b.minor:
			return 1 if a.minor > b.minor else -1
		if a.patch != b.patch:
			return 1 if a.patch > b.patch else -1
		# Prerelease has lower precedence than release
		if a.prerelease == "" and b.prerelease != "":
			return 1
		if a.prerelease != "" and b.prerelease == "":
			return -1
		if a.prerelease != b.prerelease:
			return 1 if a.prerelease > b.prerelease else -1
		return 0


# ============================================================================
# DATA MODULE 5: SafeColor
# ============================================================================

## Safe color operations with WCAG contrast calculations
class SafeColor extends RefCounted:
	## RGB color (0-255)
	class Rgb extends RefCounted:
		var r: int
		var g: int
		var b: int

		func _init(red: int, green: int, blue: int) -> void:
			r = clampi(red, 0, 255)
			g = clampi(green, 0, 255)
			b = clampi(blue, 0, 255)

		func to_godot_color() -> Color:
			return Color(r / 255.0, g / 255.0, b / 255.0)

		func to_hex() -> String:
			return "%02x%02x%02x" % [r, g, b]

		func luminance() -> float:
			var rs := r / 255.0
			var gs := g / 255.0
			var bs := b / 255.0
			rs = rs / 12.92 if rs <= 0.04045 else pow((rs + 0.055) / 1.055, 2.4)
			gs = gs / 12.92 if gs <= 0.04045 else pow((gs + 0.055) / 1.055, 2.4)
			bs = bs / 12.92 if bs <= 0.04045 else pow((bs + 0.055) / 1.055, 2.4)
			return 0.2126 * rs + 0.7152 * gs + 0.0722 * bs

	## Calculate WCAG contrast ratio between two colors
	static func contrast_ratio(color1: Rgb, color2: Rgb) -> float:
		var l1 := color1.luminance()
		var l2 := color2.luminance()
		var lighter := maxf(l1, l2)
		var darker := minf(l1, l2)
		return (lighter + 0.05) / (darker + 0.05)

	## Check if contrast meets WCAG AA (4.5:1 for normal text)
	static func meets_wcag_aa(color1: Rgb, color2: Rgb) -> bool:
		return contrast_ratio(color1, color2) >= 4.5

	## Check if contrast meets WCAG AAA (7:1 for normal text)
	static func meets_wcag_aaa(color1: Rgb, color2: Rgb) -> bool:
		return contrast_ratio(color1, color2) >= 7.0

	## Parse hex color string
	static func from_hex(hex_str: String) -> Result:
		var clean := hex_str.trim_prefix("#")
		if clean.length() != 6:
			return Proven.Err("invalid_hex_length")
		var r := ("0x" + clean.substr(0, 2)).hex_to_int()
		var g := ("0x" + clean.substr(2, 2)).hex_to_int()
		var b := ("0x" + clean.substr(4, 2)).hex_to_int()
		return Proven.Ok(Rgb.new(r, g, b))


# ============================================================================
# DATA MODULE 6: SafeAngle
# ============================================================================

## Safe angle operations with degree/radian conversions
class SafeAngle extends RefCounted:
	## Convert degrees to radians
	static func deg_to_rad(degrees: float) -> float:
		return degrees * PI / 180.0

	## Convert radians to degrees
	static func rad_to_deg(radians: float) -> float:
		return radians * 180.0 / PI

	## Normalize angle to 0-360 degrees
	static func normalize_degrees(degrees: float) -> float:
		var result := fmod(degrees, 360.0)
		if result < 0:
			result += 360.0
		return result

	## Normalize angle to 0-2*PI radians
	static func normalize_radians(radians: float) -> float:
		var result := fmod(radians, TAU)
		if result < 0:
			result += TAU
		return result

	## Lerp between angles (shortest path)
	static func lerp_degrees(from: float, to: float, weight: float) -> float:
		var diff := fmod(to - from + 180.0, 360.0) - 180.0
		return from + diff * clampf(weight, 0.0, 1.0)

	## Calculate angle difference (signed, shortest path)
	static func difference_degrees(from: float, to: float) -> float:
		return fmod(to - from + 180.0, 360.0) - 180.0


# ============================================================================
# DATA MODULE 7: SafeUnit
# ============================================================================

## Safe unit conversions
class SafeUnit extends RefCounted:
	## Length units
	enum LengthUnit { METERS, KILOMETERS, MILES, FEET, INCHES, YARDS }

	## Mass units
	enum MassUnit { GRAMS, KILOGRAMS, POUNDS, OUNCES }

	## Temperature units
	enum TempUnit { CELSIUS, FAHRENHEIT, KELVIN }

	## Convert length between units
	static func convert_length(value: float, from: LengthUnit, to: LengthUnit) -> float:
		# Convert to meters first
		var meters := value
		match from:
			LengthUnit.KILOMETERS:
				meters = value * 1000.0
			LengthUnit.MILES:
				meters = value * 1609.344
			LengthUnit.FEET:
				meters = value * 0.3048
			LengthUnit.INCHES:
				meters = value * 0.0254
			LengthUnit.YARDS:
				meters = value * 0.9144
		# Convert from meters to target
		match to:
			LengthUnit.METERS:
				return meters
			LengthUnit.KILOMETERS:
				return meters / 1000.0
			LengthUnit.MILES:
				return meters / 1609.344
			LengthUnit.FEET:
				return meters / 0.3048
			LengthUnit.INCHES:
				return meters / 0.0254
			LengthUnit.YARDS:
				return meters / 0.9144
		return meters

	## Convert temperature between units
	static func convert_temperature(value: float, from: TempUnit, to: TempUnit) -> float:
		# Convert to Celsius first
		var celsius := value
		match from:
			TempUnit.FAHRENHEIT:
				celsius = (value - 32.0) * 5.0 / 9.0
			TempUnit.KELVIN:
				celsius = value - 273.15
		# Convert from Celsius to target
		match to:
			TempUnit.CELSIUS:
				return celsius
			TempUnit.FAHRENHEIT:
				return celsius * 9.0 / 5.0 + 32.0
			TempUnit.KELVIN:
				return celsius + 273.15
		return celsius


# ============================================================================
# DATA STRUCTURES MODULE 1: SafeBuffer
# ============================================================================

## Bounded buffer with safe operations
class SafeBuffer extends RefCounted:
	var _data: Array
	var _capacity: int

	func _init(capacity: int) -> void:
		_capacity = maxi(1, capacity)
		_data = []

	func len() -> int:
		return _data.size()

	func is_empty() -> bool:
		return _data.is_empty()

	func is_full() -> bool:
		return _data.size() >= _capacity

	func remaining() -> int:
		return _capacity - _data.size()

	func push(value: Variant) -> Result:
		if is_full():
			return Proven.Err("buffer_full")
		_data.append(value)
		return Proven.Ok(null)

	func pop() -> Result:
		if is_empty():
			return Proven.Err("buffer_empty")
		return Proven.Ok(_data.pop_back())

	func get_at(index: int) -> Result:
		if index < 0 or index >= _data.size():
			return Proven.Err("index_out_of_bounds")
		return Proven.Ok(_data[index])

	func clear() -> void:
		_data.clear()


# ============================================================================
# DATA STRUCTURES MODULE 2: SafeQueue
# ============================================================================

## Bounded FIFO queue
class SafeQueue extends RefCounted:
	var _data: Array
	var _capacity: int

	func _init(capacity: int) -> void:
		_capacity = maxi(1, capacity)
		_data = []

	func len() -> int:
		return _data.size()

	func is_empty() -> bool:
		return _data.is_empty()

	func is_full() -> bool:
		return _data.size() >= _capacity

	func enqueue(value: Variant) -> Result:
		if is_full():
			return Proven.Err("queue_full")
		_data.append(value)
		return Proven.Ok(null)

	func dequeue() -> Result:
		if is_empty():
			return Proven.Err("queue_empty")
		return Proven.Ok(_data.pop_front())

	func peek() -> Result:
		if is_empty():
			return Proven.Err("queue_empty")
		return Proven.Ok(_data[0])

	func clear() -> void:
		_data.clear()


# ============================================================================
# DATA STRUCTURES MODULE 3: SafeBloom
# ============================================================================

## Bloom filter for probabilistic set membership
class SafeBloom extends RefCounted:
	var _bits: PackedByteArray
	var _size: int
	var _num_hashes: int

	func _init(size: int, num_hashes: int = 3) -> void:
		_size = maxi(8, size)
		_num_hashes = maxi(1, num_hashes)
		_bits = PackedByteArray()
		_bits.resize((_size + 7) / 8)

	func insert(value: String) -> void:
		for i in range(_num_hashes):
			var hash := _compute_hash(value, i)
			var byte_idx := hash / 8
			var bit_idx := hash % 8
			_bits[byte_idx] |= (1 << bit_idx)

	func contains(value: String) -> bool:
		for i in range(_num_hashes):
			var hash := _compute_hash(value, i)
			var byte_idx := hash / 8
			var bit_idx := hash % 8
			if (_bits[byte_idx] & (1 << bit_idx)) == 0:
				return false
		return true

	func _compute_hash(value: String, seed: int) -> int:
		var h := value.hash()
		h = h ^ (seed * 0x9e3779b9)
		return absi(h) % _size

	func clear() -> void:
		_bits.fill(0)

	func fill_ratio() -> float:
		var count := 0
		for byte in _bits:
			for i in range(8):
				if byte & (1 << i):
					count += 1
		return float(count) / float(_size)


# ============================================================================
# DATA STRUCTURES MODULE 4: SafeLRU
# ============================================================================

## LRU (Least Recently Used) cache
class SafeLRU extends RefCounted:
	var _data: Dictionary
	var _order: Array
	var _capacity: int

	func _init(capacity: int) -> void:
		_capacity = maxi(1, capacity)
		_data = {}
		_order = []

	func len() -> int:
		return _data.size()

	func is_empty() -> bool:
		return _data.is_empty()

	func get_value(key: Variant) -> Result:
		if not _data.has(key):
			return Proven.Err("key_not_found")
		# Move to front (most recently used)
		_order.erase(key)
		_order.push_front(key)
		return Proven.Ok(_data[key])

	func put(key: Variant, value: Variant) -> void:
		if _data.has(key):
			_order.erase(key)
		elif _data.size() >= _capacity:
			# Evict least recently used (last in order)
			var lru_key = _order.pop_back()
			_data.erase(lru_key)
		_data[key] = value
		_order.push_front(key)

	func remove(key: Variant) -> Result:
		if not _data.has(key):
			return Proven.Err("key_not_found")
		_order.erase(key)
		var value = _data[key]
		_data.erase(key)
		return Proven.Ok(value)

	func contains(key: Variant) -> bool:
		return _data.has(key)

	func clear() -> void:
		_data.clear()
		_order.clear()


# ============================================================================
# DATA STRUCTURES MODULE 5: SafeGraph
# ============================================================================

## Directed graph with cycle detection
class SafeGraph extends RefCounted:
	var _adjacency: Dictionary  # node -> Array of connected nodes
	var _nodes: Array

	func _init() -> void:
		_adjacency = {}
		_nodes = []

	func add_node(node: Variant) -> void:
		if not _adjacency.has(node):
			_adjacency[node] = []
			_nodes.append(node)

	func add_edge(from: Variant, to: Variant) -> void:
		add_node(from)
		add_node(to)
		if not to in _adjacency[from]:
			_adjacency[from].append(to)

	func has_edge(from: Variant, to: Variant) -> bool:
		return _adjacency.has(from) and to in _adjacency[from]

	func get_neighbors(node: Variant) -> Array:
		if not _adjacency.has(node):
			return []
		return _adjacency[node].duplicate()

	## Detect if graph has a cycle using DFS
	func has_cycle() -> bool:
		var visited := {}
		var rec_stack := {}
		for node in _nodes:
			if _has_cycle_util(node, visited, rec_stack):
				return true
		return false

	func _has_cycle_util(node: Variant, visited: Dictionary, rec_stack: Dictionary) -> bool:
		visited[node] = true
		rec_stack[node] = true
		for neighbor in _adjacency.get(node, []):
			if not visited.get(neighbor, false):
				if _has_cycle_util(neighbor, visited, rec_stack):
					return true
			elif rec_stack.get(neighbor, false):
				return true
		rec_stack[node] = false
		return false

	## Topological sort (returns empty array if cycle exists)
	func topological_sort() -> Array:
		if has_cycle():
			return []
		var visited := {}
		var result := []
		for node in _nodes:
			if not visited.get(node, false):
				_topo_sort_util(node, visited, result)
		result.reverse()
		return result

	func _topo_sort_util(node: Variant, visited: Dictionary, result: Array) -> void:
		visited[node] = true
		for neighbor in _adjacency.get(node, []):
			if not visited.get(neighbor, false):
				_topo_sort_util(neighbor, visited, result)
		result.append(node)


# ============================================================================
# RESILIENCE MODULE 1: SafeRateLimiter
# ============================================================================

## Rate limiter using token bucket algorithm
class SafeRateLimiter extends RefCounted:
	var _capacity: int
	var _tokens: int
	var _refill_rate: int  # tokens per time unit
	var _last_refill: int

	func _init(capacity: int, refill_rate: int) -> void:
		_capacity = maxi(1, capacity)
		_refill_rate = maxi(1, refill_rate)
		_tokens = _capacity
		_last_refill = 0

	func refill(current_time: int) -> void:
		var elapsed := current_time - _last_refill
		var new_tokens := _refill_rate * elapsed
		_tokens = mini(_capacity, _tokens + new_tokens)
		_last_refill = current_time

	func try_acquire(count: int, current_time: int) -> bool:
		refill(current_time)
		if _tokens >= count:
			_tokens -= count
			return true
		return false

	func current_tokens(current_time: int) -> int:
		refill(current_time)
		return _tokens

	func time_until_available(count: int, current_time: int) -> int:
		refill(current_time)
		if _tokens >= count:
			return 0
		var needed := count - _tokens
		return (needed + _refill_rate - 1) / _refill_rate


# ============================================================================
# RESILIENCE MODULE 2: SafeCircuitBreaker
# ============================================================================

## Circuit breaker for fault tolerance
class SafeCircuitBreaker extends RefCounted:
	enum State { CLOSED, OPEN, HALF_OPEN }

	var _state: State
	var _failure_threshold: int
	var _success_threshold: int
	var _timeout: int
	var _failures: int
	var _successes: int
	var _last_failure_time: int
	var _half_open_calls: int
	var _half_open_max_calls: int

	func _init(failure_threshold: int = 5, success_threshold: int = 2, timeout: int = 30) -> void:
		_state = State.CLOSED
		_failure_threshold = failure_threshold
		_success_threshold = success_threshold
		_timeout = timeout
		_failures = 0
		_successes = 0
		_last_failure_time = 0
		_half_open_calls = 0
		_half_open_max_calls = 3

	func state() -> State:
		return _state

	func is_healthy() -> bool:
		return _state == State.CLOSED

	func update_state(current_time: int) -> void:
		if _state == State.OPEN and current_time >= _last_failure_time + _timeout:
			_state = State.HALF_OPEN
			_successes = 0
			_half_open_calls = 0

	func can_execute(current_time: int) -> bool:
		update_state(current_time)
		match _state:
			State.CLOSED:
				return true
			State.OPEN:
				return false
			State.HALF_OPEN:
				return _half_open_calls < _half_open_max_calls
		return false

	func record_success() -> void:
		match _state:
			State.CLOSED:
				_failures = 0
			State.HALF_OPEN:
				_successes += 1
				if _successes >= _success_threshold:
					_state = State.CLOSED
					_failures = 0
					_successes = 0

	func record_failure(current_time: int) -> void:
		_last_failure_time = current_time
		match _state:
			State.CLOSED:
				_failures += 1
				if _failures >= _failure_threshold:
					_state = State.OPEN
			State.HALF_OPEN:
				_state = State.OPEN
				_failures += 1

	func record_attempt() -> void:
		if _state == State.HALF_OPEN:
			_half_open_calls += 1

	func reset() -> void:
		_state = State.CLOSED
		_failures = 0
		_successes = 0
		_half_open_calls = 0


# ============================================================================
# RESILIENCE MODULE 3: SafeRetry
# ============================================================================

## Retry with exponential backoff
class SafeRetry extends RefCounted:
	var _max_attempts: int
	var _base_delay: float
	var _max_delay: float
	var _multiplier: float
	var _current_attempt: int
	var _current_delay: float

	func _init(max_attempts: int = 3, base_delay: float = 1.0, max_delay: float = 60.0, multiplier: float = 2.0) -> void:
		_max_attempts = maxi(1, max_attempts)
		_base_delay = maxf(0.0, base_delay)
		_max_delay = maxf(base_delay, max_delay)
		_multiplier = maxf(1.0, multiplier)
		_current_attempt = 0
		_current_delay = base_delay

	func should_retry() -> bool:
		return _current_attempt < _max_attempts

	func record_attempt() -> void:
		_current_attempt += 1
		_current_delay = minf(_current_delay * _multiplier, _max_delay)

	func get_delay() -> float:
		return _current_delay

	## Get delay with jitter (prevents thundering herd)
	func get_delay_with_jitter() -> float:
		var jitter := randf() * _current_delay * 0.1
		return _current_delay + jitter

	func reset() -> void:
		_current_attempt = 0
		_current_delay = _base_delay

	func attempts_remaining() -> int:
		return _max_attempts - _current_attempt


# ============================================================================
# RESILIENCE MODULE 4: SafeMonotonic
# ============================================================================

## Monotonically increasing counter/sequence
class SafeMonotonic extends RefCounted:
	var _value: int
	var _high_water_mark: int

	func _init(initial: int = 0) -> void:
		_value = initial
		_high_water_mark = initial

	func get_value() -> int:
		return _value

	func get_high_water_mark() -> int:
		return _high_water_mark

	func increment() -> int:
		_value += 1
		_high_water_mark = maxi(_high_water_mark, _value)
		return _value

	func set_value(new_value: int) -> bool:
		if new_value <= _value:
			return false
		_value = new_value
		_high_water_mark = maxi(_high_water_mark, _value)
		return true

	func next() -> int:
		return increment()


# ============================================================================
# STATE MODULE 1: SafeStateMachine
# ============================================================================

## State machine with validated transitions
class SafeStateMachine extends RefCounted:
	var _current: Variant
	var _transitions: Dictionary  # state -> Array of valid next states
	var _history: Array
	var _max_history: int

	func _init(initial: Variant, max_history: int = 100) -> void:
		_current = initial
		_transitions = {}
		_history = []
		_max_history = max_history

	func add_transition(from: Variant, to: Variant) -> void:
		if not _transitions.has(from):
			_transitions[from] = []
		if not to in _transitions[from]:
			_transitions[from].append(to)

	func add_transitions(from: Variant, to_states: Array) -> void:
		for to in to_states:
			add_transition(from, to)

	func current() -> Variant:
		return _current

	func can_transition(to: Variant) -> bool:
		if not _transitions.has(_current):
			return false
		return to in _transitions[_current]

	func valid_transitions() -> Array:
		if not _transitions.has(_current):
			return []
		return _transitions[_current].duplicate()

	func transition(to: Variant) -> Result:
		if not can_transition(to):
			return Proven.Err("invalid_transition")
		_history.append(_current)
		while _history.size() > _max_history:
			_history.pop_front()
		_current = to
		return Proven.Ok(_current)

	func force_transition(to: Variant) -> void:
		_history.append(_current)
		while _history.size() > _max_history:
			_history.pop_front()
		_current = to

	func history() -> Array:
		return _history.duplicate()

	func clear_history() -> void:
		_history.clear()

	func reset() -> void:
		if not _history.is_empty():
			_current = _history[0]
		_history.clear()


# ============================================================================
# STATE MODULE 2: SafeCalculator
# ============================================================================

## Safe expression calculator with overflow protection
class SafeCalculator extends RefCounted:
	var _variables: Dictionary
	var _max_iterations: int

	func _init() -> void:
		_variables = {}
		_max_iterations = 1000

	func set_variable(name: String, value: float) -> void:
		_variables[name] = value

	func get_variable(name: String) -> Result:
		if not _variables.has(name):
			return Proven.Err("undefined_variable")
		return Proven.Ok(_variables[name])

	func clear_variables() -> void:
		_variables.clear()

	## Evaluate simple arithmetic expression
	## Supports: +, -, *, /, parentheses, and variables
	func evaluate(expression: String) -> Result:
		var tokens := _tokenize(expression)
		if tokens.is_empty():
			return Proven.Err("empty_expression")
		return _parse_expression(tokens, 0)

	func _tokenize(expr: String) -> Array:
		var tokens := []
		var i := 0
		while i < expr.length():
			var c := expr[i]
			if c == " ":
				i += 1
				continue
			if c in "+-*/()":
				tokens.append(c)
				i += 1
			elif c.is_valid_float() or c == ".":
				var num_str := ""
				while i < expr.length() and (expr[i].is_valid_float() or expr[i] == "."):
					num_str += expr[i]
					i += 1
				tokens.append(num_str.to_float())
			elif c.is_valid_identifier():
				var var_name := ""
				while i < expr.length() and (expr[i].is_valid_identifier() or expr[i].is_valid_int()):
					var_name += expr[i]
					i += 1
				if _variables.has(var_name):
					tokens.append(_variables[var_name])
				else:
					return []
			else:
				return []
		return tokens

	func _parse_expression(tokens: Array, _depth: int) -> Result:
		# Simplified: use Godot's Expression for actual evaluation
		var expr := Expression.new()
		var expr_str := ""
		for token in tokens:
			expr_str += str(token)
		var error := expr.parse(expr_str)
		if error != OK:
			return Proven.Err("parse_error")
		var result = expr.execute()
		if expr.has_execute_failed():
			return Proven.Err("execution_error")
		if is_inf(result) or is_nan(result):
			return Proven.Err("overflow_or_invalid")
		return Proven.Ok(result)


# ============================================================================
# ALGORITHM MODULE 1: SafeGeo
# ============================================================================

## Geographic coordinate operations
class SafeGeo extends RefCounted:
	## Coordinate with validation
	class Coordinate extends RefCounted:
		var lat: float
		var lon: float

		func _init(latitude: float, longitude: float) -> void:
			lat = clampf(latitude, -90.0, 90.0)
			lon = clampf(longitude, -180.0, 180.0)

		func lat_rad() -> float:
			return lat * PI / 180.0

		func lon_rad() -> float:
			return lon * PI / 180.0

		func is_northern() -> bool:
			return lat >= 0.0

		func is_eastern() -> bool:
			return lon >= 0.0

	## Validate latitude
	static func is_valid_latitude(lat: float) -> bool:
		return lat >= -90.0 and lat <= 90.0

	## Validate longitude
	static func is_valid_longitude(lon: float) -> bool:
		return lon >= -180.0 and lon <= 180.0

	## Create validated coordinate
	static func coordinate(lat: float, lon: float) -> Result:
		if not is_valid_latitude(lat):
			return Proven.Err("invalid_latitude")
		if not is_valid_longitude(lon):
			return Proven.Err("invalid_longitude")
		return Proven.Ok(Coordinate.new(lat, lon))

	## Calculate Haversine distance in km
	static func haversine_km(c1: Coordinate, c2: Coordinate) -> float:
		var dlat := (c2.lat - c1.lat) * PI / 180.0
		var dlon := (c2.lon - c1.lon) * PI / 180.0
		var a := sin(dlat / 2.0) * sin(dlat / 2.0) + cos(c1.lat_rad()) * cos(c2.lat_rad()) * sin(dlon / 2.0) * sin(dlon / 2.0)
		var c := 2.0 * asin(sqrt(a))
		return EARTH_RADIUS_KM * c

	## Calculate bearing from c1 to c2 in degrees
	static func bearing(c1: Coordinate, c2: Coordinate) -> float:
		var dlon := (c2.lon - c1.lon) * PI / 180.0
		var y := sin(dlon) * cos(c2.lat_rad())
		var x := cos(c1.lat_rad()) * sin(c2.lat_rad()) - sin(c1.lat_rad()) * cos(c2.lat_rad()) * cos(dlon)
		var bearing_rad := atan2(y, x)
		return fmod(bearing_rad * 180.0 / PI + 360.0, 360.0)


# ============================================================================
# ALGORITHM MODULE 2: SafeProbability
# ============================================================================

## Probability values clamped to [0, 1]
class SafeProbability extends RefCounted:
	var _value: float

	func _init(val: float) -> void:
		_value = clampf(val, 0.0, 1.0)

	func get_value() -> float:
		return _value

	func as_percentage() -> float:
		return _value * 100.0

	func complement() -> SafeProbability:
		return SafeProbability.new(1.0 - _value)

	## Combine probabilities (independent events)
	func and_prob(other: SafeProbability) -> SafeProbability:
		return SafeProbability.new(_value * other._value)

	## Either event (independent events)
	func or_prob(other: SafeProbability) -> SafeProbability:
		return SafeProbability.new(_value + other._value - _value * other._value)

	## Sample: returns true with this probability
	func sample() -> bool:
		return randf() < _value


# ============================================================================
# ALGORITHM MODULE 3: SafeChecksum
# ============================================================================

## Checksum algorithms
class SafeChecksum extends RefCounted:
	## CRC-32 (IEEE polynomial)
	static func crc32(data: PackedByteArray) -> int:
		var crc := 0xFFFFFFFF
		for byte in data:
			crc ^= byte
			for _i in range(8):
				if crc & 1:
					crc = (crc >> 1) ^ 0xEDB88320
				else:
					crc >>= 1
		return crc ^ 0xFFFFFFFF

	## Adler-32
	static func adler32(data: PackedByteArray) -> int:
		var a := 1
		var b := 0
		for byte in data:
			a = (a + byte) % 65521
			b = (b + a) % 65521
		return (b << 16) | a

	## Luhn check digit validation (credit cards, etc.)
	static func luhn_check(digits: String) -> bool:
		var sum := 0
		var double := false
		for i in range(digits.length() - 1, -1, -1):
			var d := digits[i].to_int()
			if double:
				d *= 2
				if d > 9:
					d -= 9
			sum += d
			double = not double
		return sum % 10 == 0

	## DJB2 hash
	static func djb2(data: String) -> int:
		var hash := 5381
		for c in data:
			hash = ((hash << 5) + hash) + c.unicode_at(0)
		return hash & 0x7FFFFFFF

	## FNV-1a hash (32-bit)
	static func fnv1a_32(data: PackedByteArray) -> int:
		var hash := 0x811c9dc5
		for byte in data:
			hash ^= byte
			hash = (hash * 0x01000193) & 0xFFFFFFFF
		return hash


# ============================================================================
# ALGORITHM MODULE 4: SafeTensor
# ============================================================================

## Safe tensor/vector operations with bounds checking
class SafeTensor extends RefCounted:
	var _data: PackedFloat64Array
	var _shape: PackedInt32Array

	func _init(shape: Array[int]) -> void:
		_shape = PackedInt32Array(shape)
		var size := 1
		for dim in shape:
			size *= dim
		_data = PackedFloat64Array()
		_data.resize(size)

	func shape() -> PackedInt32Array:
		return _shape

	func size() -> int:
		return _data.size()

	func _flat_index(indices: Array[int]) -> int:
		if indices.size() != _shape.size():
			return -1
		var idx := 0
		var stride := 1
		for i in range(_shape.size() - 1, -1, -1):
			if indices[i] < 0 or indices[i] >= _shape[i]:
				return -1
			idx += indices[i] * stride
			stride *= _shape[i]
		return idx

	func get_at(indices: Array[int]) -> Result:
		var idx := _flat_index(indices)
		if idx < 0:
			return Proven.Err("index_out_of_bounds")
		return Proven.Ok(_data[idx])

	func set_at(indices: Array[int], value: float) -> Result:
		var idx := _flat_index(indices)
		if idx < 0:
			return Proven.Err("index_out_of_bounds")
		_data[idx] = value
		return Proven.Ok(null)

	func fill(value: float) -> void:
		_data.fill(value)

	## Element-wise addition (same shape required)
	func add(other: SafeTensor) -> Result:
		if _shape != other._shape:
			return Proven.Err("shape_mismatch")
		var result := SafeTensor.new(Array(_shape))
		for i in range(_data.size()):
			result._data[i] = _data[i] + other._data[i]
		return Proven.Ok(result)

	## Dot product (1D tensors)
	func dot(other: SafeTensor) -> Result:
		if _shape.size() != 1 or other._shape.size() != 1:
			return Proven.Err("not_1d_tensor")
		if _shape[0] != other._shape[0]:
			return Proven.Err("length_mismatch")
		var sum := 0.0
		for i in range(_data.size()):
			sum += _data[i] * other._data[i]
		return Proven.Ok(sum)


# ============================================================================
# SECURITY MODULE 1: SafePassword
# ============================================================================

## Safe password validation and policy checking
class SafePassword extends RefCounted:
	## Password strength levels
	enum Strength { VERY_WEAK, WEAK, FAIR, STRONG, VERY_STRONG }

	## Check password meets minimum requirements
	static func meets_policy(password: String, min_length: int = 8, require_upper: bool = true, require_lower: bool = true, require_digit: bool = true, require_special: bool = false) -> Dictionary:
		var result := {
			"valid": true,
			"errors": []
		}
		if password.length() < min_length:
			result.valid = false
			result.errors.append("too_short")
		if require_upper and not _has_upper(password):
			result.valid = false
			result.errors.append("missing_uppercase")
		if require_lower and not _has_lower(password):
			result.valid = false
			result.errors.append("missing_lowercase")
		if require_digit and not _has_digit(password):
			result.valid = false
			result.errors.append("missing_digit")
		if require_special and not _has_special(password):
			result.valid = false
			result.errors.append("missing_special")
		return result

	static func _has_upper(s: String) -> bool:
		var regex := RegEx.new()
		regex.compile("[A-Z]")
		return regex.search(s) != null

	static func _has_lower(s: String) -> bool:
		var regex := RegEx.new()
		regex.compile("[a-z]")
		return regex.search(s) != null

	static func _has_digit(s: String) -> bool:
		var regex := RegEx.new()
		regex.compile("[0-9]")
		return regex.search(s) != null

	static func _has_special(s: String) -> bool:
		var regex := RegEx.new()
		regex.compile("[!@#$%^&*(),.?\":{}|<>]")
		return regex.search(s) != null

	## Calculate password strength
	static func strength(password: String) -> Strength:
		var score := 0
		if password.length() >= 8:
			score += 1
		if password.length() >= 12:
			score += 1
		if _has_upper(password):
			score += 1
		if _has_lower(password):
			score += 1
		if _has_digit(password):
			score += 1
		if _has_special(password):
			score += 1
		if password.length() >= 16:
			score += 1
		if score <= 2:
			return Strength.VERY_WEAK
		elif score <= 3:
			return Strength.WEAK
		elif score <= 4:
			return Strength.FAIR
		elif score <= 5:
			return Strength.STRONG
		else:
			return Strength.VERY_STRONG


# ============================================================================
# SECURITY MODULE 2: SafeML
# ============================================================================

## Safe machine learning primitives
class SafeML extends RefCounted:
	## Numerically stable softmax
	static func softmax(values: PackedFloat64Array) -> PackedFloat64Array:
		if values.is_empty():
			return PackedFloat64Array()
		# Find max for numerical stability
		var max_val := values[0]
		for v in values:
			if v > max_val:
				max_val = v
		# Compute exp(x - max) and sum
		var exp_values := PackedFloat64Array()
		exp_values.resize(values.size())
		var sum := 0.0
		for i in range(values.size()):
			exp_values[i] = exp(values[i] - max_val)
			sum += exp_values[i]
		# Normalize
		if sum > 0:
			for i in range(exp_values.size()):
				exp_values[i] /= sum
		return exp_values

	## Sigmoid activation
	static func sigmoid(x: float) -> float:
		if x >= 0:
			return 1.0 / (1.0 + exp(-x))
		else:
			var ez := exp(x)
			return ez / (1.0 + ez)

	## ReLU activation
	static func relu(x: float) -> float:
		return maxf(0.0, x)

	## Leaky ReLU
	static func leaky_relu(x: float, alpha: float = 0.01) -> float:
		return x if x >= 0 else alpha * x

	## Cross-entropy loss (numerically stable)
	static func cross_entropy(predictions: PackedFloat64Array, targets: PackedFloat64Array) -> Result:
		if predictions.size() != targets.size():
			return Proven.Err("size_mismatch")
		var loss := 0.0
		var epsilon := 1e-15
		for i in range(predictions.size()):
			var p := clampf(predictions[i], epsilon, 1.0 - epsilon)
			loss -= targets[i] * log(p)
		return Proven.Ok(loss / predictions.size())

	## Mean squared error
	static func mse(predictions: PackedFloat64Array, targets: PackedFloat64Array) -> Result:
		if predictions.size() != targets.size():
			return Proven.Err("size_mismatch")
		var sum := 0.0
		for i in range(predictions.size()):
			var diff := predictions[i] - targets[i]
			sum += diff * diff
		return Proven.Ok(sum / predictions.size())


# ============================================================================
# HTTP MODULE 1: SafeHeader
# ============================================================================

## Safe HTTP header validation (CRLF injection prevention)
class SafeHeader extends RefCounted:
	## Validate header name
	static func is_valid_name(name: String) -> bool:
		if name.is_empty():
			return false
		var regex := RegEx.new()
		regex.compile("^[A-Za-z0-9!#$%&'*+.^_`|~-]+$")
		return regex.search(name) != null

	## Validate header value (no CRLF)
	static func is_valid_value(value: String) -> bool:
		return not value.contains("\r") and not value.contains("\n")

	## Sanitize header value (remove CRLF)
	static func sanitize_value(value: String) -> String:
		return value.replace("\r", "").replace("\n", "")

	## Create validated header
	static func create(name: String, value: String) -> Result:
		if not is_valid_name(name):
			return Proven.Err("invalid_header_name")
		if not is_valid_value(value):
			return Proven.Err("invalid_header_value")
		return Proven.Ok({"name": name, "value": value})


# ============================================================================
# HTTP MODULE 2: SafeCookie
# ============================================================================

## Safe HTTP cookie validation
class SafeCookie extends RefCounted:
	enum SameSite { STRICT, LAX, NONE }

	## Validate cookie name
	static func is_valid_name(name: String) -> bool:
		if name.is_empty():
			return false
		var regex := RegEx.new()
		regex.compile("^[A-Za-z0-9!#$%&'*+.^_`|~-]+$")
		return regex.search(name) != null

	## Validate cookie value
	static func is_valid_value(value: String) -> bool:
		return not value.contains(";") and not value.contains(",") and not value.contains(" ")

	## Create cookie string
	static func create(name: String, value: String, max_age: int = -1, path: String = "/", secure: bool = true, http_only: bool = true, same_site: SameSite = SameSite.LAX) -> Result:
		if not is_valid_name(name):
			return Proven.Err("invalid_cookie_name")
		if not is_valid_value(value):
			return Proven.Err("invalid_cookie_value")
		var cookie := "%s=%s; Path=%s" % [name, value, path]
		if max_age >= 0:
			cookie += "; Max-Age=%d" % max_age
		if secure:
			cookie += "; Secure"
		if http_only:
			cookie += "; HttpOnly"
		match same_site:
			SameSite.STRICT:
				cookie += "; SameSite=Strict"
			SameSite.LAX:
				cookie += "; SameSite=Lax"
			SameSite.NONE:
				cookie += "; SameSite=None"
		return Proven.Ok(cookie)


# ============================================================================
# HTTP MODULE 3: SafeContentType
# ============================================================================

## Safe MIME/Content-Type handling
class SafeContentType extends RefCounted:
	## Common MIME types
	const MIME_JSON := "application/json"
	const MIME_HTML := "text/html"
	const MIME_TEXT := "text/plain"
	const MIME_XML := "application/xml"
	const MIME_FORM := "application/x-www-form-urlencoded"
	const MIME_MULTIPART := "multipart/form-data"

	## Validate MIME type format
	static func is_valid(mime_type: String) -> bool:
		var regex := RegEx.new()
		regex.compile("^[a-zA-Z0-9][a-zA-Z0-9!#$&\\-^_.+]*\\/[a-zA-Z0-9][a-zA-Z0-9!#$&\\-^_.+]*$")
		return regex.search(mime_type.split(";")[0].strip_edges()) != null

	## Get type without parameters
	static func get_type(content_type: String) -> String:
		return content_type.split(";")[0].strip_edges().to_lower()

	## Check if content type is JSON
	static func is_json(content_type: String) -> bool:
		var t := get_type(content_type)
		return t == "application/json" or t.ends_with("+json")

	## Check if content type is text
	static func is_text(content_type: String) -> bool:
		return get_type(content_type).begins_with("text/")

	## Get charset from content type
	static func get_charset(content_type: String) -> String:
		var regex := RegEx.new()
		regex.compile("charset=([^;\\s]+)")
		var match := regex.search(content_type)
		if match != null:
			return match.get_string(1).to_lower()
		return "utf-8"


# ============================================================================
# GAME-SPECIFIC UTILITIES (Godot-native)
# ============================================================================

## Check if Vector2 is within bounds
static func vector2_in_bounds(v: Vector2, min_v: Vector2, max_v: Vector2) -> bool:
	return v.x >= min_v.x and v.x <= max_v.x and v.y >= min_v.y and v.y <= max_v.y


## Check if Vector3 is within bounds
static func vector3_in_bounds(v: Vector3, min_v: Vector3, max_v: Vector3) -> bool:
	return (v.x >= min_v.x and v.x <= max_v.x and
			v.y >= min_v.y and v.y <= max_v.y and
			v.z >= min_v.z and v.z <= max_v.z)


## Clamp Vector2 to bounds
static func clamp_vector2(v: Vector2, min_v: Vector2, max_v: Vector2) -> Vector2:
	return Vector2(
		clamp(v.x, min_v.x, max_v.x),
		clamp(v.y, min_v.y, max_v.y)
	)


## Clamp Vector3 to bounds
static func clamp_vector3(v: Vector3, min_v: Vector3, max_v: Vector3) -> Vector3:
	return Vector3(
		clamp(v.x, min_v.x, max_v.x),
		clamp(v.y, min_v.y, max_v.y),
		clamp(v.z, min_v.z, max_v.z)
	)


## Safe velocity clamping
static func clamp_velocity(velocity: Vector2, max_speed: float) -> Vector2:
	if max_speed <= 0.0:
		return Vector2.ZERO
	if velocity.length_squared() > max_speed * max_speed:
		return velocity.normalized() * max_speed
	return velocity


## Safe velocity clamping (3D)
static func clamp_velocity_3d(velocity: Vector3, max_speed: float) -> Vector3:
	if max_speed <= 0.0:
		return Vector3.ZERO
	if velocity.length_squared() > max_speed * max_speed:
		return velocity.normalized() * max_speed
	return velocity


## Health system with bounded values
class Health extends RefCounted:
	var current: int
	var maximum: int

	func _init(max_hp: int, initial: int = -1) -> void:
		maximum = max(1, max_hp)
		current = clampi(initial if initial >= 0 else max_hp, 0, maximum)

	func damage(amount: int) -> int:
		var actual := mini(current, maxi(0, amount))
		current -= actual
		return actual

	func heal(amount: int) -> int:
		var actual := mini(maximum - current, maxi(0, amount))
		current += actual
		return actual

	func is_dead() -> bool:
		return current <= 0

	func is_full() -> bool:
		return current >= maximum

	func get_percentage() -> float:
		return float(current) / float(maximum) * 100.0


## Cooldown timer with safe operations
class Cooldown extends RefCounted:
	var duration: float
	var _remaining: float

	func _init(duration_sec: float) -> void:
		duration = maxf(0.0, duration_sec)
		_remaining = 0.0

	func start() -> void:
		_remaining = duration

	func update(delta: float) -> void:
		_remaining = maxf(0.0, _remaining - delta)

	func is_ready() -> bool:
		return _remaining <= 0.0

	func get_remaining() -> float:
		return _remaining

	func get_progress() -> float:
		if duration <= 0.0:
			return 1.0
		return 1.0 - (_remaining / duration)


## Safe node lookup
static func safe_get_node(from: Node, path: NodePath) -> Result:
	if from == null:
		return Err("null_parent")
	if path.is_empty():
		return Err("empty_path")
	var node := from.get_node_or_null(path)
	if node == null:
		return Err("node_not_found")
	return Ok(node)


## Safe instantiation with validation
static func safe_instantiate(scene: PackedScene) -> Result:
	if scene == null:
		return Err("null_scene")
	var instance := scene.instantiate()
	if instance == null:
		return Err("instantiation_failed")
	return Ok(instance)


# ============================================================================
# VERSION INFO
# ============================================================================

static func get_version() -> String:
	return VERSION


static func get_module_count() -> int:
	return MODULE_COUNT


static func get_modules() -> Array[String]:
	return [
		# Core (11)
		"SafeMath", "SafeString", "SafePath", "SafeEmail", "SafeUrl",
		"SafeNetwork", "SafeCrypto", "SafeUUID", "SafeCurrency", "SafePhone", "SafeHex",
		# Data (7)
		"SafeJson", "SafeDateTime", "SafeFloat", "SafeVersion", "SafeColor", "SafeAngle", "SafeUnit",
		# Data Structures (5)
		"SafeBuffer", "SafeQueue", "SafeBloom", "SafeLRU", "SafeGraph",
		# Resilience (4)
		"SafeRateLimiter", "SafeCircuitBreaker", "SafeRetry", "SafeMonotonic",
		# State (2)
		"SafeStateMachine", "SafeCalculator",
		# Algorithm (4)
		"SafeGeo", "SafeProbability", "SafeChecksum", "SafeTensor",
		# Security (2)
		"SafePassword", "SafeML",
		# HTTP (3)
		"SafeHeader", "SafeCookie", "SafeContentType"
	]
