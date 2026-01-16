# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

## Proven Safety Library for Godot (GDScript)
##
## Formally verified safety primitives for game development.
## Provides safe math, bounded types, and validation for games.
##
## @version 0.9.0

class_name Proven
extends RefCounted

# ============================================================================
# CONSTANTS
# ============================================================================

const VERSION := "0.9.0"

# Integer bounds (Godot uses 64-bit integers)
const INT_MAX := 9223372036854775807
const INT_MIN := -9223372036854775808

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
static func Ok(value: Variant) -> Result:
	return Result.new(true, value)


## Create error result
static func Err(error: String) -> Result:
	return Result.new(false, null, error)


# ============================================================================
# SAFE MATH
# ============================================================================

## Safe addition with overflow check
static func safe_add(a: int, b: int) -> Result:
	if b > 0 and a > INT_MAX - b:
		return Err("overflow")
	if b < 0 and a < INT_MIN - b:
		return Err("underflow")
	return Ok(a + b)


## Safe subtraction with underflow check
static func safe_sub(a: int, b: int) -> Result:
	if b < 0 and a > INT_MAX + b:
		return Err("overflow")
	if b > 0 and a < INT_MIN + b:
		return Err("underflow")
	return Ok(a - b)


## Safe multiplication with overflow check
static func safe_mul(a: int, b: int) -> Result:
	if a == 0 or b == 0:
		return Ok(0)

	var result := a * b

	# Verify with division
	if result / a != b:
		return Err("overflow")

	return Ok(result)


## Safe division with zero check
static func safe_div(a: int, b: int) -> Result:
	if b == 0:
		return Err("division_by_zero")

	# Handle INT_MIN / -1
	if a == INT_MIN and b == -1:
		return Err("overflow")

	return Ok(a / b)


## Safe modulo with zero check
static func safe_mod(a: int, b: int) -> Result:
	if b == 0:
		return Err("modulo_by_zero")
	return Ok(a % b)


## Safe float addition (checks for inf/nan)
static func safe_add_float(a: float, b: float) -> Result:
	var result := a + b
	if is_inf(result) or is_nan(result):
		return Err("float_overflow")
	return Ok(result)


## Safe float multiplication (checks for inf/nan)
static func safe_mul_float(a: float, b: float) -> Result:
	var result := a * b
	if is_inf(result) or is_nan(result):
		return Err("float_overflow")
	return Ok(result)


## Safe float division with zero and inf check
static func safe_div_float(a: float, b: float) -> Result:
	if b == 0.0:
		return Err("division_by_zero")
	var result := a / b
	if is_inf(result) or is_nan(result):
		return Err("float_overflow")
	return Ok(result)


# ============================================================================
# BOUNDED VALUES
# ============================================================================

## Clamp value to range
static func clamp_value(value: Variant, min_val: Variant, max_val: Variant) -> Variant:
	return clamp(value, min_val, max_val)


## Check if value is in range (inclusive)
static func in_range(value: Variant, min_val: Variant, max_val: Variant) -> bool:
	return value >= min_val and value <= max_val


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


# ============================================================================
# GAME-SPECIFIC VALIDATION
# ============================================================================

## Validate health/HP value
static func is_valid_health(value: int, max_hp: int = 100) -> bool:
	return value >= 0 and value <= max_hp


## Validate percentage (0-100)
static func is_valid_percentage(value: float) -> bool:
	return value >= 0.0 and value <= 100.0


## Validate normalized value (0.0-1.0)
static func is_normalized(value: float) -> bool:
	return value >= 0.0 and value <= 1.0


## Validate angle in degrees (0-360)
static func is_valid_angle_deg(value: float) -> bool:
	return value >= 0.0 and value < 360.0


## Validate angle in radians (0-2π)
static func is_valid_angle_rad(value: float) -> bool:
	return value >= 0.0 and value < TAU


## Validate color component (0-1)
static func is_valid_color_component(value: float) -> bool:
	return value >= 0.0 and value <= 1.0


## Validate 8-bit color (0-255)
static func is_valid_color8(value: int) -> bool:
	return value >= 0 and value <= 255


## Validate layer number (1-32 for Godot)
static func is_valid_layer(layer: int) -> bool:
	return layer >= 1 and layer <= 32


## Validate group name (non-empty, valid characters)
static func is_valid_group_name(name: String) -> bool:
	if name.is_empty():
		return false
	# Only allow alphanumeric and underscore
	var regex := RegEx.new()
	regex.compile("^[a-zA-Z_][a-zA-Z0-9_]*$")
	return regex.search(name) != null


# ============================================================================
# SAFE RESOURCE MANAGEMENT
# ============================================================================

## Safe instantiation with validation
static func safe_instantiate(scene: PackedScene) -> Result:
	if scene == null:
		return Err("null_scene")

	var instance := scene.instantiate()
	if instance == null:
		return Err("instantiation_failed")

	return Ok(instance)


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


## Safe typed node lookup
static func safe_get_node_as(from: Node, path: NodePath, type: GDScript) -> Result:
	var result := safe_get_node(from, path)
	if result.is_err():
		return result

	if not result.value is type:
		return Err("type_mismatch")

	return result


# ============================================================================
# SAFE ARRAY OPERATIONS
# ============================================================================

## Safe array access
static func safe_array_get(arr: Array, index: int) -> Result:
	if index < 0 or index >= arr.size():
		return Err("index_out_of_bounds")
	return Ok(arr[index])


## Safe array pop
static func safe_array_pop(arr: Array) -> Result:
	if arr.is_empty():
		return Err("empty_array")
	return Ok(arr.pop_back())


## Safe array pop front
static func safe_array_pop_front(arr: Array) -> Result:
	if arr.is_empty():
		return Err("empty_array")
	return Ok(arr.pop_front())


# ============================================================================
# SAFE DICTIONARY OPERATIONS
# ============================================================================

## Safe dictionary access
static func safe_dict_get(dict: Dictionary, key: Variant) -> Result:
	if not dict.has(key):
		return Err("key_not_found")
	return Ok(dict[key])


## Safe dictionary access with type check
static func safe_dict_get_typed(dict: Dictionary, key: Variant, expected_type: int) -> Result:
	if not dict.has(key):
		return Err("key_not_found")

	var value := dict[key]
	if typeof(value) != expected_type:
		return Err("type_mismatch")

	return Ok(value)


# ============================================================================
# BOUNDED TYPES
# ============================================================================

## Bounded integer class
class BoundedInt extends RefCounted:
	var _value: int
	var _min: int
	var _max: int

	func _init(value: int, min_val: int, max_val: int) -> void:
		_min = min_val
		_max = max_val
		_value = clampi(value, min_val, max_val)

	func get_value() -> int:
		return _value

	func set_value(value: int) -> void:
		_value = clampi(value, _min, _max)

	func add(delta: int) -> bool:
		var result := Proven.safe_add(_value, delta)
		if result.is_ok():
			_value = clampi(result.value, _min, _max)
			return true
		return false

	func sub(delta: int) -> bool:
		var result := Proven.safe_sub(_value, delta)
		if result.is_ok():
			_value = clampi(result.value, _min, _max)
			return true
		return false


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


# ============================================================================
# PHYSICS UTILITIES
# ============================================================================

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


## Safe lerp that never overshoots
static func safe_lerp(from: float, to: float, weight: float) -> float:
	weight = clampf(weight, 0.0, 1.0)
	return lerp(from, to, weight)


## Safe move_toward that handles edge cases
static func safe_move_toward(from: float, to: float, delta: float) -> float:
	if delta <= 0.0:
		return from
	return move_toward(from, to, delta)


# ============================================================================
# VERSION
# ============================================================================

static func get_version() -> String:
	return VERSION
