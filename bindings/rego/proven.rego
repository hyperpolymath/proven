# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# Proven Safety Primitives for Rego (Open Policy Agent)
#
# Provides reusable safety rules and validation functions
# for use in OPA policies.

package proven

import future.keywords.if
import future.keywords.in
import future.keywords.contains

# Version
version := "0.9.0"

# ============================================================================
# RESULT TYPE HELPERS
# ============================================================================

ok(value) := {"ok": true, "value": value}
err(error) := {"ok": false, "error": error}

is_ok(result) if result.ok == true
is_err(result) if result.ok == false

unwrap_or(result, default) := result.value if is_ok(result)
unwrap_or(result, default) := default if is_err(result)

# ============================================================================
# STRING VALIDATION
# ============================================================================

# Validate non-empty string
validate_non_empty(s) := ok(s) if {
	is_string(s)
	count(s) > 0
}
validate_non_empty(s) := err("String cannot be empty") if {
	is_string(s)
	count(s) == 0
}
validate_non_empty(s) := err("Not a string") if not is_string(s)

# Validate max length
validate_max_length(s, max) := ok(s) if {
	is_string(s)
	count(s) <= max
}
validate_max_length(s, max) := err(sprintf("String exceeds max length %d", [max])) if {
	is_string(s)
	count(s) > max
}

# Validate email format
validate_email(s) := ok(s) if {
	is_string(s)
	regex.match(`^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$`, s)
}
validate_email(s) := err("Invalid email format") if {
	is_string(s)
	not regex.match(`^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$`, s)
}

# ============================================================================
# NUMERIC VALIDATION
# ============================================================================

# Validate port number
validate_port(port) := ok(port) if {
	is_number(port)
	port >= 1
	port <= 65535
}
validate_port(port) := err("Port must be between 1 and 65535") if {
	is_number(port)
	not port >= 1
}
validate_port(port) := err("Port must be between 1 and 65535") if {
	is_number(port)
	not port <= 65535
}

# Validate positive number
validate_positive(n) := ok(n) if {
	is_number(n)
	n > 0
}
validate_positive(n) := err("Number must be positive") if {
	is_number(n)
	n <= 0
}

# Validate range
validate_range(n, min, max) := ok(n) if {
	is_number(n)
	n >= min
	n <= max
}
validate_range(n, min, max) := err(sprintf("Number must be between %d and %d", [min, max])) if {
	is_number(n)
	not n >= min
}
validate_range(n, min, max) := err(sprintf("Number must be between %d and %d", [min, max])) if {
	is_number(n)
	not n <= max
}

# ============================================================================
# NETWORK VALIDATION
# ============================================================================

# Common ports
common_ports := {
	"http": 80,
	"https": 443,
	"ssh": 22,
	"dns": 53,
	"mysql": 3306,
	"postgres": 5432,
	"redis": 6379,
}

# Validate CIDR
validate_cidr(cidr) := ok(cidr) if {
	is_string(cidr)
	regex.match(`^(\d{1,3}\.){3}\d{1,3}/\d{1,2}$`, cidr)
	parts := split(cidr, "/")
	mask := to_number(parts[1])
	mask >= 0
	mask <= 32
}
validate_cidr(cidr) := err("Invalid CIDR notation") if {
	is_string(cidr)
	not regex.match(`^(\d{1,3}\.){3}\d{1,3}/\d{1,2}$`, cidr)
}

# Validate IPv4
validate_ipv4(ip) := ok(ip) if {
	is_string(ip)
	parts := split(ip, ".")
	count(parts) == 4
	all_valid_octets(parts)
}
validate_ipv4(ip) := err("Invalid IPv4 address") if {
	is_string(ip)
	parts := split(ip, ".")
	count(parts) != 4
}

all_valid_octets(parts) if {
	every part in parts {
		n := to_number(part)
		n >= 0
		n <= 255
	}
}

# ============================================================================
# KUBERNETES RESOURCE VALIDATION
# ============================================================================

# Validate memory format (e.g., "512Mi", "1Gi")
validate_memory(mem) := ok(mem) if {
	is_string(mem)
	regex.match(`^\d+[EPTGMK]?i?$`, mem)
}
validate_memory(mem) := err("Invalid memory format") if {
	is_string(mem)
	not regex.match(`^\d+[EPTGMK]?i?$`, mem)
}

# Validate CPU format (e.g., "100m", "1.5")
validate_cpu(cpu) := ok(cpu) if {
	is_string(cpu)
	regex.match(`^\d+(\.\d+)?m?$`, cpu)
}
validate_cpu(cpu) := err("Invalid CPU format") if {
	is_string(cpu)
	not regex.match(`^\d+(\.\d+)?m?$`, cpu)
}

# Validate replica count
validate_replicas(n, min, max) := validate_range(n, min, max)
validate_replicas_default(n) := validate_range(n, 1, 100)

# ============================================================================
# SECURITY RULES
# ============================================================================

# Check if container runs as non-root
deny_root_container contains msg if {
	input.kind == "Pod"
	container := input.spec.containers[_]
	not container.securityContext.runAsNonRoot == true
	msg := sprintf("Container %s must run as non-root", [container.name])
}

# Check if privileged mode is disabled
deny_privileged contains msg if {
	input.kind == "Pod"
	container := input.spec.containers[_]
	container.securityContext.privileged == true
	msg := sprintf("Container %s must not run in privileged mode", [container.name])
}

# Check for required resource limits
deny_missing_limits contains msg if {
	input.kind == "Pod"
	container := input.spec.containers[_]
	not container.resources.limits
	msg := sprintf("Container %s must have resource limits", [container.name])
}

# Check for dangerous capabilities
deny_dangerous_capabilities contains msg if {
	input.kind == "Pod"
	container := input.spec.containers[_]
	cap := container.securityContext.capabilities.add[_]
	dangerous := {"SYS_ADMIN", "NET_ADMIN", "ALL"}
	cap in dangerous
	msg := sprintf("Container %s has dangerous capability: %s", [container.name, cap])
}

# ============================================================================
# VALIDATION COMBINATORS
# ============================================================================

# All validations must pass
validate_all(results) := ok([r.value | r := results[_]; is_ok(r)]) if {
	not any_err(results)
}
validate_all(results) := first_err(results) if {
	any_err(results)
}

any_err(results) if {
	some r in results
	is_err(r)
}

first_err(results) := r if {
	r := results[_]
	is_err(r)
}

# Validate object field
validate_field(obj, field, validator) := validator(obj[field]) if {
	obj[field]
}
validate_field(obj, field, validator) := err(sprintf("Missing field: %s", [field])) if {
	not obj[field]
}
