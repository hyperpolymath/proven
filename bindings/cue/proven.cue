// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

// Proven Safety Primitives for CUE
//
// CUE's type system naturally aligns with dependent types.
// These definitions provide safe, bounded types for configurations.

package proven

import "strings"

// Version
#Version: "0.9.0"

// Safe bounded integer type
#BoundedInt: {
	value: int
	min:   int
	max:   int
	_check: value >= min & value <= max
}

// Safe non-negative integer
#NonNegInt: int & >=0

// Safe positive integer
#PosInt: int & >0

// Safe port number (1-65535)
#Port: int & >=1 & <=65535

// Common ports
#CommonPorts: {
	http:  80
	https: 443
	ssh:   22
	dns:   53
	mysql: 3306
	postgres: 5432
	redis: 6379
	mongodb: 27017
}

// Safe percentage (0-100)
#Percentage: int & >=0 & <=100

// Safe ratio (0.0-1.0)
#Ratio: float & >=0.0 & <=1.0

// Safe non-empty string
#NonEmptyString: string & !=""

// Safe email (basic validation)
#Email: string & =~"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"

// Safe URL
#URL: string & =~"^https?://[^\\s]+$"

// Safe hostname
#Hostname: string & =~"^[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(\\.[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"

// Safe IPv4 address
#IPv4: string & =~"^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"

// Safe CIDR notation
#CIDR: string & =~"^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)/([0-9]|[1-2][0-9]|3[0-2])$"

// Memory size with units
#Memory: {
	value: #PosInt
	unit:  "B" | "KB" | "MB" | "GB" | "TB"

	// Computed bytes
	_bytes: {
		if unit == "B" { value }
		if unit == "KB" { value * 1024 }
		if unit == "MB" { value * 1024 * 1024 }
		if unit == "GB" { value * 1024 * 1024 * 1024 }
		if unit == "TB" { value * 1024 * 1024 * 1024 * 1024 }
	}
	bytes: _bytes
}

// CPU resources with units
#CPU: {
	value: float & >0
	unit:  "m" | "cores" | *"cores"

	// Computed millicores
	_millicores: {
		if unit == "m" { int(value) }
		if unit == "cores" { int(value * 1000) }
	}
	millicores: _millicores
}

// Safe replica count
#Replicas: {
	count: #PosInt
	min:   #PosInt | *1
	max:   #PosInt | *100

	_valid: count >= min & count <= max
}

// Resource constraints (Kubernetes-style)
#ResourceConstraints: {
	requests?: {
		memory?: #Memory
		cpu?:    #CPU
	}
	limits?: {
		memory?: #Memory
		cpu?:    #CPU
	}

	// Validation: requests <= limits when both specified
	if requests != _|_ & limits != _|_ {
		if requests.memory != _|_ & limits.memory != _|_ {
			requests.memory.bytes <= limits.memory.bytes
		}
		if requests.cpu != _|_ & limits.cpu != _|_ {
			requests.cpu.millicores <= limits.cpu.millicores
		}
	}
}

// Safe duration
#Duration: {
	value: #PosInt
	unit:  "ms" | "s" | "m" | "h" | "d"

	// Computed milliseconds
	_ms: {
		if unit == "ms" { value }
		if unit == "s" { value * 1000 }
		if unit == "m" { value * 60 * 1000 }
		if unit == "h" { value * 3600 * 1000 }
		if unit == "d" { value * 86400 * 1000 }
	}
	milliseconds: _ms
}

// Safe timeout (with reasonable bounds)
#Timeout: #Duration & {
	milliseconds: >=100 & <=3600000 // 100ms to 1 hour
}

// Environment variable (no secrets in plain text)
#EnvVar: {
	name:  =~"^[A-Z][A-Z0-9_]*$"
	value: string
	// Warn if it looks like a secret
	_notSecret: !strings.Contains(strings.ToLower(name), "password") &
		!strings.Contains(strings.ToLower(name), "secret") &
		!strings.Contains(strings.ToLower(name), "token") &
		!strings.Contains(strings.ToLower(name), "key")
}

// Secret reference (reference, not value)
#SecretRef: {
	name: #NonEmptyString
	key:  #NonEmptyString
}

// Health check probe
#Probe: {
	type: "http" | "tcp" | "exec"

	if type == "http" {
		path: string & =~"^/"
		port: #Port
	}
	if type == "tcp" {
		port: #Port
	}
	if type == "exec" {
		command: [...#NonEmptyString] & [_, ...]
	}

	initialDelay?: #Duration
	interval?:     #Duration
	timeout?:      #Duration & {milliseconds: <interval.milliseconds} // timeout < interval
	retries?:      int & >=1 & <=10
}

// Service definition
#Service: {
	name:     #NonEmptyString & =~"^[a-z][a-z0-9-]*$"
	port:     #Port
	protocol: "TCP" | "UDP" | *"TCP"
	targetPort?: #Port
}
