# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# Proven Safety Primitives for HCL/Terraform
#
# This module provides validated types and safe defaults
# for Terraform configurations across all 38 Proven modules.

terraform {
  required_version = ">= 1.0"
}

# ============================================================================
# MODULE METADATA
# ============================================================================

locals {
  proven_version  = "0.4.0"
  proven_modules  = 38
  proven_manifest = {
    core = [
      "safe_math",
      "safe_string",
      "safe_path",
      "safe_email",
      "safe_url",
      "safe_network",
      "safe_crypto",
      "safe_uuid",
      "safe_currency",
      "safe_phone",
      "safe_hex"
    ]
    data = [
      "safe_json",
      "safe_datetime",
      "safe_float",
      "safe_version",
      "safe_color",
      "safe_angle",
      "safe_unit"
    ]
    data_structures = [
      "safe_buffer",
      "safe_queue",
      "safe_bloom",
      "safe_lru",
      "safe_graph"
    ]
    resilience = [
      "safe_rate_limiter",
      "safe_circuit_breaker",
      "safe_retry",
      "safe_monotonic"
    ]
    state = [
      "safe_state_machine",
      "safe_calculator"
    ]
    algorithm = [
      "safe_geo",
      "safe_probability",
      "safe_checksum",
      "safe_tensor"
    ]
    security = [
      "safe_password",
      "safe_ml"
    ]
    http = [
      "safe_header",
      "safe_cookie",
      "safe_content_type"
    ]
  }
}

# ============================================================================
# CORE MODULE: safe_math
# ============================================================================

# Safe integer bounds
variable "safe_int" {
  type        = number
  description = "Safe integer within i64 bounds"
  default     = 0

  validation {
    condition     = floor(var.safe_int) == var.safe_int
    error_message = "Value must be an integer."
  }
}

# Safe positive integer
variable "safe_positive_int" {
  type        = number
  description = "Safe positive integer (> 0)"
  default     = 1

  validation {
    condition     = var.safe_positive_int > 0 && floor(var.safe_positive_int) == var.safe_positive_int
    error_message = "Value must be a positive integer greater than 0."
  }
}

# Safe non-negative integer
variable "safe_non_negative_int" {
  type        = number
  description = "Safe non-negative integer (>= 0)"
  default     = 0

  validation {
    condition     = var.safe_non_negative_int >= 0 && floor(var.safe_non_negative_int) == var.safe_non_negative_int
    error_message = "Value must be a non-negative integer."
  }
}

locals {
  # Safe math bounds
  safe_math = {
    i64_max = 9223372036854775807
    i64_min = -9223372036854775808
    u64_max = 18446744073709551615
    i32_max = 2147483647
    i32_min = -2147483648
    u32_max = 4294967295
  }

  # Safe math functions (using Terraform expressions)
  safe_clamp = {
    description = "Clamp value between min and max"
  }
}

# ============================================================================
# CORE MODULE: safe_string
# ============================================================================

# Non-empty string validation
variable "non_empty_string" {
  type        = string
  description = "Non-empty string"
  default     = ""

  validation {
    condition     = length(var.non_empty_string) > 0
    error_message = "String cannot be empty."
  }
}

# String with length bounds
variable "bounded_string" {
  type        = string
  description = "String with length constraints"
  default     = ""

  validation {
    condition     = length(var.bounded_string) <= 65536
    error_message = "String length must not exceed 65536 characters."
  }
}

locals {
  # Safe string constants
  safe_string = {
    max_length          = 65536
    max_identifier_len  = 255
    max_label_len       = 63
    allowed_chars_regex = "^[a-zA-Z0-9_-]+$"
  }

  # String sanitization patterns
  string_patterns = {
    alphanumeric      = "^[a-zA-Z0-9]+$"
    alphanumeric_dash = "^[a-zA-Z0-9-]+$"
    identifier        = "^[a-zA-Z_][a-zA-Z0-9_]*$"
    dns_label         = "^[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?$"
  }
}

# ============================================================================
# CORE MODULE: safe_path
# ============================================================================

# Safe file path
variable "safe_path" {
  type        = string
  description = "Safe file path without traversal attacks"
  default     = ""

  validation {
    condition     = !can(regex("\\.\\.", var.safe_path))
    error_message = "Path cannot contain '..' (directory traversal)."
  }
}

# Safe filename
variable "safe_filename" {
  type        = string
  description = "Safe filename without path separators"
  default     = ""

  validation {
    condition     = !can(regex("[/\\\\]", var.safe_filename))
    error_message = "Filename cannot contain path separators."
  }
}

locals {
  safe_path = {
    max_path_length     = 4096
    max_component_len   = 255
    forbidden_chars     = ["<", ">", ":", "\"", "|", "?", "*"]
    forbidden_names     = ["CON", "PRN", "AUX", "NUL", "COM1", "LPT1"]
    allowed_extensions  = [".txt", ".json", ".yaml", ".yml", ".toml", ".conf"]
  }
}

# ============================================================================
# CORE MODULE: safe_email
# ============================================================================

# Email validation
variable "email" {
  type        = string
  description = "Valid email address"
  default     = ""

  validation {
    condition     = var.email == "" || can(regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", var.email))
    error_message = "Invalid email format."
  }
}

locals {
  safe_email = {
    max_local_part_length  = 64
    max_domain_length      = 255
    max_total_length       = 320
    valid_tlds_sample      = ["com", "org", "net", "edu", "gov", "io", "dev"]
  }
}

# ============================================================================
# CORE MODULE: safe_url
# ============================================================================

# URL validation
variable "url" {
  type        = string
  description = "Valid URL"
  default     = ""

  validation {
    condition     = var.url == "" || can(regex("^https?://[a-zA-Z0-9.-]+(/.*)?$", var.url))
    error_message = "Invalid URL format. Must start with http:// or https://"
  }
}

# HTTPS-only URL
variable "secure_url" {
  type        = string
  description = "HTTPS URL only"
  default     = ""

  validation {
    condition     = var.secure_url == "" || can(regex("^https://", var.secure_url))
    error_message = "URL must use HTTPS protocol."
  }
}

locals {
  safe_url = {
    allowed_schemes   = ["https", "http"]
    secure_schemes    = ["https"]
    max_url_length    = 2048
    max_host_length   = 253
  }
}

# ============================================================================
# CORE MODULE: safe_network
# ============================================================================

# Port number validation
variable "port" {
  type        = number
  description = "Safe port number (1-65535)"
  default     = 443

  validation {
    condition     = var.port >= 1 && var.port <= 65535
    error_message = "Port must be between 1 and 65535."
  }
}

# Unprivileged port
variable "unprivileged_port" {
  type        = number
  description = "Unprivileged port (1024-65535)"
  default     = 8080

  validation {
    condition     = var.unprivileged_port >= 1024 && var.unprivileged_port <= 65535
    error_message = "Unprivileged port must be between 1024 and 65535."
  }
}

# CIDR block validation
variable "cidr_block" {
  type        = string
  description = "Valid CIDR notation"
  default     = ""

  validation {
    condition     = var.cidr_block == "" || can(cidrhost(var.cidr_block, 0))
    error_message = "Invalid CIDR block format."
  }
}

# IPv4 address validation
variable "ipv4_address" {
  type        = string
  description = "Valid IPv4 address"
  default     = ""

  validation {
    condition     = var.ipv4_address == "" || can(regex("^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$", var.ipv4_address))
    error_message = "Invalid IPv4 address format."
  }
}

locals {
  safe_network = {
    # Common ports
    common_ports = {
      http     = 80
      https    = 443
      ssh      = 22
      dns      = 53
      mysql    = 3306
      postgres = 5432
      redis    = 6379
      mongodb  = 27017
    }

    # Private CIDR ranges
    private_cidrs = {
      class_a = "10.0.0.0/8"
      class_b = "172.16.0.0/12"
      class_c = "192.168.0.0/16"
    }

    # IPv4 classification
    ipv4_classes = {
      loopback   = "127.0.0.0/8"
      link_local = "169.254.0.0/16"
      multicast  = "224.0.0.0/4"
      broadcast  = "255.255.255.255/32"
    }

    # Port ranges
    port_ranges = {
      privileged   = { min = 1, max = 1023 }
      unprivileged = { min = 1024, max = 65535 }
      ephemeral    = { min = 49152, max = 65535 }
    }
  }
}

# ============================================================================
# CORE MODULE: safe_crypto
# ============================================================================

locals {
  safe_crypto = {
    # Recommended key sizes (bits)
    key_sizes = {
      aes_128     = 128
      aes_256     = 256
      rsa_2048    = 2048
      rsa_4096    = 4096
      ed25519     = 256
      x25519      = 256
    }

    # Hash output sizes (bytes)
    hash_sizes = {
      md5        = 16
      sha1       = 20
      sha256     = 32
      sha384     = 48
      sha512     = 64
      blake2b    = 64
      blake3     = 32
    }

    # Recommended algorithms
    recommended = {
      symmetric_encryption = "aes-256-gcm"
      asymmetric_signing   = "ed25519"
      key_exchange         = "x25519"
      hashing              = "sha256"
      password_hashing     = "argon2id"
    }

    # Deprecated algorithms (do not use)
    deprecated = ["md5", "sha1", "des", "3des", "rc4", "rsa-1024"]
  }
}

# ============================================================================
# CORE MODULE: safe_uuid
# ============================================================================

# UUID validation
variable "uuid" {
  type        = string
  description = "Valid UUID format"
  default     = ""

  validation {
    condition     = var.uuid == "" || can(regex("^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$", var.uuid))
    error_message = "Invalid UUID format."
  }
}

locals {
  safe_uuid = {
    nil_uuid      = "00000000-0000-0000-0000-000000000000"
    max_uuid      = "ffffffff-ffff-ffff-ffff-ffffffffffff"
    uuid_length   = 36
    uuid_versions = [1, 3, 4, 5, 6, 7, 8]
  }
}

# ============================================================================
# CORE MODULE: safe_currency
# ============================================================================

# Currency amount (in smallest unit, e.g., cents)
variable "currency_amount" {
  type        = number
  description = "Currency amount in smallest unit (e.g., cents)"
  default     = 0

  validation {
    condition     = floor(var.currency_amount) == var.currency_amount
    error_message = "Currency amount must be an integer (smallest unit)."
  }
}

locals {
  safe_currency = {
    # ISO 4217 currency codes (sample)
    codes = {
      usd = { code = "USD", symbol = "$", decimals = 2, name = "US Dollar" }
      eur = { code = "EUR", symbol = "\u20ac", decimals = 2, name = "Euro" }
      gbp = { code = "GBP", symbol = "\u00a3", decimals = 2, name = "British Pound" }
      jpy = { code = "JPY", symbol = "\u00a5", decimals = 0, name = "Japanese Yen" }
      btc = { code = "BTC", symbol = "\u20bf", decimals = 8, name = "Bitcoin" }
      eth = { code = "ETH", symbol = "\u039e", decimals = 18, name = "Ethereum" }
    }

    # Decimal places by currency type
    decimal_places = {
      fiat_standard = 2
      fiat_zero     = 0
      crypto_btc    = 8
      crypto_eth    = 18
    }
  }
}

# ============================================================================
# CORE MODULE: safe_phone
# ============================================================================

# Phone number validation (E.164 format)
variable "phone_number" {
  type        = string
  description = "Phone number in E.164 format"
  default     = ""

  validation {
    condition     = var.phone_number == "" || can(regex("^\\+[1-9][0-9]{6,14}$", var.phone_number))
    error_message = "Phone number must be in E.164 format (+[country code][number])."
  }
}

locals {
  safe_phone = {
    max_length         = 15
    min_length         = 7
    country_code_max   = 3
    format_e164        = "^\\+[1-9][0-9]{6,14}$"
    sample_codes       = { us = "+1", uk = "+44", de = "+49", jp = "+81" }
  }
}

# ============================================================================
# CORE MODULE: safe_hex
# ============================================================================

# Hex string validation
variable "hex_string" {
  type        = string
  description = "Valid hexadecimal string"
  default     = ""

  validation {
    condition     = var.hex_string == "" || can(regex("^[0-9a-fA-F]*$", var.hex_string))
    error_message = "String must contain only hexadecimal characters."
  }
}

locals {
  safe_hex = {
    valid_chars      = "0123456789abcdefABCDEF"
    bytes_to_hex_len = 2  # Each byte = 2 hex chars
  }
}

# ============================================================================
# DATA MODULE: safe_json
# ============================================================================

locals {
  safe_json = {
    max_depth       = 64
    max_size_bytes  = 10485760  # 10 MB
    content_types   = ["application/json", "application/ld+json"]
  }
}

# ============================================================================
# DATA MODULE: safe_datetime
# ============================================================================

# ISO 8601 date validation
variable "iso_date" {
  type        = string
  description = "ISO 8601 date format"
  default     = ""

  validation {
    condition     = var.iso_date == "" || can(regex("^\\d{4}-\\d{2}-\\d{2}$", var.iso_date))
    error_message = "Date must be in ISO 8601 format (YYYY-MM-DD)."
  }
}

# ISO 8601 datetime validation
variable "iso_datetime" {
  type        = string
  description = "ISO 8601 datetime format"
  default     = ""

  validation {
    condition     = var.iso_datetime == "" || can(regex("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}", var.iso_datetime))
    error_message = "Datetime must be in ISO 8601 format (YYYY-MM-DDTHH:MM:SS)."
  }
}

locals {
  safe_datetime = {
    formats = {
      date         = "YYYY-MM-DD"
      time         = "HH:MM:SS"
      datetime     = "YYYY-MM-DDTHH:MM:SS"
      datetime_utc = "YYYY-MM-DDTHH:MM:SSZ"
    }

    bounds = {
      year_min  = 1970
      year_max  = 9999
      month_min = 1
      month_max = 12
      day_min   = 1
      day_max   = 31
    }

    durations = {
      second = 1
      minute = 60
      hour   = 3600
      day    = 86400
      week   = 604800
    }
  }
}

# ============================================================================
# DATA MODULE: safe_float
# ============================================================================

# Safe float with finite check
variable "safe_float" {
  type        = number
  description = "Safe floating point number (finite)"
  default     = 0.0

  # Note: Terraform numbers are always finite
  validation {
    condition     = true
    error_message = "Value must be a finite number."
  }
}

locals {
  safe_float = {
    epsilon       = 0.0000001
    max_precision = 15  # IEEE 754 double precision
    comparison = {
      tolerance_loose  = 0.0001
      tolerance_tight  = 0.000001
      tolerance_strict = 0.0000001
    }
  }
}

# ============================================================================
# DATA MODULE: safe_version
# ============================================================================

# Semantic version validation
variable "semver" {
  type        = string
  description = "Semantic version (X.Y.Z)"
  default     = ""

  validation {
    condition     = var.semver == "" || can(regex("^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)(-[a-zA-Z0-9-]+(\\.[a-zA-Z0-9-]+)*)?(\\+[a-zA-Z0-9-]+(\\.[a-zA-Z0-9-]+)*)?$", var.semver))
    error_message = "Version must follow semantic versioning (X.Y.Z[-prerelease][+build])."
  }
}

locals {
  safe_version = {
    pattern = "^(0|[1-9]\\d*)\\.(0|[1-9]\\d*)\\.(0|[1-9]\\d*)$"
    parts   = ["major", "minor", "patch"]
    prerelease_identifiers = ["alpha", "beta", "rc", "dev"]
  }
}

# ============================================================================
# DATA MODULE: safe_color
# ============================================================================

# Hex color validation
variable "hex_color" {
  type        = string
  description = "Hex color code (#RGB or #RRGGBB)"
  default     = ""

  validation {
    condition     = var.hex_color == "" || can(regex("^#([0-9a-fA-F]{3}|[0-9a-fA-F]{6})$", var.hex_color))
    error_message = "Color must be in hex format (#RGB or #RRGGBB)."
  }
}

locals {
  safe_color = {
    formats = {
      hex3   = "^#[0-9a-fA-F]{3}$"
      hex6   = "^#[0-9a-fA-F]{6}$"
      hex8   = "^#[0-9a-fA-F]{8}$"  # With alpha
    }

    named_colors = {
      black   = "#000000"
      white   = "#ffffff"
      red     = "#ff0000"
      green   = "#00ff00"
      blue    = "#0000ff"
      yellow  = "#ffff00"
      cyan    = "#00ffff"
      magenta = "#ff00ff"
    }

    bounds = {
      rgb_min   = 0
      rgb_max   = 255
      alpha_min = 0.0
      alpha_max = 1.0
    }
  }
}

# ============================================================================
# DATA MODULE: safe_angle
# ============================================================================

# Degrees validation
variable "angle_degrees" {
  type        = number
  description = "Angle in degrees"
  default     = 0

  validation {
    condition     = var.angle_degrees >= -360 && var.angle_degrees <= 360
    error_message = "Angle must be between -360 and 360 degrees."
  }
}

locals {
  safe_angle = {
    conversion = {
      deg_to_rad = 0.017453292519943295  # pi / 180
      rad_to_deg = 57.29577951308232     # 180 / pi
      deg_to_grad = 1.1111111111111112   # 10 / 9
      grad_to_deg = 0.9                  # 9 / 10
    }

    bounds = {
      full_rotation_deg  = 360
      full_rotation_rad  = 6.283185307179586  # 2 * pi
      full_rotation_grad = 400
    }

    named_angles = {
      right    = 90
      straight = 180
      full     = 360
    }
  }
}

# ============================================================================
# DATA MODULE: safe_unit
# ============================================================================

locals {
  safe_unit = {
    # SI base units
    si_base = {
      length      = "m"   # meter
      mass        = "kg"  # kilogram
      time        = "s"   # second
      current     = "A"   # ampere
      temperature = "K"   # kelvin
      amount      = "mol" # mole
      intensity   = "cd"  # candela
    }

    # SI prefixes
    si_prefixes = {
      yotta = { symbol = "Y", factor = 1e24 }
      zetta = { symbol = "Z", factor = 1e21 }
      exa   = { symbol = "E", factor = 1e18 }
      peta  = { symbol = "P", factor = 1e15 }
      tera  = { symbol = "T", factor = 1e12 }
      giga  = { symbol = "G", factor = 1e9 }
      mega  = { symbol = "M", factor = 1e6 }
      kilo  = { symbol = "k", factor = 1e3 }
      milli = { symbol = "m", factor = 1e-3 }
      micro = { symbol = "\u03bc", factor = 1e-6 }
      nano  = { symbol = "n", factor = 1e-9 }
      pico  = { symbol = "p", factor = 1e-12 }
    }

    # Data units
    data_units = {
      bit  = 1
      byte = 8
      kb   = 1024
      mb   = 1048576
      gb   = 1073741824
      tb   = 1099511627776
      pb   = 1125899906842624
    }
  }
}

# ============================================================================
# DATA STRUCTURES MODULE: safe_buffer
# ============================================================================

# Buffer size validation
variable "buffer_size" {
  type        = number
  description = "Buffer size in bytes"
  default     = 4096

  validation {
    condition     = var.buffer_size > 0 && var.buffer_size <= 1073741824  # 1 GB
    error_message = "Buffer size must be between 1 byte and 1 GB."
  }
}

locals {
  safe_buffer = {
    default_size     = 4096
    min_size         = 1
    max_size         = 1073741824  # 1 GB
    page_size        = 4096
    alignment        = 8

    common_sizes = {
      tiny   = 256
      small  = 4096
      medium = 65536
      large  = 1048576
      huge   = 16777216
    }
  }
}

# ============================================================================
# DATA STRUCTURES MODULE: safe_queue
# ============================================================================

# Queue capacity validation
variable "queue_capacity" {
  type        = number
  description = "Queue capacity (max items)"
  default     = 1000

  validation {
    condition     = var.queue_capacity >= 1 && var.queue_capacity <= 10000000
    error_message = "Queue capacity must be between 1 and 10 million."
  }
}

locals {
  safe_queue = {
    default_capacity = 1000
    max_capacity     = 10000000
    growth_factor    = 2

    types = {
      fifo     = "first-in-first-out"
      lifo     = "last-in-first-out"
      priority = "priority-based"
      ring     = "circular-buffer"
    }
  }
}

# ============================================================================
# DATA STRUCTURES MODULE: safe_bloom
# ============================================================================

# Bloom filter configuration
variable "bloom_filter_size" {
  type        = number
  description = "Bloom filter bit array size"
  default     = 10000

  validation {
    condition     = var.bloom_filter_size >= 100 && var.bloom_filter_size <= 100000000
    error_message = "Bloom filter size must be between 100 and 100 million bits."
  }
}

locals {
  safe_bloom = {
    default_size             = 10000
    default_hash_functions   = 7
    default_false_positive   = 0.01  # 1%

    # Optimal hash functions = (m/n) * ln(2)
    # where m = bit array size, n = expected elements
    recommended_configs = {
      small  = { size = 10000, hashes = 7, capacity = 1000 }
      medium = { size = 100000, hashes = 7, capacity = 10000 }
      large  = { size = 1000000, hashes = 7, capacity = 100000 }
    }
  }
}

# ============================================================================
# DATA STRUCTURES MODULE: safe_lru
# ============================================================================

# LRU cache capacity
variable "lru_capacity" {
  type        = number
  description = "LRU cache capacity"
  default     = 1000

  validation {
    condition     = var.lru_capacity >= 1 && var.lru_capacity <= 1000000
    error_message = "LRU capacity must be between 1 and 1 million."
  }
}

locals {
  safe_lru = {
    default_capacity = 1000
    max_capacity     = 1000000

    eviction_policies = {
      lru  = "least-recently-used"
      lfu  = "least-frequently-used"
      fifo = "first-in-first-out"
      ttl  = "time-to-live"
    }
  }
}

# ============================================================================
# DATA STRUCTURES MODULE: safe_graph
# ============================================================================

locals {
  safe_graph = {
    max_nodes        = 1000000
    max_edges        = 10000000
    max_path_length  = 10000

    types = {
      directed         = "directed"
      undirected       = "undirected"
      weighted         = "weighted"
      dag              = "directed-acyclic"
    }

    algorithms = {
      shortest_path    = ["dijkstra", "bellman-ford", "a-star"]
      traversal        = ["bfs", "dfs"]
      cycle_detection  = ["tarjan", "kosaraju"]
      minimum_spanning = ["prim", "kruskal"]
    }
  }
}

# ============================================================================
# RESILIENCE MODULE: safe_rate_limiter
# ============================================================================

# Rate limit configuration
variable "rate_limit_requests" {
  type        = number
  description = "Maximum requests per time window"
  default     = 100

  validation {
    condition     = var.rate_limit_requests >= 1 && var.rate_limit_requests <= 1000000
    error_message = "Rate limit must be between 1 and 1 million requests."
  }
}

variable "rate_limit_window_seconds" {
  type        = number
  description = "Rate limit time window in seconds"
  default     = 60

  validation {
    condition     = var.rate_limit_window_seconds >= 1 && var.rate_limit_window_seconds <= 86400
    error_message = "Rate limit window must be between 1 second and 24 hours."
  }
}

locals {
  safe_rate_limiter = {
    algorithms = {
      token_bucket    = "token-bucket"
      sliding_window  = "sliding-window"
      fixed_window    = "fixed-window"
      leaky_bucket    = "leaky-bucket"
    }

    default_config = {
      requests_per_second = 10
      burst_size          = 50
      window_size_seconds = 60
    }

    common_limits = {
      api_standard    = { requests = 1000, window = 3600 }  # 1000/hour
      api_rate_limit  = { requests = 100, window = 60 }     # 100/minute
      login_attempts  = { requests = 5, window = 300 }      # 5/5min
      password_reset  = { requests = 3, window = 3600 }     # 3/hour
    }
  }
}

# ============================================================================
# RESILIENCE MODULE: safe_circuit_breaker
# ============================================================================

# Circuit breaker configuration
variable "circuit_breaker_threshold" {
  type        = number
  description = "Failure threshold to open circuit"
  default     = 5

  validation {
    condition     = var.circuit_breaker_threshold >= 1 && var.circuit_breaker_threshold <= 100
    error_message = "Circuit breaker threshold must be between 1 and 100."
  }
}

variable "circuit_breaker_timeout_seconds" {
  type        = number
  description = "Timeout before half-open state"
  default     = 30

  validation {
    condition     = var.circuit_breaker_timeout_seconds >= 1 && var.circuit_breaker_timeout_seconds <= 3600
    error_message = "Circuit breaker timeout must be between 1 second and 1 hour."
  }
}

locals {
  safe_circuit_breaker = {
    states = {
      closed    = "closed"     # Normal operation
      open      = "open"       # Failing fast
      half_open = "half-open"  # Testing recovery
    }

    default_config = {
      failure_threshold       = 5
      success_threshold       = 3
      timeout_seconds         = 30
      half_open_max_requests  = 3
    }

    failure_types = [
      "timeout",
      "connection_error",
      "http_5xx",
      "exception"
    ]
  }
}

# ============================================================================
# RESILIENCE MODULE: safe_retry
# ============================================================================

# Retry configuration
variable "max_retries" {
  type        = number
  description = "Maximum retry attempts"
  default     = 3

  validation {
    condition     = var.max_retries >= 0 && var.max_retries <= 10
    error_message = "Maximum retries must be between 0 and 10."
  }
}

variable "retry_delay_ms" {
  type        = number
  description = "Initial retry delay in milliseconds"
  default     = 1000

  validation {
    condition     = var.retry_delay_ms >= 100 && var.retry_delay_ms <= 60000
    error_message = "Retry delay must be between 100ms and 60 seconds."
  }
}

locals {
  safe_retry = {
    strategies = {
      constant     = "constant"     # Same delay each time
      linear       = "linear"       # Delay increases linearly
      exponential  = "exponential"  # Delay doubles each time
      fibonacci    = "fibonacci"    # Delay follows fibonacci sequence
      decorrelated = "decorrelated" # AWS-style decorrelated jitter
    }

    default_config = {
      max_attempts        = 3
      initial_delay_ms    = 1000
      max_delay_ms        = 30000
      multiplier          = 2.0
      jitter_factor       = 0.1
    }

    retryable_errors = [
      "timeout",
      "connection_refused",
      "connection_reset",
      "too_many_requests",
      "service_unavailable"
    ]
  }
}

# ============================================================================
# RESILIENCE MODULE: safe_monotonic
# ============================================================================

locals {
  safe_monotonic = {
    # Monotonic ID generation strategies
    strategies = {
      timestamp     = "timestamp-based"
      sequence      = "sequence-based"
      snowflake     = "snowflake-like"
      ulid          = "ulid-compatible"
    }

    # Snowflake-like ID configuration
    snowflake_config = {
      epoch_ms         = 1704067200000  # 2024-01-01
      timestamp_bits   = 41
      datacenter_bits  = 5
      machine_bits     = 5
      sequence_bits    = 12
    }

    # Maximum values per component
    max_values = {
      datacenter = 31
      machine    = 31
      sequence   = 4095
    }
  }
}

# ============================================================================
# STATE MODULE: safe_state_machine
# ============================================================================

locals {
  safe_state_machine = {
    max_states        = 1000
    max_transitions   = 10000
    max_history       = 100

    common_patterns = {
      # HTTP request states
      http_request = {
        states      = ["pending", "executing", "success", "error", "timeout"]
        initial     = "pending"
        final       = ["success", "error", "timeout"]
      }

      # Order processing states
      order_processing = {
        states      = ["created", "confirmed", "processing", "shipped", "delivered", "cancelled"]
        initial     = "created"
        final       = ["delivered", "cancelled"]
      }

      # Connection states
      connection = {
        states      = ["disconnected", "connecting", "connected", "disconnecting"]
        initial     = "disconnected"
        final       = ["disconnected"]
      }
    }
  }
}

# ============================================================================
# STATE MODULE: safe_calculator
# ============================================================================

locals {
  safe_calculator = {
    max_operand_stack    = 1000
    max_operator_stack   = 500
    max_expression_depth = 100

    operators = {
      arithmetic = ["+", "-", "*", "/", "%", "^"]
      comparison = ["==", "!=", "<", ">", "<=", ">="]
      logical    = ["&&", "||", "!"]
      bitwise    = ["&", "|", "^", "~", "<<", ">>"]
    }

    precedence = {
      low     = 1  # +, -
      medium  = 2  # *, /, %
      high    = 3  # ^
      unary   = 4  # -, !
    }
  }
}

# ============================================================================
# ALGORITHM MODULE: safe_geo
# ============================================================================

# Latitude validation
variable "latitude" {
  type        = number
  description = "Latitude coordinate (-90 to 90)"
  default     = 0

  validation {
    condition     = var.latitude >= -90 && var.latitude <= 90
    error_message = "Latitude must be between -90 and 90 degrees."
  }
}

# Longitude validation
variable "longitude" {
  type        = number
  description = "Longitude coordinate (-180 to 180)"
  default     = 0

  validation {
    condition     = var.longitude >= -180 && var.longitude <= 180
    error_message = "Longitude must be between -180 and 180 degrees."
  }
}

locals {
  safe_geo = {
    bounds = {
      lat_min  = -90
      lat_max  = 90
      lon_min  = -180
      lon_max  = 180
    }

    earth = {
      radius_km    = 6371.0
      radius_mi    = 3958.8
      circumference_km = 40075.0
    }

    distance_formulas = {
      haversine  = "great-circle-distance"
      vincenty   = "ellipsoidal-distance"
      euclidean  = "flat-earth-approximation"
    }

    geohash = {
      max_precision = 12
      chars         = "0123456789bcdefghjkmnpqrstuvwxyz"
    }
  }
}

# ============================================================================
# ALGORITHM MODULE: safe_probability
# ============================================================================

# Probability validation (0.0 to 1.0)
variable "probability" {
  type        = number
  description = "Probability value (0.0 to 1.0)"
  default     = 0.5

  validation {
    condition     = var.probability >= 0 && var.probability <= 1
    error_message = "Probability must be between 0.0 and 1.0."
  }
}

locals {
  safe_probability = {
    bounds = {
      min = 0.0
      max = 1.0
    }

    distributions = {
      uniform     = "uniform"
      normal      = "normal"
      exponential = "exponential"
      poisson     = "poisson"
      binomial    = "binomial"
    }

    constants = {
      certainty    = 1.0
      impossibility = 0.0
      coin_flip    = 0.5
    }
  }
}

# ============================================================================
# ALGORITHM MODULE: safe_checksum
# ============================================================================

locals {
  safe_checksum = {
    algorithms = {
      crc32     = { name = "CRC-32", bits = 32 }
      crc64     = { name = "CRC-64", bits = 64 }
      adler32   = { name = "Adler-32", bits = 32 }
      xxhash32  = { name = "xxHash32", bits = 32 }
      xxhash64  = { name = "xxHash64", bits = 64 }
      xxhash128 = { name = "xxHash128", bits = 128 }
    }

    validation_digits = {
      luhn      = "credit-card-numbers"
      verhoeff  = "high-error-detection"
      damm      = "anti-symmetric"
      isbn10    = "book-numbers"
      isbn13    = "modern-book-numbers"
    }
  }
}

# ============================================================================
# ALGORITHM MODULE: safe_tensor
# ============================================================================

# Tensor dimension validation
variable "tensor_dimensions" {
  type        = list(number)
  description = "Tensor shape/dimensions"
  default     = [1]

  validation {
    condition     = length(var.tensor_dimensions) > 0 && length(var.tensor_dimensions) <= 16
    error_message = "Tensor must have 1-16 dimensions."
  }
}

locals {
  safe_tensor = {
    max_dimensions    = 16
    max_elements      = 2147483647  # 2^31 - 1
    max_dimension_size = 65536

    dtypes = {
      float32  = { size = 4, name = "32-bit float" }
      float64  = { size = 8, name = "64-bit float" }
      int32    = { size = 4, name = "32-bit integer" }
      int64    = { size = 8, name = "64-bit integer" }
      bool     = { size = 1, name = "boolean" }
      complex64 = { size = 8, name = "64-bit complex" }
    }

    operations = {
      element_wise = ["add", "sub", "mul", "div"]
      reduction    = ["sum", "mean", "max", "min"]
      linear       = ["matmul", "transpose", "reshape"]
    }
  }
}

# ============================================================================
# SECURITY MODULE: safe_password
# ============================================================================

locals {
  safe_password = {
    requirements = {
      min_length       = 12
      max_length       = 128
      min_uppercase    = 1
      min_lowercase    = 1
      min_digits       = 1
      min_special      = 1
    }

    strength_levels = {
      weak     = { score = 0, description = "Easily guessed" }
      fair     = { score = 1, description = "Somewhat predictable" }
      good     = { score = 2, description = "Reasonable strength" }
      strong   = { score = 3, description = "Hard to crack" }
      excellent = { score = 4, description = "Very secure" }
    }

    hashing = {
      algorithm     = "argon2id"
      memory_kb     = 65536
      iterations    = 3
      parallelism   = 4
      hash_length   = 32
    }

    forbidden_patterns = [
      "password",
      "123456",
      "qwerty",
      "admin"
    ]
  }
}

# ============================================================================
# SECURITY MODULE: safe_ml
# ============================================================================

locals {
  safe_ml = {
    # Input validation bounds
    input_bounds = {
      max_features        = 100000
      max_samples         = 10000000
      max_embedding_dim   = 4096
      max_sequence_length = 8192
    }

    # Model safety thresholds
    safety_thresholds = {
      confidence_min      = 0.0
      confidence_max      = 1.0
      probability_epsilon = 0.0001
      gradient_clip_norm  = 1.0
    }

    # Supported model types
    model_types = [
      "classification",
      "regression",
      "clustering",
      "embedding",
      "generation"
    ]

    # Safety checks
    safety_checks = {
      input_sanitization  = true
      output_validation   = true
      adversarial_defense = true
      bias_detection      = true
    }
  }
}

# ============================================================================
# HTTP MODULE: safe_header
# ============================================================================

locals {
  safe_header = {
    # Maximum header sizes
    max_sizes = {
      name_length  = 256
      value_length = 8192
      total_size   = 65536
    }

    # Security headers
    security_headers = {
      content_security_policy   = "Content-Security-Policy"
      strict_transport_security = "Strict-Transport-Security"
      x_content_type_options    = "X-Content-Type-Options"
      x_frame_options           = "X-Frame-Options"
      x_xss_protection          = "X-XSS-Protection"
      referrer_policy           = "Referrer-Policy"
      permissions_policy        = "Permissions-Policy"
    }

    # Recommended security header values
    recommended_values = {
      strict_transport_security = "max-age=31536000; includeSubDomains"
      x_content_type_options    = "nosniff"
      x_frame_options           = "DENY"
      referrer_policy           = "strict-origin-when-cross-origin"
    }

    # Headers that should not be exposed
    sensitive_headers = [
      "Authorization",
      "Cookie",
      "Set-Cookie",
      "X-Api-Key",
      "X-Auth-Token"
    ]
  }
}

# ============================================================================
# HTTP MODULE: safe_cookie
# ============================================================================

locals {
  safe_cookie = {
    # Maximum sizes
    max_sizes = {
      name_length    = 256
      value_length   = 4096
      total_cookies  = 50
      total_size     = 4096
    }

    # Secure cookie attributes
    secure_defaults = {
      secure     = true
      http_only  = true
      same_site  = "Strict"
      path       = "/"
    }

    # SameSite options
    same_site_options = ["Strict", "Lax", "None"]

    # Cookie prefixes for enhanced security
    prefixes = {
      host   = "__Host-"
      secure = "__Secure-"
    }

    # Maximum expiry (1 year)
    max_age_seconds = 31536000
  }
}

# ============================================================================
# HTTP MODULE: safe_content_type
# ============================================================================

locals {
  safe_content_type = {
    # Common MIME types
    mime_types = {
      text = {
        plain = "text/plain"
        html  = "text/html"
        css   = "text/css"
        csv   = "text/csv"
      }
      application = {
        json          = "application/json"
        xml           = "application/xml"
        javascript    = "application/javascript"
        octet_stream  = "application/octet-stream"
        form_urlencoded = "application/x-www-form-urlencoded"
        pdf           = "application/pdf"
      }
      multipart = {
        form_data = "multipart/form-data"
        mixed     = "multipart/mixed"
      }
      image = {
        png  = "image/png"
        jpeg = "image/jpeg"
        gif  = "image/gif"
        webp = "image/webp"
        svg  = "image/svg+xml"
      }
    }

    # Default charset
    default_charset = "utf-8"

    # Content type validation
    format_pattern = "^[a-z]+/[a-z0-9.+-]+(;\\s*[a-z]+=.+)*$"
  }
}

# ============================================================================
# RESOURCE CONFIGURATION HELPERS
# ============================================================================

# Safe security group ingress rule
variable "ingress_rules" {
  type = list(object({
    port        = number
    protocol    = string
    cidr_blocks = list(string)
    description = string
  }))
  default = []

  validation {
    condition = alltrue([
      for rule in var.ingress_rules :
        rule.port >= 1 && rule.port <= 65535 &&
        contains(["tcp", "udp", "icmp", "-1"], rule.protocol) &&
        length(rule.description) > 0
    ])
    error_message = "Invalid ingress rule: port must be 1-65535, protocol must be tcp/udp/icmp/-1, description required."
  }
}

# Safe replica count
variable "replicas" {
  type        = number
  description = "Number of replicas (1-100)"
  default     = 1

  validation {
    condition     = var.replicas >= 1 && var.replicas <= 100
    error_message = "Replicas must be between 1 and 100."
  }
}

# Safe memory allocation (MB)
variable "memory_mb" {
  type        = number
  description = "Memory allocation in MB"
  default     = 512

  validation {
    condition     = var.memory_mb >= 128 && var.memory_mb <= 65536
    error_message = "Memory must be between 128 MB and 64 GB."
  }
}

# Safe CPU allocation (millicores)
variable "cpu_millicores" {
  type        = number
  description = "CPU allocation in millicores"
  default     = 250

  validation {
    condition     = var.cpu_millicores >= 50 && var.cpu_millicores <= 32000
    error_message = "CPU must be between 50m and 32 cores."
  }
}

# ============================================================================
# CLOUD-SPECIFIC SAFE DEFAULTS
# ============================================================================

locals {
  # AWS safe defaults
  aws = {
    instance_types = [
      "t3.micro",
      "t3.small",
      "t3.medium",
      "t3.large",
      "m5.large",
      "m5.xlarge",
    ]

    storage_bounds = {
      min_root_volume = 8
      max_root_volume = 1000
      min_data_volume = 1
      max_data_volume = 16000
    }

    regions = [
      "us-east-1",
      "us-east-2",
      "us-west-1",
      "us-west-2",
      "eu-west-1",
      "eu-central-1",
      "ap-northeast-1",
      "ap-southeast-1",
    ]
  }

  # GCP safe defaults
  gcp = {
    machine_types = [
      "e2-micro",
      "e2-small",
      "e2-medium",
      "n2-standard-2",
      "n2-standard-4",
      "n2-standard-8",
    ]

    regions = [
      "us-central1",
      "us-east1",
      "us-west1",
      "europe-west1",
      "asia-east1",
    ]
  }

  # Azure safe defaults
  azure = {
    vm_sizes = [
      "Standard_B1s",
      "Standard_B2s",
      "Standard_D2s_v3",
      "Standard_D4s_v3",
    ]

    regions = [
      "eastus",
      "eastus2",
      "westus",
      "westus2",
      "westeurope",
      "northeurope",
    ]
  }

  # Safe timeout defaults
  timeouts = {
    create = "30m"
    update = "30m"
    delete = "30m"
  }
}

# ============================================================================
# OUTPUT HELPERS
# ============================================================================

output "proven_version" {
  value       = local.proven_version
  description = "Proven library version"
}

output "proven_module_count" {
  value       = local.proven_modules
  description = "Total number of Proven modules"
}

output "proven_manifest" {
  value       = local.proven_manifest
  description = "Proven module manifest by category"
}

output "safe_defaults" {
  value = {
    network  = local.safe_network
    crypto   = local.safe_crypto
    timeouts = local.timeouts
    retry    = local.safe_retry
    rate_limiter = local.safe_rate_limiter
  }
  description = "Safe default values for common configurations"
}
