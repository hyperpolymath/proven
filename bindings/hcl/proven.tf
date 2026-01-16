# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath

# Proven Safety Primitives for HCL/Terraform
#
# This module provides validated types and safe defaults
# for Terraform configurations.

terraform {
  required_version = ">= 1.0"
}

# ============================================================================
# VALIDATION FUNCTIONS
# ============================================================================

# Validate port number
variable "port" {
  type        = number
  description = "Safe port number (1-65535)"

  validation {
    condition     = var.port >= 1 && var.port <= 65535
    error_message = "Port must be between 1 and 65535."
  }
}

# Validate non-empty string
variable "non_empty_string" {
  type        = string
  description = "Non-empty string"

  validation {
    condition     = length(var.non_empty_string) > 0
    error_message = "String cannot be empty."
  }
}

# Validate email format
variable "email" {
  type        = string
  description = "Valid email address"

  validation {
    condition     = can(regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", var.email))
    error_message = "Invalid email format."
  }
}

# Validate CIDR block
variable "cidr_block" {
  type        = string
  description = "Valid CIDR notation"

  validation {
    condition     = can(cidrhost(var.cidr_block, 0))
    error_message = "Invalid CIDR block format."
  }
}

# ============================================================================
# LOCAL SAFE DEFAULTS
# ============================================================================

locals {
  # Safe port defaults
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

  # Safe instance sizes (AWS)
  safe_instance_types = [
    "t3.micro",
    "t3.small",
    "t3.medium",
    "t3.large",
    "m5.large",
    "m5.xlarge",
  ]

  # Safe storage sizes (GB)
  storage_bounds = {
    min_root_volume = 8
    max_root_volume = 1000
    min_data_volume = 1
    max_data_volume = 16000
  }

  # Safe timeout defaults
  timeouts = {
    create = "30m"
    update = "30m"
    delete = "30m"
  }

  # Safe retry defaults
  retry_policy = {
    max_attempts = 3
    initial_delay_seconds = 1
    max_delay_seconds = 30
  }
}

# ============================================================================
# SAFE RESOURCE MODULES
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
# OUTPUT HELPERS
# ============================================================================

output "proven_version" {
  value       = "0.9.0"
  description = "Proven library version"
}

output "safe_defaults" {
  value = {
    ports    = local.common_ports
    timeouts = local.timeouts
    retry    = local.retry_policy
  }
  description = "Safe default values"
}
