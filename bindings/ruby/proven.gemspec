# SPDX-License-Identifier: PMPL-1.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
# frozen_string_literal: true

Gem::Specification.new do |spec|
  spec.name = "proven"
  spec.version = "0.3.0"
  spec.authors = ["Hyperpolymath"]
  spec.email = ["info@hyperpolymath.com"]

  spec.summary = "Safety-first utility functions with formal verification guarantees"
  spec.description = <<~DESC
    Proven provides safe operations for math, strings, paths, email,
    network addresses, and cryptographic comparisons. All functions
    are designed to never crash and return nil on invalid input.
  DESC
  spec.homepage = "https://github.com/hyperpolymath/proven"
  spec.license = "PMPL-1.0"
  spec.required_ruby_version = ">= 3.0.0"

  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata["source_code_uri"] = spec.homepage
  spec.metadata["rubygems_mfa_required"] = "true"

  spec.files = Dir.glob("lib/**/*.rb") + ["proven.gemspec"]
  spec.require_paths = ["lib"]

  spec.add_development_dependency "rspec", "~> 3.0"
end
