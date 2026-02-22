# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

Gem::Specification.new do |spec|
  spec.name = "proven"
  spec.version = "1.0.0"
  spec.authors = ["Jonathan D.A. Jewell"]
  spec.email = ["jonathan.jewell@open.ac.uk"]

  spec.summary = "FFI bindings to libproven - formally verified safety functions (Idris 2)"
  spec.description = <<~DESC
    Proven provides safe operations for math, strings, paths, email, network
    addresses, cryptographic comparisons, UUID, currency, phone numbers, hex
    encoding, colors, angles, unit conversions, datetime, JSON, and URLs.

    This gem is a thin FFI wrapper using Ruby's Fiddle (stdlib). ALL computation
    happens in Idris 2 via the Zig FFI bridge (libproven.so). No safety logic
    is reimplemented in Ruby.

    Requires libproven shared library (build from ffi/zig/ with: zig build).
  DESC
  spec.homepage = "https://github.com/hyperpolymath/proven"
  spec.license = "PMPL-1.0"
  spec.required_ruby_version = ">= 3.0.0"

  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata["source_code_uri"] = spec.homepage
  spec.metadata["rubygems_mfa_required"] = "true"

  spec.files = Dir.glob("lib/**/*.rb") + ["proven.gemspec"]
  spec.require_paths = ["lib"]

  # No external gem dependencies - uses Ruby stdlib Fiddle for FFI.
  # libproven.so must be available at runtime (system lib or local build).
end
