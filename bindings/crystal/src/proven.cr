# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven -- FFI bindings to libproven (formally verified safety primitives).
#
# All computation is performed in the Idris2/Zig core via the C ABI exposed by
# libproven.  This Crystal binding is a thin wrapper that marshals data across
# the FFI boundary and returns idiomatic Crystal types (nilable values for
# errors, never exceptions).
#
# Link with: -lproven

require "./proven/lib_proven"
require "./proven/safe_math"
require "./proven/safe_string"
require "./proven/safe_path"
require "./proven/safe_email"
require "./proven/safe_url"
require "./proven/safe_network"
require "./proven/safe_crypto"
require "./proven/safe_uuid"
require "./proven/safe_currency"
require "./proven/safe_phone"
require "./proven/safe_hex"
require "./proven/safe_json"
require "./proven/safe_datetime"
require "./proven/safe_float"
require "./proven/safe_version"
require "./proven/safe_color"
require "./proven/safe_angle"
require "./proven/safe_unit"

module Proven
  VERSION      = "0.5.0"
  MODULE_COUNT = 18 # modules with C ABI coverage

  # Initialize the libproven runtime.  Call once at program start.
  def self.init : Bool
    LibProven.proven_init == 0
  end

  # Shut down the libproven runtime.  Call once at program exit.
  def self.deinit : Nil
    LibProven.proven_deinit
  end

  # Check whether the runtime has been initialized.
  def self.initialized? : Bool
    LibProven.proven_is_initialized
  end

  # Return the FFI ABI version for compatibility checking.
  def self.abi_version : UInt32
    LibProven.proven_ffi_abi_version
  end

  # Return the library version triple.
  def self.version_triple : {UInt32, UInt32, UInt32}
    {LibProven.proven_version_major,
     LibProven.proven_version_minor,
     LibProven.proven_version_patch}
  end
end
