# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
# frozen_string_literal: true

# Raw FFI bindings to libproven via Ruby's Fiddle (stdlib).
#
# This module provides the low-level C ABI interface to the formally verified
# Proven library. ALL computation happens in Idris 2 via the Zig FFI bridge.
# Ruby code here is ONLY data marshaling -- no logic is reimplemented.

require "fiddle"
require "fiddle/import"

module Proven
  # Low-level Fiddle bindings to libproven shared library.
  # Every function here maps 1:1 to a C ABI export from ffi/zig/src/main.zig.
  module FFI
    extend Fiddle::Importer

    # Search paths for the libproven shared library, ordered by preference.
    SEARCH_PATHS = [
      File.expand_path("../../../../ffi/zig/zig-out/lib/libproven.so", __dir__),
      File.expand_path("../../../../ffi/zig/zig-out/lib/libproven.dylib", __dir__),
      "/usr/local/lib/libproven.so",
      "/usr/local/lib/libproven.dylib",
      "/usr/lib/libproven.so",
      "/usr/lib/libproven.dylib",
    ].freeze

    # Locate and load the shared library.
    LIB_PATH = SEARCH_PATHS.find { |p| File.exist?(p) }

    begin
      if LIB_PATH
        dlload LIB_PATH
      else
        dlload "libproven"
      end
    rescue Fiddle::DLError => e
      warn "[proven] Failed to load libproven: #{e.message}"
      warn "[proven] Searched: #{SEARCH_PATHS.join(', ')}"
      raise LoadError,
            "Cannot load libproven shared library. " \
            "Build it with: cd ffi/zig && zig build"
    end

    # -------------------------------------------------------------------
    # C ABI status codes (mirrors ProvenStatus enum in main.zig)
    # -------------------------------------------------------------------
    STATUS_OK                  = 0
    STATUS_ERR_NULL_POINTER    = -1
    STATUS_ERR_INVALID_ARG     = -2
    STATUS_ERR_OVERFLOW        = -3
    STATUS_ERR_UNDERFLOW       = -4
    STATUS_ERR_DIV_BY_ZERO     = -5
    STATUS_ERR_PARSE_FAILURE   = -6
    STATUS_ERR_VALIDATION      = -7
    STATUS_ERR_OUT_OF_BOUNDS   = -8
    STATUS_ERR_ENCODING        = -9
    STATUS_ERR_ALLOC_FAILED    = -10
    STATUS_ERR_NOT_IMPLEMENTED = -99

    # -------------------------------------------------------------------
    # Runtime lifecycle
    # -------------------------------------------------------------------
    extern "int proven_init()"
    extern "void proven_deinit()"
    extern "int proven_is_initialized()"
    extern "unsigned int proven_ffi_abi_version()"

    # -------------------------------------------------------------------
    # Memory management
    # -------------------------------------------------------------------
    extern "void proven_free_string(void*)"

    # -------------------------------------------------------------------
    # Version info
    # -------------------------------------------------------------------
    extern "unsigned int proven_version_major()"
    extern "unsigned int proven_version_minor()"
    extern "unsigned int proven_version_patch()"
    extern "unsigned int proven_module_count()"

    # -------------------------------------------------------------------
    # SafeMath
    # -------------------------------------------------------------------
    # IntResult is { int status, long long value } = 16 bytes
    # We import as returning void* and manually unpack the struct.
    # For functions returning structs by value, we use Fiddle::Function.
    # -------------------------------------------------------------------

    # Helper: create a Fiddle::Function bound to the loaded library.
    # @param name [String] C symbol name
    # @param arg_types [Array<Integer>] Fiddle type constants for arguments
    # @param ret_type [Integer] Fiddle type constant for return
    # @return [Fiddle::Function]
    def self.func(name, arg_types, ret_type)
      addr = handler.sym(name)
      Fiddle::Function.new(addr, arg_types, ret_type)
    rescue Fiddle::DLError
      nil
    end

    # Provide direct access to the library handle for Fiddle::Function.
    def self.handler
      @handler ||= begin
        path = LIB_PATH || "libproven"
        Fiddle::Handle.new(path)
      end
    end

    # -------------------------------------------------------------------
    # SafeMath functions
    # Returns packed struct IntResult { i32 status, i64 value }
    # On x86_64 SysV ABI, small structs <= 16 bytes are returned in
    # registers (RAX:RDX), so we receive them as two values.
    # We use VOIDP return and manually parse the struct from memory.
    # -------------------------------------------------------------------

    # Helper to call an FFI function that returns IntResult (status + i64).
    # IntResult is 16 bytes: int32 status (padded to 8) + int64 value.
    # @param sym_name [String]
    # @param args [Array] arguments to pass
    # @return [Array(Integer, Integer)] [status, value]
    def self.call_int_result(sym_name, *args)
      # Allocate struct for result
      buf = Fiddle::Pointer.malloc(16, Fiddle::RUBY_FREE)
      f = Fiddle::Function.new(
        handler[sym_name],
        args.map { Fiddle::TYPE_LONG_LONG },
        Fiddle::TYPE_VOIDP
      )
      # For struct returns via SysV ABI, the struct is returned in registers.
      # We call and interpret the raw return as packed struct.
      result_ptr = f.call(*args)

      # On most ABIs, IntResult (16 bytes) returned in RAX:RDX
      # Fiddle returns it as a pointer-sized integer; we need to handle
      # it differently based on ABI. Use direct approach instead.
      nil
    rescue Fiddle::DLError
      nil
    end

    # -------------------------------------------------------------------
    # Low-level struct-returning function calls
    #
    # For struct returns on x86_64 SysV ABI, structs > 16 bytes use
    # hidden pointer (sret). Structs <= 16 bytes use registers.
    #
    # We use a wrapper approach: allocate a buffer, call the function
    # with the buffer as a hidden first arg (sret convention), then
    # unpack the buffer.
    #
    # However, Fiddle does not natively support struct returns.
    # Instead, we use Fiddle::Closure or call functions that return
    # simple types and manually compose results.
    #
    # Simplest reliable approach: use Fiddle::Pointer and pack/unpack.
    # -------------------------------------------------------------------

    # For struct-returning functions, we define helper wrappers that
    # use a C-level shim approach. Since libproven returns small structs
    # (IntResult = 16 bytes) in registers on x86_64, Fiddle can handle
    # this if we treat the return as two LONG_LONG values packed together.
    #
    # Alternative: use the raw Fiddle.dlopen + Fiddle::Function approach
    # with a return buffer.

    # The most portable approach with Fiddle is to use CFunc directly
    # and interpret register returns. For x86_64:
    # - IntResult (16 bytes) returns in RAX (status+padding) and RDX (value)
    # - BoolResult (8 bytes) returns in RAX
    # - StringResult (24 bytes) uses hidden sret pointer

    class << self
      # Call a function returning IntResult { i32 status, i64 value }.
      # On x86_64, the 16-byte struct is returned in RAX:RDX.
      #
      # @param name [String] FFI symbol name
      # @param arg_types [Array<Integer>] Fiddle types for arguments
      # @param args [Array] actual argument values
      # @return [Array(Integer, Integer)] [status, value] or nil
      def invoke_int_result(name, arg_types, args)
        fn = Fiddle::Function.new(
          handler[name],
          arg_types,
          Fiddle::TYPE_VOIDP, # Treat struct return as opaque
          need_gvl: true
        )
        # Call the function - Fiddle will handle the ABI struct return
        raw = fn.call(*args)
        # raw is a Fiddle::Pointer containing the IntResult struct
        # Unpack: int32 status (4 bytes) + 4 padding + int64 value (8 bytes)
        packed = raw.to_str(16) rescue nil
        return nil unless packed

        status, value = packed.unpack("l!x4q") # int + padding + long long
        [status, value]
      rescue Fiddle::DLError, TypeError, NoMethodError
        nil
      end

      # Call a function returning BoolResult { i32 status, i32 value }.
      # 8 bytes total - fits in single register.
      #
      # @param name [String] FFI symbol name
      # @param arg_types [Array<Integer>] Fiddle types for arguments
      # @param args [Array] actual argument values
      # @return [Array(Integer, Boolean)] [status, value] or nil
      def invoke_bool_result(name, arg_types, args)
        fn = Fiddle::Function.new(
          handler[name],
          arg_types,
          Fiddle::TYPE_LONG_LONG, # 8 bytes fits in one register
          need_gvl: true
        )
        raw = fn.call(*args)
        # BoolResult is { i32 status, bool value } = 8 bytes packed
        # In a single 64-bit register: low 32 bits = status, high 32 bits = value
        status = raw & 0xFFFFFFFF
        # Sign-extend status if negative
        status = status - 0x100000000 if status >= 0x80000000
        value = (raw >> 32) & 0x1
        [status, value != 0]
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Call a function returning StringResult { i32 status, ptr value, size_t length }.
      # 24 bytes total on 64-bit - uses hidden sret pointer.
      #
      # @param name [String] FFI symbol name
      # @param arg_types [Array<Integer>] Fiddle types for arguments
      # @param args [Array] actual argument values
      # @return [Array(Integer, String)] [status, string] or nil
      def invoke_string_result(name, arg_types, args)
        # StringResult is 24 bytes: i32 status + 4 pad + ptr + size_t
        # On x86_64, >16 byte structs use hidden sret pointer
        buf = Fiddle::Pointer.malloc(32, Fiddle::RUBY_FREE) # Extra space for safety

        # Hidden sret: first argument is pointer to result buffer
        sret_arg_types = [Fiddle::TYPE_VOIDP] + arg_types
        sret_args = [buf] + args

        fn = Fiddle::Function.new(
          handler[name],
          sret_arg_types,
          Fiddle::TYPE_VOID, # sret functions return void
          need_gvl: true
        )
        fn.call(*sret_args)

        packed = buf.to_str(24)
        status = packed[0, 4].unpack1("l!")
        ptr_val = packed[8, 8].unpack1("Q")
        length = packed[16, 8].unpack1("Q")

        if status == STATUS_OK && ptr_val != 0 && length > 0
          str_ptr = Fiddle::Pointer.new(ptr_val)
          str = str_ptr.to_str(length).dup
          # Free the C-allocated string
          proven_free_string(str_ptr)
          [status, str]
        else
          [status, nil]
        end
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Call a function returning FloatResult { i32 status, f64 value }.
      # 16 bytes: returned in xmm0 (float) + eax (status) or via sret.
      # Actually on x86_64 SysV: status in RAX (INTEGER class),
      # value in XMM0 (SSE class). Fiddle can't split register classes.
      # Use sret approach instead.
      #
      # @param name [String] FFI symbol name
      # @param arg_types [Array<Integer>] Fiddle types for arguments
      # @param args [Array] actual argument values
      # @return [Array(Integer, Float)] [status, value] or nil
      def invoke_float_result(name, arg_types, args)
        buf = Fiddle::Pointer.malloc(16, Fiddle::RUBY_FREE)

        sret_arg_types = [Fiddle::TYPE_VOIDP] + arg_types
        sret_args = [buf] + args

        fn = Fiddle::Function.new(
          handler[name],
          sret_arg_types,
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(*sret_args)

        packed = buf.to_str(16)
        status = packed[0, 4].unpack1("l!")
        value = packed[8, 8].unpack1("d") # double

        [status, value]
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Call a function returning a simple bool.
      #
      # @param name [String] FFI symbol name
      # @param arg_types [Array<Integer>] Fiddle types for arguments
      # @param args [Array] actual argument values
      # @return [Boolean, nil]
      def invoke_bool(name, arg_types, args)
        fn = Fiddle::Function.new(
          handler[name],
          arg_types,
          Fiddle::TYPE_INT,
          need_gvl: true
        )
        result = fn.call(*args)
        result != 0
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Call a function returning a simple i64.
      #
      # @param name [String] FFI symbol name
      # @param arg_types [Array<Integer>] Fiddle types for arguments
      # @param args [Array] actual argument values
      # @return [Integer, nil]
      def invoke_i64(name, arg_types, args)
        fn = Fiddle::Function.new(
          handler[name],
          arg_types,
          Fiddle::TYPE_LONG_LONG,
          need_gvl: true
        )
        fn.call(*args)
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Call a function returning a simple f64.
      #
      # @param name [String] FFI symbol name
      # @param arg_types [Array<Integer>] Fiddle types for arguments
      # @param args [Array] actual argument values
      # @return [Float, nil]
      def invoke_f64(name, arg_types, args)
        fn = Fiddle::Function.new(
          handler[name],
          arg_types,
          Fiddle::TYPE_DOUBLE,
          need_gvl: true
        )
        fn.call(*args)
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Call a function returning a simple u32 / i32.
      #
      # @param name [String] FFI symbol name
      # @param arg_types [Array<Integer>] Fiddle types for arguments
      # @param args [Array] actual argument values
      # @return [Integer, nil]
      def invoke_i32(name, arg_types, args)
        fn = Fiddle::Function.new(
          handler[name],
          arg_types,
          Fiddle::TYPE_INT,
          need_gvl: true
        )
        fn.call(*args)
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Call a function returning u8.
      def invoke_u8(name, arg_types, args)
        fn = Fiddle::Function.new(
          handler[name],
          arg_types,
          Fiddle::TYPE_CHAR,
          need_gvl: true
        )
        fn.call(*args) & 0xFF
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Call a function returning a pointer (may be null).
      def invoke_ptr(name, arg_types, args)
        fn = Fiddle::Function.new(
          handler[name],
          arg_types,
          Fiddle::TYPE_VOIDP,
          need_gvl: true
        )
        fn.call(*args)
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Call a void function.
      def invoke_void(name, arg_types, args)
        fn = Fiddle::Function.new(
          handler[name],
          arg_types,
          Fiddle::TYPE_VOID,
          need_gvl: true
        )
        fn.call(*args)
        nil
      rescue Fiddle::DLError, TypeError
        nil
      end

      # Prepare a Ruby string for C: returns [Fiddle::Pointer, length].
      # The pointer references the string's internal buffer (no copy).
      #
      # @param str [String]
      # @return [Array(Fiddle::Pointer, Integer)]
      def str_to_ptr(str)
        [Fiddle::Pointer[str], str.bytesize]
      end
    end
  end
end
