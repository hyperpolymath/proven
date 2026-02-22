# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Raw DLL/shared library FFI declarations for libproven.
# These map 1:1 to the C ABI exposed by libproven (Idris2 + Zig).
# All computation is performed in the verified Idris 2 core;
# this file is a thin declaration layer. Do NOT reimplement any logic.
#
# Ring language bindings using LoadLib() / CallDLL().

# ---------------------------------------------------------------------------
# Status codes
# ---------------------------------------------------------------------------
PROVEN_OK                   = 0
PROVEN_ERR_NULL_POINTER     = -1
PROVEN_ERR_INVALID_ARGUMENT = -2
PROVEN_ERR_OVERFLOW         = -3
PROVEN_ERR_UNDERFLOW        = -4
PROVEN_ERR_DIVISION_BY_ZERO = -5
PROVEN_ERR_PARSE_FAILURE    = -6
PROVEN_ERR_VALIDATION_FAILED = -7
PROVEN_ERR_OUT_OF_BOUNDS    = -8
PROVEN_ERR_ENCODING_ERROR   = -9
PROVEN_ERR_ALLOCATION_FAILED = -10
PROVEN_ERR_NOT_IMPLEMENTED  = -99

# ---------------------------------------------------------------------------
# Library handle (loaded once)
# ---------------------------------------------------------------------------
_proven_lib = NULL

# Load the libproven shared library. Returns true on success.
func proven_load_library
    if _proven_lib != NULL
        return true
    ok
    if isWindows()
        _proven_lib = loadlib("proven.dll")
    but isLinux()
        _proven_lib = loadlib("libproven.so")
    but isMacOS()
        _proven_lib = loadlib("libproven.dylib")
    ok
    return _proven_lib != NULL

# Unload the libproven shared library.
func proven_unload_library
    if _proven_lib != NULL
        freelib(_proven_lib)
        _proven_lib = NULL
    ok

# Get the library handle (loads if needed).
func proven_lib
    if _proven_lib = NULL
        proven_load_library()
    ok
    return _proven_lib

# ---------------------------------------------------------------------------
# Lifecycle
# ---------------------------------------------------------------------------

# Initialize the Proven runtime. Must be called before any other function.
# Returns PROVEN_OK (0) on success.
func proven_init
    lib = proven_lib()
    if lib = NULL return PROVEN_ERR_ALLOCATION_FAILED ok
    return calldll(lib, "proven_init", "")

# Cleanup the Proven runtime.
func proven_deinit
    lib = proven_lib()
    if lib = NULL return ok
    calldll(lib, "proven_deinit", "")

# Check if the runtime is initialized. Returns 1 (true) or 0 (false).
func proven_is_initialized
    lib = proven_lib()
    if lib = NULL return 0 ok
    return calldll(lib, "proven_is_initialized", "")

# ---------------------------------------------------------------------------
# Version information
# ---------------------------------------------------------------------------

# Get FFI ABI version.
func proven_ffi_abi_version
    return calldll(proven_lib(), "proven_ffi_abi_version", "")

# Get major version number.
func proven_version_major
    return calldll(proven_lib(), "proven_version_major", "")

# Get minor version number.
func proven_version_minor
    return calldll(proven_lib(), "proven_version_minor", "")

# Get patch version number.
func proven_version_patch
    return calldll(proven_lib(), "proven_version_patch", "")

# Get total module count.
func proven_module_count
    return calldll(proven_lib(), "proven_module_count", "")

# ---------------------------------------------------------------------------
# Memory management
# ---------------------------------------------------------------------------

# Free a string allocated by Proven functions.
func proven_free_string ptr
    if ptr = NULL return ok
    calldll(proven_lib(), "proven_free_string", "p", ptr)

# ---------------------------------------------------------------------------
# Helper: call a function that returns IntResult {status, value}
# Returns a list [status, value] or NULL on library failure.
# ---------------------------------------------------------------------------
func _call_int_result funcName, params, paramTypes
    lib = proven_lib()
    if lib = NULL return NULL ok
    return calldll(lib, funcName, paramTypes, params)

# ---------------------------------------------------------------------------
# Helper: check result status and return value or NULL
# ---------------------------------------------------------------------------
func _unwrap_int_or_null result
    if result = NULL return NULL ok
    if isList(result)
        if result[1] = PROVEN_OK
            return result[2]
        ok
    ok
    return NULL
