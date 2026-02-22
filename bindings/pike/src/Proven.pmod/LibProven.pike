// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// LibProven.pike - FFI bridge to libproven for Pike.
//
// Pike does not have direct dlopen/FFI support in the standard library,
// so this module shells out to the "proven-cli" command-line tool which
// wraps the same libproven C ABI. Each call invokes proven-cli with
// appropriate arguments and parses the JSON output.
//
// If a native C module is compiled (see README.md), it can be loaded
// instead for better performance.
//
// All computation is performed in the verified Idris 2 core; this file
// is exclusively a marshaling layer.
//
// Usage:
//   LibProven.init();
//   mapping result = LibProven.call("math_add_checked", ({"100", "200"}));
//   LibProven.deinit();

//! @class LibProven
//! Low-level FFI bridge to libproven.
//!
//! Provides the @[call()] method that invokes proven-cli and returns
//! parsed results. Higher-level Safe* classes use this internally.

//! Path to the proven-cli executable. Set this before calling init()
//! if proven-cli is not in your PATH.
string cli_path = "proven-cli";

//! Whether the runtime has been initialized.
protected int(0..1) _initialized = 0;

//! @decl void init()
//! Initialize the Proven runtime.
//!
//! Verifies that proven-cli is accessible and initializes the library.
//! Must be called before any other Proven function.
void init()
{
    mapping result = low_call("init", ({}));
    if (result && result->status == 0) {
        _initialized = 1;
    }
}

//! @decl void deinit()
//! Shut down the Proven runtime.
void deinit()
{
    low_call("deinit", ({}));
    _initialized = 0;
}

//! @decl int(0..1) is_initialized()
//! Check if the runtime is initialized.
int(0..1) is_initialized()
{
    return _initialized;
}

//! @decl mapping|int(0..0) call(string func_name, array(string) args)
//! Call a libproven function via proven-cli.
//!
//! @param func_name
//!   The function name (e.g., "math_add_checked", "email_is_valid").
//! @param args
//!   Array of string arguments to pass to the function.
//! @returns
//!   A mapping with "status" and "value" keys, or @expr{UNDEFINED@}
//!   on failure.
mapping|zero call(string func_name, array(string) args)
{
    return low_call(func_name, args);
}

//! @decl protected mapping|int(0..0) low_call(string func_name, array(string) args)
//! Internal: invoke proven-cli and parse the output.
//!
//! The proven-cli tool outputs JSON: {"status": 0, "value": ...}
//! On non-zero status, the "value" field may be absent.
protected mapping|zero low_call(string func_name, array(string) args)
{
    // Build the command line
    array(string) cmd = ({cli_path, func_name}) + args;

    // Execute the command
    mapping proc_result = Process.run(cmd);

    // Check for process-level failure
    if (proc_result->exitcode != 0) {
        return UNDEFINED;
    }

    // Parse JSON output
    string output = String.trim(proc_result->stdout || "");
    if (output == "") {
        return UNDEFINED;
    }

    mixed parsed;
    if (catch {
        parsed = Standards.JSON.decode(output);
    }) {
        return UNDEFINED;
    }

    if (!mappingp(parsed)) {
        return UNDEFINED;
    }

    return parsed;
}

//! @decl int|zero call_int(string func_name, array(string) args)
//! Call a libproven function that returns an integer result.
//! Returns UNDEFINED on error.
int|zero call_int(string func_name, array(string) args)
{
    mapping|zero result = call(func_name, args);
    if (!result || result->status != 0) {
        return UNDEFINED;
    }
    return (int)result->value;
}

//! @decl int(0..1)|zero call_bool(string func_name, array(string) args)
//! Call a libproven function that returns a boolean result.
//! Returns UNDEFINED on error.
int(0..1)|zero call_bool(string func_name, array(string) args)
{
    mapping|zero result = call(func_name, args);
    if (!result || result->status != 0) {
        return UNDEFINED;
    }
    return result->value ? 1 : 0;
}

//! @decl string|zero call_string(string func_name, array(string) args)
//! Call a libproven function that returns a string result.
//! Returns UNDEFINED on error.
string|zero call_string(string func_name, array(string) args)
{
    mapping|zero result = call(func_name, args);
    if (!result || result->status != 0) {
        return UNDEFINED;
    }
    return (string)result->value;
}

//! @decl float|zero call_float(string func_name, array(string) args)
//! Call a libproven function that returns a float result.
//! Returns UNDEFINED on error.
float|zero call_float(string func_name, array(string) args)
{
    mapping|zero result = call(func_name, args);
    if (!result || result->status != 0) {
        return UNDEFINED;
    }
    return (float)result->value;
}
