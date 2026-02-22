// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

/// SafeCurrency -- Resource-typed wrappers around libproven currency FFI.
///
/// Leverages Eclexia's economics-as-code paradigm and resource types to
/// provide safe currency operations. All computation is delegated to the
/// formally verified Idris 2 implementation via the Zig FFI layer.
///
/// Currency values are tracked as resources with dimensional types,
/// ensuring at compile time that operations respect economic invariants.

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/// A safe currency amount stored in minor units (e.g. cents).
/// Uses Eclexia's resource tracking to prevent double-spending.
type SafeAmount = {
    minor_units: Int,
    currency_code: String,
    decimal_places: Int
}

/// Currency operation error.
type CurrencyError = {
    code: Int,
    message: String
}

// ---------------------------------------------------------------------------
// Resource budget declarations (Eclexia economics-as-code)
// ---------------------------------------------------------------------------

/// Currency parsing has bounded computational cost.
def parse_currency(input: String) -> Result[SafeAmount, CurrencyError]
    @requires: energy < 10J
{
    let (ptr, len) = string_to_raw(input)
    let result = proven_currency_parse(ptr, len)
    if result.status == STATUS_OK {
        // Unpack the FFI result into SafeAmount fields
        Ok(SafeAmount {
            minor_units: result.value,
            currency_code: extract_currency_code(result),
            decimal_places: extract_decimal_places(result)
        })
    } else {
        Err(CurrencyError {
            code: result.status,
            message: status_to_error(result.status).message
        })
    }
}

/// Format a currency amount to a display string.
def format_currency(amount: SafeAmount) -> Result[String, CurrencyError]
    @requires: energy < 5J
{
    let (code_ptr, _) = string_to_raw(amount.currency_code)
    let result = proven_currency_format(
        amount.minor_units,
        code_ptr,
        amount.decimal_places
    )
    if result.status == STATUS_OK {
        let s = string_from_raw(result.ptr, result.len)
        proven_free_string(result.ptr)
        Ok(s)
    } else {
        Err(CurrencyError {
            code: result.status,
            message: status_to_error(result.status).message
        })
    }
}

// ---------------------------------------------------------------------------
// Safe arithmetic on currency amounts
// ---------------------------------------------------------------------------

/// Safe addition of two currency amounts.
/// Ensures both amounts are in the same currency before adding.
/// Uses proven_math_add_checked for overflow-safe arithmetic.
fn add_amounts(a: SafeAmount, b: SafeAmount) -> Result[SafeAmount, CurrencyError] {
    if a.currency_code != b.currency_code {
        Err(CurrencyError {
            code: -2,
            message: "cannot add amounts in different currencies: "
                + a.currency_code + " vs " + b.currency_code
        })
    } else {
        let result = proven_math_add_checked(a.minor_units, b.minor_units)
        if result.status == STATUS_OK {
            Ok(SafeAmount {
                minor_units: result.value,
                currency_code: a.currency_code,
                decimal_places: a.decimal_places
            })
        } else {
            Err(CurrencyError {
                code: result.status,
                message: status_to_error(result.status).message
            })
        }
    }
}

/// Safe subtraction of two currency amounts.
/// Ensures both amounts are in the same currency before subtracting.
/// Uses proven_math_sub_checked for underflow-safe arithmetic.
fn sub_amounts(a: SafeAmount, b: SafeAmount) -> Result[SafeAmount, CurrencyError] {
    if a.currency_code != b.currency_code {
        Err(CurrencyError {
            code: -2,
            message: "cannot subtract amounts in different currencies: "
                + a.currency_code + " vs " + b.currency_code
        })
    } else {
        let result = proven_math_sub_checked(a.minor_units, b.minor_units)
        if result.status == STATUS_OK {
            Ok(SafeAmount {
                minor_units: result.value,
                currency_code: a.currency_code,
                decimal_places: a.decimal_places
            })
        } else {
            Err(CurrencyError {
                code: result.status,
                message: status_to_error(result.status).message
            })
        }
    }
}

/// Multiply a currency amount by a scalar (e.g. quantity).
/// Uses proven_math_mul_checked for overflow-safe arithmetic.
fn mul_amount(amount: SafeAmount, scalar: Int) -> Result[SafeAmount, CurrencyError] {
    let result = proven_math_mul_checked(amount.minor_units, scalar)
    if result.status == STATUS_OK {
        Ok(SafeAmount {
            minor_units: result.value,
            currency_code: amount.currency_code,
            decimal_places: amount.decimal_places
        })
    } else {
        Err(CurrencyError {
            code: result.status,
            message: status_to_error(result.status).message
        })
    }
}

/// Divide a currency amount by a scalar.
/// Uses proven_math_div for division-by-zero checking.
fn div_amount(amount: SafeAmount, divisor: Int) -> Result[SafeAmount, CurrencyError] {
    let result = proven_math_div(amount.minor_units, divisor)
    if result.status == STATUS_OK {
        Ok(SafeAmount {
            minor_units: result.value,
            currency_code: amount.currency_code,
            decimal_places: amount.decimal_places
        })
    } else {
        Err(CurrencyError {
            code: result.status,
            message: status_to_error(result.status).message
        })
    }
}

/// Check if an amount is zero.
fn is_zero(amount: SafeAmount) -> Bool {
    amount.minor_units == 0
}

/// Check if an amount is positive (greater than zero).
fn is_positive(amount: SafeAmount) -> Bool {
    amount.minor_units > 0
}

/// Check if an amount is negative.
fn is_negative(amount: SafeAmount) -> Bool {
    amount.minor_units < 0
}

/// Get the absolute value of a currency amount.
/// Uses proven_math_abs_safe to handle MIN_INT correctly.
fn abs_amount(amount: SafeAmount) -> Result[SafeAmount, CurrencyError] {
    let result = proven_math_abs_safe(amount.minor_units)
    if result.status == STATUS_OK {
        Ok(SafeAmount {
            minor_units: result.value,
            currency_code: amount.currency_code,
            decimal_places: amount.decimal_places
        })
    } else {
        Err(CurrencyError {
            code: result.status,
            message: status_to_error(result.status).message
        })
    }
}
