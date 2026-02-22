/* SPDX-License-Identifier: PMPL-1.0-or-later */
/* Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk> */

/**
 * @file example.c
 * @brief Demonstration of the Proven C API
 *
 * This example shows typical usage patterns for libproven, including
 * initialisation, safe arithmetic, string escaping, path validation,
 * email validation, and proper cleanup.
 *
 * Build:
 *   gcc -o example example.c -I../include -L/path/to/lib -lproven
 *
 * Or with pkg-config:
 *   gcc -o example example.c $(pkg-config --cflags --libs proven)
 */

#include <proven.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

/**
 * @brief Demonstrate safe arithmetic operations that never crash.
 */
static void demo_safe_math(void)
{
    printf("=== SafeMath ===\n");

    /* Safe division: returns error instead of crashing on divide-by-zero */
    ProvenIntResult div_ok = proven_math_div(42, 7);
    if (PROVEN_SUCCEEDED(div_ok)) {
        printf("  42 / 7 = %ld\n", (long)div_ok.value);
    }

    ProvenIntResult div_zero = proven_math_div(10, 0);
    if (PROVEN_FAILED(div_zero)) {
        printf("  10 / 0 => error status %d (division by zero)\n", div_zero.status);
    }

    /* Overflow-checked addition */
    ProvenIntResult add_ok = proven_math_add_checked(1000, 2000);
    if (PROVEN_SUCCEEDED(add_ok)) {
        printf("  1000 + 2000 = %ld\n", (long)add_ok.value);
    }

    ProvenIntResult add_overflow = proven_math_add_checked(INT64_MAX, 1);
    if (add_overflow.status == PROVEN_ERR_OVERFLOW) {
        printf("  INT64_MAX + 1 => overflow detected\n");
    }

    /* Clamping */
    int64_t clamped = proven_math_clamp(0, 100, 150);
    printf("  clamp(0, 100, 150) = %ld\n", (long)clamped);

    printf("\n");
}

/**
 * @brief Demonstrate string escaping for XSS prevention.
 */
static void demo_safe_string(void)
{
    printf("=== SafeString ===\n");

    /* HTML escaping */
    const char* unsafe_html = "<script>alert('xss')</script>";
    ProvenStringResult html = proven_string_escape_html(
        (const uint8_t*)unsafe_html, strlen(unsafe_html)
    );
    if (PROVEN_SUCCEEDED(html)) {
        printf("  HTML escaped: %s\n", html.value);
        proven_free_string(html.value);
    }

    /* SQL escaping (prefer parameterized queries in production!) */
    const char* unsafe_sql = "'; DROP TABLE users; --";
    ProvenStringResult sql = proven_string_escape_sql(
        (const uint8_t*)unsafe_sql, strlen(unsafe_sql)
    );
    if (PROVEN_SUCCEEDED(sql)) {
        printf("  SQL escaped: %s\n", sql.value);
        proven_free_string(sql.value);
    }

    /* UTF-8 validation */
    const char* valid_utf8 = "Hello, World!";
    ProvenBoolResult utf8 = proven_string_is_valid_utf8(
        (const uint8_t*)valid_utf8, strlen(valid_utf8)
    );
    printf("  \"%s\" is valid UTF-8: %s\n", valid_utf8,
           utf8.value ? "yes" : "no");

    printf("\n");
}

/**
 * @brief Demonstrate path traversal detection.
 */
static void demo_safe_path(void)
{
    printf("=== SafePath ===\n");

    const char* safe_path = "uploads/photo.jpg";
    ProvenBoolResult r1 = proven_path_has_traversal(
        (const uint8_t*)safe_path, strlen(safe_path)
    );
    printf("  \"%s\" has traversal: %s\n", safe_path,
           r1.value ? "YES (blocked)" : "no (safe)");

    const char* attack_path = "../../../etc/passwd";
    ProvenBoolResult r2 = proven_path_has_traversal(
        (const uint8_t*)attack_path, strlen(attack_path)
    );
    printf("  \"%s\" has traversal: %s\n", attack_path,
           r2.value ? "YES (blocked)" : "no (safe)");

    printf("\n");
}

/**
 * @brief Demonstrate email validation.
 */
static void demo_safe_email(void)
{
    printf("=== SafeEmail ===\n");

    const char* emails[] = {
        "user@example.com",
        "invalid@",
        "test.user+tag@domain.co.uk"
    };

    for (size_t i = 0; i < sizeof(emails) / sizeof(emails[0]); i++) {
        ProvenBoolResult valid = proven_email_is_valid(
            (const uint8_t*)emails[i], strlen(emails[i])
        );
        printf("  \"%s\" valid: %s\n", emails[i],
               valid.value ? "yes" : "no");
    }

    printf("\n");
}

/**
 * @brief Demonstrate version information.
 */
static void demo_version(void)
{
    printf("=== Version ===\n");
    printf("  Library:    %u.%u.%u\n",
           proven_version_major(),
           proven_version_minor(),
           proven_version_patch());
    printf("  ABI:        %u\n", proven_ffi_abi_version());
    printf("  Modules:    %u\n", proven_module_count());
    printf("\n");
}

/**
 * @brief Demonstrate JSON validation.
 */
static void demo_safe_json(void)
{
    printf("=== SafeJson ===\n");

    const char* valid_json = "{\"key\": \"value\", \"n\": 42}";
    ProvenBoolResult is_valid = proven_json_is_valid(
        (const uint8_t*)valid_json, strlen(valid_json)
    );
    printf("  \"%s\" valid JSON: %s\n", valid_json,
           is_valid.value ? "yes" : "no");

    ProvenJsonType jtype = proven_json_get_type(
        (const uint8_t*)valid_json, strlen(valid_json)
    );
    printf("  Root type: %d (5 = object)\n", jtype);

    printf("\n");
}

/**
 * @brief Demonstrate safe floating-point operations.
 */
static void demo_safe_float(void)
{
    printf("=== SafeFloat ===\n");

    ProvenFloatResult sqrt_r = proven_float_sqrt(144.0);
    if (PROVEN_SUCCEEDED(sqrt_r)) {
        printf("  sqrt(144) = %.1f\n", sqrt_r.value);
    }

    ProvenFloatResult sqrt_neg = proven_float_sqrt(-1.0);
    if (PROVEN_FAILED(sqrt_neg)) {
        printf("  sqrt(-1) => error status %d\n", sqrt_neg.status);
    }

    ProvenFloatResult ln_r = proven_float_ln(2.718281828);
    if (PROVEN_SUCCEEDED(ln_r)) {
        printf("  ln(e) = %.6f\n", ln_r.value);
    }

    printf("\n");
}

int main(void)
{
    printf("Proven C Binding Example\n");
    printf("========================\n\n");

    /* Initialize the runtime (required before any other calls) */
    int32_t init_status = proven_init();
    if (init_status != PROVEN_OK) {
        fprintf(stderr, "Failed to initialize Proven runtime: %d\n", init_status);
        return 1;
    }

    if (!proven_is_initialized()) {
        fprintf(stderr, "Runtime reports not initialized after init()\n");
        return 1;
    }

    demo_version();
    demo_safe_math();
    demo_safe_string();
    demo_safe_path();
    demo_safe_email();
    demo_safe_json();
    demo_safe_float();

    /* Cleanup */
    proven_deinit();

    printf("All demonstrations completed successfully.\n");
    return 0;
}
