// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
package io.github.hyperpolymath.proven;

/**
 * Status codes returned by libproven C ABI functions.
 *
 * Maps 1:1 to the ProvenStatus enum in ffi/zig/src/main.zig.
 * No logic is implemented here; this is purely a data mapping.
 */
public enum ProvenStatus {
    OK(0, "Operation completed successfully"),
    ERR_NULL_POINTER(-1, "Null pointer provided"),
    ERR_INVALID_ARGUMENT(-2, "Invalid argument"),
    ERR_OVERFLOW(-3, "Integer overflow"),
    ERR_UNDERFLOW(-4, "Integer underflow"),
    ERR_DIVISION_BY_ZERO(-5, "Division by zero"),
    ERR_PARSE_FAILURE(-6, "Parse failure"),
    ERR_VALIDATION_FAILED(-7, "Validation failed"),
    ERR_OUT_OF_BOUNDS(-8, "Index out of bounds"),
    ERR_ENCODING_ERROR(-9, "Encoding error"),
    ERR_ALLOCATION_FAILED(-10, "Allocation failed"),
    ERR_NOT_IMPLEMENTED(-99, "Not implemented");

    private final int code;
    private final String description;

    ProvenStatus(int code, String description) {
        this.code = code;
        this.description = description;
    }

    /** Get the numeric status code matching the C ABI. */
    public int getCode() {
        return code;
    }

    /** Check if this status indicates success. */
    public boolean isOk() {
        return this == OK;
    }

    /** Check if this status indicates an error. */
    public boolean isError() {
        return this != OK;
    }

    /** Get human-readable description. */
    public String getDescription() {
        return description;
    }

    /**
     * Convert a numeric status code from the C ABI to a ProvenStatus.
     *
     * @param code the integer status code from a libproven call
     * @return the corresponding ProvenStatus, or ERR_NOT_IMPLEMENTED if unknown
     */
    public static ProvenStatus fromCode(int code) {
        for (ProvenStatus status : values()) {
            if (status.code == code) {
                return status;
            }
        }
        return ERR_NOT_IMPLEMENTED;
    }
}
