// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

/**
 * Status codes returned by Proven operations.
 * Maps to ProvenStatus enum in C ABI.
 */
public enum ProvenStatus {
    /** Operation completed successfully. */
    OK(0),
    /** Null pointer was passed where a valid pointer was expected. */
    ERR_NULL_POINTER(-1),
    /** Invalid argument provided to function. */
    ERR_INVALID_ARGUMENT(-2),
    /** Integer overflow occurred. */
    ERR_OVERFLOW(-3),
    /** Integer underflow occurred. */
    ERR_UNDERFLOW(-4),
    /** Division by zero attempted. */
    ERR_DIVISION_BY_ZERO(-5),
    /** Failed to parse input. */
    ERR_PARSE_FAILURE(-6),
    /** Validation check failed. */
    ERR_VALIDATION_FAILED(-7),
    /** Index or value out of bounds. */
    ERR_OUT_OF_BOUNDS(-8),
    /** Character encoding error. */
    ERR_ENCODING_ERROR(-9),
    /** Memory allocation failed. */
    ERR_ALLOCATION_FAILED(-10),
    /** Empty input where non-empty was expected. */
    ERR_EMPTY_INPUT(-11),
    /** Shape mismatch in tensor/matrix operation. */
    ERR_SHAPE_MISMATCH(-12),
    /** Currency mismatch in money operation. */
    ERR_CURRENCY_MISMATCH(-13),
    /** Invalid state transition. */
    ERR_INVALID_TRANSITION(-14),
    /** Rate limit exceeded. */
    ERR_RATE_LIMITED(-15),
    /** Circuit breaker is open. */
    ERR_CIRCUIT_OPEN(-16),
    /** Maximum retries exceeded. */
    ERR_MAX_RETRIES(-17),
    /** Buffer is full. */
    ERR_BUFFER_FULL(-18),
    /** Buffer is empty. */
    ERR_BUFFER_EMPTY(-19),
    /** Feature not implemented. */
    ERR_NOT_IMPLEMENTED(-99);

    private final int code;

    ProvenStatus(int code) {
        this.code = code;
    }

    /**
     * Get the numeric status code.
     */
    public int getCode() {
        return code;
    }

    /**
     * Check if this status indicates success.
     */
    public boolean isOk() {
        return this == OK;
    }

    /**
     * Check if this status indicates an error.
     */
    public boolean isError() {
        return this != OK;
    }

    /**
     * Get human-readable description of the status.
     */
    public String getDescription() {
        return switch (this) {
            case OK -> "Operation completed successfully";
            case ERR_NULL_POINTER -> "Null pointer provided";
            case ERR_INVALID_ARGUMENT -> "Invalid argument";
            case ERR_OVERFLOW -> "Integer overflow";
            case ERR_UNDERFLOW -> "Integer underflow";
            case ERR_DIVISION_BY_ZERO -> "Division by zero";
            case ERR_PARSE_FAILURE -> "Parse failure";
            case ERR_VALIDATION_FAILED -> "Validation failed";
            case ERR_OUT_OF_BOUNDS -> "Index out of bounds";
            case ERR_ENCODING_ERROR -> "Encoding error";
            case ERR_ALLOCATION_FAILED -> "Allocation failed";
            case ERR_EMPTY_INPUT -> "Empty input";
            case ERR_SHAPE_MISMATCH -> "Shape mismatch";
            case ERR_CURRENCY_MISMATCH -> "Currency mismatch";
            case ERR_INVALID_TRANSITION -> "Invalid state transition";
            case ERR_RATE_LIMITED -> "Rate limit exceeded";
            case ERR_CIRCUIT_OPEN -> "Circuit breaker is open";
            case ERR_MAX_RETRIES -> "Maximum retries exceeded";
            case ERR_BUFFER_FULL -> "Buffer is full";
            case ERR_BUFFER_EMPTY -> "Buffer is empty";
            case ERR_NOT_IMPLEMENTED -> "Not implemented";
        };
    }

    /**
     * Convert a numeric status code to a ProvenStatus.
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
