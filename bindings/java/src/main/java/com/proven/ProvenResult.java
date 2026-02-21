// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.proven;

import java.util.Optional;
import java.util.function.Function;

/**
 * A Result type representing either success (Ok) or failure (Err).
 * Enables exception-free error handling with ProvenStatus codes.
 *
 * @param <T> The success value type
 */
public sealed interface ProvenResult<T> permits ProvenResult.Ok, ProvenResult.Err {

    /**
     * Creates a successful result.
     */
    static <T> ProvenResult<T> ok(T value) {
        return new Ok<>(value);
    }

    /**
     * Creates a failure result with a status code.
     */
    static <T> ProvenResult<T> err(ProvenStatus status) {
        return new Err<>(status, status.getDescription());
    }

    /**
     * Creates a failure result with a status code and custom message.
     */
    static <T> ProvenResult<T> err(ProvenStatus status, String message) {
        return new Err<>(status, message);
    }

    /**
     * Creates a failure result with a string error (uses VALIDATION_FAILED status).
     */
    static <T> ProvenResult<T> err(String message) {
        return new Err<>(ProvenStatus.ERR_VALIDATION_FAILED, message);
    }

    /**
     * Returns true if this is a success.
     */
    boolean isOk();

    /**
     * Returns true if this is a failure.
     */
    boolean isErr();

    /**
     * Gets the status code.
     */
    ProvenStatus status();

    /**
     * Gets the success value, or throws if this is an error.
     */
    T unwrap();

    /**
     * Gets the success value, or returns the default.
     */
    T unwrapOr(T defaultValue);

    /**
     * Gets the error message as Optional.
     */
    Optional<String> errorMessage();

    /**
     * Maps the success value.
     */
    <U> ProvenResult<U> map(Function<T, U> mapper);

    /**
     * Chains another operation that may fail.
     */
    <U> ProvenResult<U> flatMap(Function<T, ProvenResult<U>> mapper);

    /**
     * Converts to Optional (empty if error).
     */
    Optional<T> toOptional();

    record Ok<T>(T value) implements ProvenResult<T> {
        @Override public boolean isOk() { return true; }
        @Override public boolean isErr() { return false; }
        @Override public ProvenStatus status() { return ProvenStatus.OK; }
        @Override public T unwrap() { return value; }
        @Override public T unwrapOr(T defaultValue) { return value; }
        @Override public Optional<String> errorMessage() { return Optional.empty(); }
        @Override public <U> ProvenResult<U> map(Function<T, U> mapper) {
            return new Ok<>(mapper.apply(value));
        }
        @Override public <U> ProvenResult<U> flatMap(Function<T, ProvenResult<U>> mapper) {
            return mapper.apply(value);
        }
        @Override public Optional<T> toOptional() { return Optional.of(value); }
    }

    record Err<T>(ProvenStatus status, String message) implements ProvenResult<T> {
        @Override public boolean isOk() { return false; }
        @Override public boolean isErr() { return true; }
        @Override public T unwrap() {
            throw new IllegalStateException("Called unwrap on Err: " + message);
        }
        @Override public T unwrapOr(T defaultValue) { return defaultValue; }
        @Override public Optional<String> errorMessage() { return Optional.of(message); }
        @Override public <U> ProvenResult<U> map(Function<T, U> mapper) {
            return new Err<>(status, message);
        }
        @Override public <U> ProvenResult<U> flatMap(Function<T, ProvenResult<U>> mapper) {
            return new Err<>(status, message);
        }
        @Override public Optional<T> toOptional() { return Optional.empty(); }
    }
}
