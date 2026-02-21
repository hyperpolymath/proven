// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.hyperpolymath.proven;

import java.util.Optional;
import java.util.function.Function;

/**
 * A Result type representing either success (Ok) or failure (Err).
 * Enables exception-free error handling.
 *
 * @param <T> The success value type
 * @param <E> The error value type
 */
public sealed interface Result<T, E> permits Result.Ok, Result.Err {

    /**
     * Creates a successful result.
     */
    static <T, E> Result<T, E> ok(T value) {
        return new Ok<>(value);
    }

    /**
     * Creates a failure result.
     */
    static <T, E> Result<T, E> err(E error) {
        return new Err<>(error);
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
     * Gets the success value, or throws if this is an error.
     */
    T unwrap();

    /**
     * Gets the success value, or returns the default.
     */
    T unwrapOr(T defaultValue);

    /**
     * Gets the error value as Optional.
     */
    Optional<E> error();

    /**
     * Maps the success value.
     */
    <U> Result<U, E> map(Function<T, U> mapper);

    /**
     * Maps the error value.
     */
    <F> Result<T, F> mapErr(Function<E, F> mapper);

    /**
     * Chains another operation that may fail.
     */
    <U> Result<U, E> flatMap(Function<T, Result<U, E>> mapper);

    record Ok<T, E>(T value) implements Result<T, E> {
        @Override public boolean isOk() { return true; }
        @Override public boolean isErr() { return false; }
        @Override public T unwrap() { return value; }
        @Override public T unwrapOr(T defaultValue) { return value; }
        @Override public Optional<E> error() { return Optional.empty(); }
        @Override public <U> Result<U, E> map(Function<T, U> mapper) {
            return new Ok<>(mapper.apply(value));
        }
        @Override public <F> Result<T, F> mapErr(Function<E, F> mapper) {
            return new Ok<>(value);
        }
        @Override public <U> Result<U, E> flatMap(Function<T, Result<U, E>> mapper) {
            return mapper.apply(value);
        }
    }

    record Err<T, E>(E error) implements Result<T, E> {
        @Override public boolean isOk() { return false; }
        @Override public boolean isErr() { return true; }
        @Override public T unwrap() { throw new IllegalStateException("Called unwrap on Err: " + error); }
        @Override public T unwrapOr(T defaultValue) { return defaultValue; }
        @Override public Optional<E> error() { return Optional.of(error); }
        @Override public <U> Result<U, E> map(Function<T, U> mapper) {
            return new Err<>(error);
        }
        @Override public <F> Result<T, F> mapErr(Function<E, F> mapper) {
            return new Err<>(mapper.apply(error));
        }
        @Override public <U> Result<U, E> flatMap(Function<T, Result<U, E>> mapper) {
            return new Err<>(error);
        }
    }
}
