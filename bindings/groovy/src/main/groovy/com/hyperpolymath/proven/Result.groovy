// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath
package com.hyperpolymath.proven

import groovy.transform.CompileStatic
import groovy.transform.Immutable

/**
 * Result type for exception-free error handling.
 * Groovy-idiomatic with operator overloading and closure support.
 *
 * Usage:
 * {@code
 * def result = SafeMath.add(a, b)
 * result.map { it * 2 }
 *       .flatMap { SafeMath.add(it, 1) }
 *       .orElse { defaultValue }
 * }
 */
@CompileStatic
sealed interface Result<T, E> permits Ok, Err {

    boolean isOk()
    boolean isErr()
    T getValue()
    E getError()

    /**
     * Transform the success value.
     */
    default <U> Result<U, E> map(Closure<U> mapper) {
        isOk() ? ok(mapper.call(value)) : (Result<U, E>) this
    }

    /**
     * Transform with a function that returns Result.
     */
    default <U> Result<U, E> flatMap(Closure<Result<U, E>> mapper) {
        isOk() ? mapper.call(value) : (Result<U, E>) this
    }

    /**
     * Get value or compute default.
     */
    default T orElse(Closure<T> defaultSupplier) {
        isOk() ? value : defaultSupplier.call()
    }

    /**
     * Get value or default value.
     */
    default T orElse(T defaultValue) {
        isOk() ? value : defaultValue
    }

    /**
     * Execute closure if Ok.
     */
    default Result<T, E> onSuccess(Closure<?> action) {
        if (isOk()) action.call(value)
        this
    }

    /**
     * Execute closure if Err.
     */
    default Result<T, E> onError(Closure<?> action) {
        if (isErr()) action.call(error)
        this
    }

    /**
     * Match on Ok or Err with closures.
     */
    default <R> R match(Closure<R> onOk, Closure<R> onErr) {
        isOk() ? onOk.call(value) : onErr.call(error)
    }

    /**
     * Unwrap the value, throwing if Err.
     */
    default T unwrap() {
        if (isErr()) throw new IllegalStateException("Unwrap called on Err: ${error}")
        value
    }

    /**
     * Unwrap the error, throwing if Ok.
     */
    default E unwrapErr() {
        if (isOk()) throw new IllegalStateException("UnwrapErr called on Ok: ${value}")
        error
    }

    // Factory methods

    static <T, E> Result<T, E> ok(T value) {
        new Ok<T, E>(value)
    }

    static <T, E> Result<T, E> err(E error) {
        new Err<T, E>(error)
    }

    /**
     * Wrap a potentially throwing operation.
     */
    static <T> Result<T, String> trying(Closure<T> operation) {
        try {
            ok(operation.call())
        } catch (Exception e) {
            err(e.message ?: e.class.simpleName)
        }
    }

    /**
     * Combine multiple Results, returning first error or all values.
     */
    static <T, E> Result<List<T>, E> all(List<Result<T, E>> results) {
        def values = []
        for (r in results) {
            if (r.err) return (Result<List<T>, E>) r
            values << r.value
        }
        ok(values)
    }
}

@Immutable
@CompileStatic
final class Ok<T, E> implements Result<T, E> {
    T value

    @Override boolean isOk() { true }
    @Override boolean isErr() { false }
    @Override T getValue() { value }
    @Override E getError() { null }

    @Override
    String toString() { "Ok(${value})" }
}

@Immutable
@CompileStatic
final class Err<T, E> implements Result<T, E> {
    E error

    @Override boolean isOk() { false }
    @Override boolean isErr() { true }
    @Override T getValue() { null }
    @Override E getError() { error }

    @Override
    String toString() { "Err(${error})" }
}
